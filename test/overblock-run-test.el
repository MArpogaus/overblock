;;; overblock-run-test.el --- Tests for overblock-run -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Marcel Arpogaus

;; Author: Marcel Arpogaus <znepry.necbtnhf@tznvy.pbz>
;; Assisted-by: Claude:claude-opus-5
;; URL: https://github.com/MArpogaus/overblock

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Run with: make test
;;
;; The runner with a process at the other end, and no language: the
;; backend here is a plist over `cat', which is enough for every way a
;; run ends.  The notebook suites cover the same file from above, and
;; cannot reach these paths — they have no process, so an interpreter
;; cannot die in them, and the death of one is what this file is for.

;;; Code:

(require 'ert)
(require 'overblock-run)

;;;; A backend over `cat'

(defvar overblock-run-test-buttons
  '((stop ("" "□" "stop") "Stop" overblock-run-interrupt running)
    (discard ("" "✕" "drop") "Discard" overblock-run-discard-output t))
  "Two buttons, which is enough to tell a running header from a done one.")

(defvar overblock-run-test-max-lines 12 "Lines a test result shows.")
(defvar overblock-run-test-max-chars 0 "Columns a test result line shows.")

(defvar overblock-run-test--shell nil
  "The shell buffer of the run in hand, for the backend's `:process'.")

(defun overblock-run-test--backend ()
  "Return the backend plist of the fixture, over `cat'."
  (list :name "runtest" :unit "cell"
        :process (lambda () (get-buffer-process overblock-run-test--shell))
        ;; `cat' echoes what it is sent, so a send that asks for a
        ;; prompt gets one back: the filter is called by hand here.
        :send (lambda (_proc _beg _end) nil)
        :prompt-p (lambda (tail) (string-suffix-p ">>> " tail))
        :clean (lambda (text) (string-trim (string-remove-suffix ">>> " text)))
        :error-p (lambda (text) (string-match-p "Error" text))
        :buttons 'overblock-run-test-buttons
        :lines 'overblock-run-test-max-lines
        :chars 'overblock-run-test-max-chars))

(defmacro overblock-run-test--with-run (&rest body)
  "Run BODY with a region of the notebook running in a shell over `cat'.
`notebook' and `shell' are bound to the two buffers, and the shell is
current: that is where the filter, the ticker and the end of a run do
their work.  Both buffers go afterwards, and so does the ticker's
timer where the run in BODY never ended."
  (declare (indent 0))
  `(let* ((notebook (generate-new-buffer " *overblock-run-test-notebook*"))
          (shell (generate-new-buffer " *overblock-run-test-shell*"))
          (overblock-run-test--shell shell)
          (proc (make-process :name "overblock-run-test" :command '("cat")
                              :buffer shell :noquery t)))
     (unwind-protect
         (with-current-buffer notebook
           (insert "one\ntwo\nthree\n")
           (setq-local overblock-run-backend (overblock-run-test--backend))
           (with-current-buffer shell
             (setq-local overblock-run-backend
                         (buffer-local-value 'overblock-run-backend notebook)))
           ;; the first line is the region that runs, so the result
           ;; hangs on the newline under it and two lines stand below
           (goto-char (point-min))
           (overblock-run-send proc (point-min) (pos-eol))
           (with-current-buffer shell ,@body))
       (when-let* ((timer (plist-get (buffer-local-value 'overblock-run--state
                                                         shell)
                                     :timer)))
         (cancel-timer timer))
       (when (process-live-p proc) (delete-process proc))
       (kill-buffer notebook)
       (kill-buffer shell))))

(defun overblock-run-test--result (buffer)
  "Return the result block of BUFFER, or nil for none."
  (with-current-buffer buffer
    (car (overblock-in (point-min) (point-max) 'result))))

(defun overblock-run-test--shown (buffer)
  "Return the header and the body a result of BUFFER shows.
The header is a string on the anchor and the body the display of the
newline the block hangs on, which is where `overblock-run-update' puts
them."
  (when-let* ((block (overblock-run-test--result buffer)))
    (substring-no-properties
     (concat (overlay-get block 'after-string)
             (when-let* ((nl (overblock-get block :newline))
                         ((overlayp nl)))
               (overlay-get nl 'display))))))

;;;; A run that ends at a prompt

(ert-deftest overblock-run-test-a-prompt-ends-the-run ()
  "The filter sees the closing prompt and the result is what was printed.
Nothing is left running, and the markers of the run are let go: they
live in the shell, and a pass over 200 regions left hundreds there."
  (overblock-run-test--with-run
    (let ((from (plist-get overblock-run--state :from)))
      (goto-char (point-max))
      (insert "hello\n>>> ")
      (overblock-run--filter "hello\n>>> ")
      (should-not overblock-run--state)
      (should (string-search "hello" (overblock-run-test--shown notebook)))
      ;; the header says the run is over: a tick and no spinner
      (should-not (string-search "⠋" (overblock-run-test--shown notebook)))
      (should-not (marker-position from)))))

(ert-deftest overblock-run-test-an-error-stops-the-pass ()
  "A result the backend calls an error empties the queue.
The rest of a run-all is dropped where one region failed, which is
what a reader expects of a pass that reached a traceback."
  (overblock-run-test--with-run
    (setq overblock-run--queue (list (copy-marker 1)))
    (goto-char (point-max))
    (insert "Error: no\n>>> ")
    (overblock-run--filter "Error: no\n>>> ")
    (should-not overblock-run--queue)))

;;;; The three ways a run dies

(ert-deftest overblock-run-test-the-ticker-finds-a-dead-interpreter ()
  "The interpreter goes away under a running region and the ticker says so.
Nothing else notices: the prompt the filter waits for will never come,
and the block would have kept its spinner for the rest of the session."
  (overblock-run-test--with-run
    (setq overblock-run--queue (list (copy-marker 1)))
    (delete-process proc)
    (overblock-run--tick shell (plist-get overblock-run--state :timer))
    (should-not overblock-run--state)
    (let ((shown (overblock-run-test--shown notebook)))
      (should (string-match-p "died\\|Process" shown)))
    ;; a death drops the pass as well
    (should-not overblock-run--queue)))

(ert-deftest overblock-run-test-a-killed-shell-ends-the-run ()
  "Killing the shell under a running region ends it as a death.
`kill-buffer-hook' in the shell is where that is caught, and the
notebook is another buffer: its block has to be finished from here."
  (overblock-run-test--with-run
    (kill-buffer shell)
    (should (string-match-p "died\\|Process"
                            (overblock-run-test--shown notebook)))))

(ert-deftest overblock-run-test-a-restarted-shell-ends-the-run ()
  "A restart reinitializes the major mode of the shell, which ends the run.
`change-major-mode-hook' is the third way in, and a restart is not an
unexpected death: the reason a caller gives is what the block shows."
  (overblock-run-test--with-run
    (overblock-run-abort "runtest: restarted")
    (should-not overblock-run--state)
    (should (string-search "restarted" (overblock-run-test--shown notebook)))))

(ert-deftest overblock-run-test-the-major-mode-hook-is-armed ()
  "A send arms the two hooks that catch the shell going away.
They are what `overblock-run-abort' hangs on, and nothing else calls
it: without them a dead shell left its region running for good."
  (overblock-run-test--with-run
    (should (memq #'overblock-run-abort kill-buffer-hook))
    (should (memq #'overblock-run-abort change-major-mode-hook))
    ;; and the mode change itself ends the run
    (fundamental-mode)
    (should-not overblock-run--state)))

(ert-deftest overblock-run-test-a-second-region-waits-for-the-first ()
  "A shell that is busy refuses the next region rather than losing it.
Both results would otherwise hang on the second region's markers."
  (overblock-run-test--with-run
    (with-current-buffer notebook
      (should-error (overblock-run-send proc (point-min) (point-max))
                    :type 'user-error))))

;;;; What a restart does

(ert-deftest overblock-run-test-a-restart-ends-what-runs ()
  "A restart ends the running region, drops the queue and the results.
The two notebook suites stub the abort half and the queue half away,
so this is the only place either of them runs."
  (overblock-run-test--with-run
    (setq overblock-run--queue (list (copy-marker 1)))
    (let (restarted)
      (with-current-buffer notebook
        (overblock-run-restart "runtest: restarting"
                               (lambda (proc) (setq restarted (or proc t)))))
      (should restarted)
      (should-not overblock-run--state)
      (should-not overblock-run--queue)
      ;; the result of the region that was running goes with them
      (should-not (overblock-run-test--result notebook)))))

(ert-deftest overblock-run-test-the-running-region-is-public ()
  "`overblock-run-running-region' answers the markers of what runs.
A caller that moves text asks it, and it answered from the shell of
this buffer: the notebook and the shell are two buffers and the
markers live in the second."
  (overblock-run-test--with-run
    (with-current-buffer notebook
      (pcase-let ((`(,beg . ,end) (overblock-run-running-region)))
        (should (eq (marker-buffer beg) notebook))
        (should (= beg (point-min)))
        (should (= end (save-excursion (goto-char (point-min)) (pos-eol))))))
    ;; and nothing once the run is over
    (goto-char (point-max))
    (insert ">>> ")
    (overblock-run--filter ">>> ")
    (with-current-buffer notebook
      (should-not (overblock-run-running-region)))))

;;;; The mark at the head of a result bar

(ert-deftest overblock-run-test-the-mark-says-which-state-it-is-in ()
  "Four states, four marks: a spinner, a warning, a fold arrow, a tick.
The mark is the one glyph a reader reads at a glance, and three of the
four were drawn by no test at all."
  (let ((overblock-run-backend (overblock-run-test--backend)))
    ;; running: a frame of the spinner, and the next tick is another
    (let ((one (overblock-run--mark nil 0 0.0 'running))
          (two (overblock-run--mark nil 0 overblock-run-tick 'running)))
      (should-not (equal one two))
      ;; the four states are four marks, whatever glyphs this display
      ;; can draw: a reader tells them apart at a glance
      (let ((died (overblock-run--mark nil 0 1.0 'died))
            (empty (overblock-run--mark nil 0 1.0 nil))
            (fold (overblock-run--mark nil 3 1.0 nil)))
        (should (= 4 (length (seq-uniq (list one died empty fold)
                                       #'equal))))
        ;; the fold arrow turns with the fold and is a button; the
        ;; other three are not pressed
        (should-not (equal fold (overblock-run--mark t 3 1.0 nil)))
        (should (get-text-property 1 'keymap fold))
        (should-not (get-text-property 1 'keymap died))))))

(provide 'overblock-run-test)
;;; overblock-run-test.el ends here
