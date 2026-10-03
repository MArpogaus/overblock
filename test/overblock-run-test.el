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
;; run ends.  The notebook suites have no process, so an interpreter
;; cannot die in them; this file tests that.

;;; Code:

(require 'ert)
(require 'overblock-run)

;;;; A backend over `cat'

(defvar overblock-run-test-buttons
  '((stop ("" "□" "stop") "Stop" overblock-run-interrupt running)
    (discard ("" "✕" "drop") "Discard" overblock-run-discard-output t))
  "Two buttons, which is enough to tell a running header from a done one.")

(defvar overblock-run-test--shell nil
  "The shell buffer of the run in hand, for the backend's `:process'.")

(defun overblock-run-test--backend ()
  "Return the backend plist of the fixture, over `cat'."
  (list :name "runtest" :unit "cell"
        :process (lambda () (get-buffer-process overblock-run-test--shell))
        ;; The tests call the filter by hand.
        :send (lambda (_proc _beg _end) nil)
        :prompt-p (lambda (tail) (string-suffix-p ">>> " tail))
        :clean (lambda (text) (string-trim (string-remove-suffix ">>> " text)))
        :error-p (lambda (text) (string-match-p "Error" text))
        :buttons 'overblock-run-test-buttons))

(defmacro overblock-run-test--with-run (&rest body)
  "Run BODY with a region of the notebook running in a shell over `cat'.
`notebook' and `shell' are bound to the two buffers, and the shell is
current, as for the filter, the ticker and the end of a run.  Both
buffers go afterwards, and so does the timer of the ticker where the
run in BODY did not end."
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
           ;; The first line is the region that runs.
           (goto-char (point-min))
           (overblock-run--send proc (point-min) (pos-eol))
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
Nothing is left running, and the markers of the run, which are in the
shell, are freed."
  (overblock-run-test--with-run
    (let ((from (plist-get overblock-run--state :from)))
      (goto-char (point-max))
      (insert "hello\n>>> ")
      (overblock-run--filter "hello\n>>> ")
      (should-not overblock-run--state)
      (should (string-search "hello" (overblock-run-test--shown notebook)))
      ;; No spinner: the run is over.
      (should-not (string-search "⠋" (overblock-run-test--shown notebook)))
      (should-not (marker-position from)))))

(ert-deftest overblock-run-test-an-error-stops-the-pass ()
  "A result the backend calls an error empties the queue.
The rest of a pass is dropped where one region failed."
  (overblock-run-test--with-run
    (setq overblock-run--queue (list (copy-marker 1)))
    (goto-char (point-max))
    (insert "Error: no\n>>> ")
    (overblock-run--filter "Error: no\n>>> ")
    (should-not overblock-run--queue)))

(ert-deftest overblock-run-test-a-backend-with-no-arm-never-waits ()
  "Arming a shell whose backend cannot arm leaves it free."
  (overblock-run-test--with-run
    (cancel-timer (plist-get overblock-run--state :timer))
    (setq overblock-run--state nil)
    (with-current-buffer notebook
      (overblock-run--arm)
      (should-not (overblock-run--busy-p)))))

;;;; The three ways a run dies

(ert-deftest overblock-run-test-the-ticker-finds-a-dead-interpreter ()
  "The interpreter goes away under a running region and the ticker says so.
Nothing else notices: the prompt the filter waits for never comes."
  (overblock-run-test--with-run
    (setq overblock-run--queue (list (copy-marker 1)))
    ;; Set from the shell: the home starts the scrolling in its notebook.
    (overblock-run--home-set (with-current-buffer notebook (point-marker)))
    (should (memq notebook overblock-run--scrolled))
    (should-not (memq shell overblock-run--scrolled))
    (delete-process proc)
    (overblock-run--tick shell (plist-get overblock-run--state :timer))
    (should-not overblock-run--state)
    (let ((shown (overblock-run-test--shown notebook)))
      (should (string-match-p "died\\|Process" shown)))
    ;; A death drops the pass too, its home and its scrolling.
    (should-not overblock-run--queue)
    (should-not overblock-run--home)
    (should-not (memq notebook overblock-run--scrolled))))

(ert-deftest overblock-run-test-a-killed-shell-ends-the-run ()
  "Killing the shell under a running region ends it as a death.
`kill-buffer-hook' in the shell catches it, and finishes the block in
the notebook, which is another buffer."
  (overblock-run-test--with-run
    (kill-buffer shell)
    (should (string-match-p "died\\|Process"
                            (overblock-run-test--shown notebook)))))

(ert-deftest overblock-run-test-a-restarted-shell-ends-the-run ()
  "A restart reinitializes the major mode of the shell, which ends the run.
`change-major-mode-hook' catches it.  A restart is no unexpected
death: the block shows the reason of the caller."
  (overblock-run-test--with-run
    (overblock-run-abort "runtest: restarted")
    (should-not overblock-run--state)
    (should (string-search "restarted" (overblock-run-test--shown notebook)))))

(ert-deftest overblock-run-test-the-major-mode-hook-is-armed ()
  "A send arms the two hooks that catch the shell going away.
Only they call `overblock-run-abort' for a shell that goes away."
  (overblock-run-test--with-run
    (should (memq #'overblock-run-abort kill-buffer-hook))
    (should (memq #'overblock-run-abort change-major-mode-hook))
    ;; The mode change ends the run.
    (fundamental-mode)
    (should-not overblock-run--state)))

(ert-deftest overblock-run-test-a-second-region-waits-for-the-first ()
  "A shell that is busy refuses the next region rather than losing it.
Else both results would hang on the markers of the second region."
  (overblock-run-test--with-run
    (with-current-buffer notebook
      (should-error (overblock-run--send proc (point-min) (point-max))
                    :type 'user-error))))

(ert-deftest overblock-run-test-a-region-sent-while-busy-runs-as-sent ()
  "A region queued behind a running one is sent as it was, not as its cell.
The `:step' of the backend, which runs the whole cell, is not called
for it."
  (overblock-run-test--with-run
    (let (stepped from to)
      (with-current-buffer notebook
        (setq overblock-run-backend
              (plist-put overblock-run-backend :step
                         (lambda () (setq stepped t))))
        (goto-char (point-min))
        (forward-line 2)
        (setq from (point) to (pos-eol))
        (overblock-run-region from to))
      (goto-char (point-max))
      (insert "one\n>>> ")
      (overblock-run--filter "one\n>>> ")
      (should-not stepped)
      (should (equal (list (marker-position (plist-get overblock-run--state :beg))
                           (marker-position (plist-get overblock-run--state :end)))
                     (list from to))))))

;;;; What a restart does

(ert-deftest overblock-run-test-a-restart-ends-what-runs ()
  "A restart ends the running region, drops the queue and the results.
The two notebook suites stub the abort and the queue away, so only
this test runs them."
  (overblock-run-test--with-run
    (setq overblock-run--queue (list (copy-marker 1)))
    (let (restarted)
      (with-current-buffer notebook
        (overblock-run-restart "runtest: restarting"
                               (lambda (proc) (setq restarted (or proc t)))))
      (should restarted)
      (should-not overblock-run--state)
      (should-not overblock-run--queue)
      ;; The result of the running region goes too.
      (should-not (overblock-run-test--result notebook)))))

(ert-deftest overblock-run-test-the-running-region-is-public ()
  "`overblock-run-running-region' returns the markers of what runs.
Called in the notebook, it reads the state in the shell, and the
markers are in the notebook."
  (overblock-run-test--with-run
    (with-current-buffer notebook
      (pcase-let ((`(,beg . ,end) (overblock-run-running-region)))
        (should (eq (marker-buffer beg) notebook))
        (should (= beg (point-min)))
        (should (= end (save-excursion (goto-char (point-min)) (pos-eol))))))
    ;; Nothing once the run is over.
    (goto-char (point-max))
    (insert ">>> ")
    (overblock-run--filter ">>> ")
    (with-current-buffer notebook
      (should-not (overblock-run-running-region)))))

(ert-deftest overblock-run-test-a-follower-gets-the-output-as-it-comes ()
  "A buffer that follows the run is written what the region prints.
This is what a popped out result does while its region runs.  The
marker of what is copied is in the shell, and the follower also gets
what was printed before it asked."
  (overblock-run-test--with-run
    (goto-char (point-max))
    (insert "first\n")
    (let ((out (generate-new-buffer " *overblock-run-test-follow*")))
      (unwind-protect
          (progn
            (with-current-buffer notebook (overblock-run-follow out))
            ;; The output so far.
            (should (equal (with-current-buffer out (buffer-string)) "first\n"))
            ;; Then only what is new.
            (goto-char (point-max))
            (insert "second\n")
            (overblock-run--follow-tick)
            (should (equal (with-current-buffer out (buffer-string))
                           "first\nsecond\n")))
        (kill-buffer out)))))

(ert-deftest overblock-run-test-a-running-result-offers-no-discard ()
  "The discard button waits for the region to end.
While the region runs, the next tick would draw the result again."
  (let ((buttons (overblock-run-result-buttons "cell" "image")))
    (should-not (string-search "drop" (overblock-buttons buttons nil 1 t)))
    (should (string-search "drop" (overblock-buttons buttons nil 1 nil)))))

(ert-deftest overblock-run-test-both-notebooks-draw-the-same-five ()
  "The five buttons of a result header are one list, drawn for both.
So a .py file and an Rmd file show the same row."
  (let ((buttons (overblock-run-result-buttons "cell" "image")))
    (should (= (length buttons) 5))
    (should (equal (mapcar #'car buttons)
                   '(stop save-image copy pop discard)))
    ;; The unit and the word for a picture reach the tooltips.
    (should (string-search "cell" (nth 2 (assq 'stop buttons))))
    (should (string-search "image" (nth 2 (assq 'save-image buttons))))
    ;; A chunk, with figures.
    (let ((chunk (overblock-run-result-buttons "chunk" "figure")))
      (should (string-search "chunk" (nth 2 (assq 'stop chunk))))
      (should (string-search "figure" (nth 2 (assq 'save-image chunk)))))))

;;;; The mark at the head of a result bar

(ert-deftest overblock-run-test-the-end-of-a-pass-says-so ()
  "The last region of a pass leaves a message that the pass is over."
  (overblock-run-test--with-run
    (overblock-run--home-set (with-current-buffer notebook (point-marker)))
    (let (said)
      (cl-letf (((symbol-function 'message)
                 (lambda (format-string &rest args)
                   (setq said (and format-string
                                   (apply #'format format-string args))))))
        (goto-char (point-max))
        (insert "ok\n>>> ")
        (overblock-run--filter "ok\n>>> "))
      (should (equal said "runtest: done")))))

(ert-deftest overblock-run-test-a-pass-that-ends-in-an-error-says-so ()
  "A pass whose last region fails says it stopped, not that it is done."
  (overblock-run-test--with-run
    (overblock-run--home-set (with-current-buffer notebook (point-marker)))
    (let (said)
      (cl-letf (((symbol-function 'message)
                 (lambda (format-string &rest args)
                   (setq said (and format-string
                                   (apply #'format format-string args))))))
        (goto-char (point-max))
        (insert "Error\n>>> ")
        (overblock-run--filter "Error\n>>> "))
      (should (equal said "runtest: stopped at error")))))

(ert-deftest overblock-run-test-a-pass-ending-on-a-cell-without-output-says-done ()
  "A pass whose last region the notebook answers itself still says done.
A markdown cell is rendered, not sent, so no run ends after it."
  (overblock-run-test--with-run
    (with-current-buffer notebook
      (setq overblock-run-backend
            (plist-put overblock-run-backend :step #'ignore)))
    (overblock-run--home-set (with-current-buffer notebook (point-marker)))
    (overblock-run--queue-set
     (list (with-current-buffer notebook (copy-marker (point-max)))))
    (let (said)
      (cl-letf (((symbol-function 'message)
                 (lambda (format-string &rest args)
                   (setq said (and format-string
                                   (apply #'format format-string args))))))
        (goto-char (point-max))
        (insert "ok\n>>> ")
        (overblock-run--filter "ok\n>>> "))
      (should (equal said "runtest: done")))))

(ert-deftest overblock-run-test-the-header-says-what-the-result-is ()
  "A failed result says so, and a folded one claims to show nothing.
A traceback looked like any other output, and a folded result of thirty
lines read showing 12."
  (let ((overblock-run-backend (overblock-run-test--backend)))
    (should (string-search
             "error" (substring-no-properties
                      (overblock-run-header nil 3 3 0.1 'failed nil))))
    (should-not (string-search
                 "error" (substring-no-properties
                          (overblock-run-header nil 3 3 0.1 nil nil))))
    (should (string-search
             "showing 12" (substring-no-properties
                           (overblock-run-header nil 30 12 0.1 nil nil))))
    (should-not (string-search
                 "showing" (substring-no-properties
                            (overblock-run-header t 30 12 0.1 nil nil))))))

(ert-deftest overblock-run-test-a-result-the-backend-calls-an-error-fails ()
  "The end of a run marks a result `failed' where `:error-p' says so."
  (overblock-run-test--with-run
    (goto-char (point-max))
    (insert "ZeroDivisionError\n>>> ")
    (overblock-run--filter "ZeroDivisionError\n>>> ")
    (should (eq (plist-get (overblock-get (overblock-run-test--result notebook)
                                          :data)
                           :state)
                'failed))))

(ert-deftest overblock-run-test-the-mark-says-which-state-it-is-in ()
  "Four states, four marks: a spinner, a warning, a fold arrow, a tick.
The four marks differ on every display."
  (let ((overblock-run-backend (overblock-run-test--backend)))
    ;; Running: the next tick shows another spinner frame.
    (let ((one (overblock-run--mark nil 0 0.0 'running))
          (two (overblock-run--mark nil 0 overblock-run--interval 'running)))
      (should-not (equal one two))
      ;; Four distinct marks, whatever glyphs this display draws.
      (let ((died (overblock-run--mark nil 0 1.0 'died))
            (empty (overblock-run--mark nil 0 1.0 nil))
            (fold (overblock-run--mark nil 3 1.0 nil)))
        (should (= 4 (length (seq-uniq (list one died empty fold)
                                       #'equal))))
        ;; The fold arrow turns with the fold and is a button; the
        ;; others are not buttons.
        (should-not (equal fold (overblock-run--mark t 3 1.0 nil)))
        ;; The mark is the first character of the bar.
        (should (get-text-property 0 'keymap fold))
        (should-not (get-text-property 0 'keymap died))))))

(provide 'overblock-run-test)
;;; overblock-run-test.el ends here
