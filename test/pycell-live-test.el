;;; pycell-live-test.el --- Tests against a real IPython -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Marcel Arpogaus

;; Author: Marcel Arpogaus <znepry.necbtnhf@tznvy.pbz>
;; Assisted-by: Claude:claude-opus-5
;; Assisted-by: Claude:claude-fable-5
;; URL: https://github.com/MArpogaus/pycell

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

;; Run with: make test-live
;;
;; What only a live interpreter can prove.  The batch suite starts no
;; process, and two faults lived past every one of its checks for
;; exactly that reason: a read-only notebook wedged the pass from
;; inside the process filter, and a one-line result came back painted
;; in comint's prompt face.  These tests send real cells down a real
;; IPython and read back what the blocks show.
;;
;; `make test' does not load this file, and the target that does skips
;; with a word where no ipython is installed: a batch CI has none.

;;; Code:

(require 'ert)
(require 'pycell)

(defun pycell-live-test--wait (predicate &optional seconds)
  "Wait until PREDICATE answers non-nil and return that answer.
Give up after SECONDS, thirty by default, and answer whatever the
predicate says then."
  (let ((deadline (+ (float-time) (or seconds 30))))
    (while (and (not (funcall predicate)) (< (float-time) deadline))
      (accept-process-output nil 0.05))
    (funcall predicate)))

(defun pycell-live-test--idle-p ()
  "Return non-nil while the shell is there and runs no cell."
  (when-let* ((proc (python-shell-get-process)))
    (not (buffer-local-value 'pycell--run (process-buffer proc)))))

(defun pycell-live-test--results ()
  "Return the result blocks of the buffer, in order."
  (sort (overblock-in (point-min) (point-max) 'result)
        (lambda (a b) (< (overlay-start a) (overlay-start b)))))

(defun pycell-live-test--text (block)
  "Return what the result BLOCK shows, without its properties."
  (substring-no-properties
   (or (plist-get (overblock-get block :data) :text) "")))

(defmacro pycell-live-test--with-notebook (text &rest body)
  "Evaluate BODY in a notebook holding TEXT, wired for a real IPython.
The shell of an earlier test is reused where one is alive, which is
what the tests want: each startup costs seconds, and the package is
meant to keep one shell across a session anyway."
  (declare (indent 1))
  `(let ((python-shell-interpreter "ipython")
         (python-shell-interpreter-args "-i --simple-prompt")
         (python-shell-prompt-detect-failure-warning nil)
         (python-shell-completion-native-enable nil)
         (buffer (generate-new-buffer "pycell-live.py")))
     (unwind-protect
         (with-current-buffer buffer
           (insert ,text)
           (setq buffer-file-name "/tmp/pycell-live.py")
           (python-mode)
           (code-cells-mode)
           (pycell-mode 1)
           (goto-char (point-min))
           ,@body)
       (with-current-buffer buffer
         (pycell-mode -1)
         (set-buffer-modified-p nil))
       (kill-buffer buffer))))

(ert-deftest pycell-live-test-a-one-line-result-is-plain ()
  "A cell that prints one line comes back with no prompt face on it.
comint calls a chunk of output that ends without a newline a prompt and
paints it `comint-highlight-prompt', and one printed line arrives as
exactly one such chunk: the commonest result of all read as a prompt."
  (pycell-live-test--with-notebook "# %%\nprint('one')\n"
    (pcase-let ((`(,beg ,end) (code-cells--bounds nil nil t)))
      (pycell-eval-region beg end))
    (should (pycell-live-test--wait
             (lambda () (and (pycell-live-test--idle-p)
                             (pycell-live-test--results)))
             60))
    (let* ((block (car (pycell-live-test--results)))
           (text (plist-get (overblock-get block :data) :text)))
      (should (equal (substring-no-properties text) "one"))
      (dotimes (i (length text))
        (should-not (memq 'comint-highlight-prompt
                          (ensure-list (get-text-property
                                        i 'font-lock-face text))))))))

(ert-deftest pycell-live-test-a-read-only-notebook-gets-its-result ()
  "A read-only notebook shows the result and the pass survives.
The result of the last cell of a file without a final newline is hung
on a newline written there, and a notebook that refused the write
signalled from inside the process filter: the cell was never ended and
the shell stayed busy for the session."
  (pycell-live-test--with-notebook "# %%\nprint('ro')"   ; no final newline
    (setq buffer-read-only t)
    (let ((size (buffer-size)))
      (pcase-let ((`(,beg ,end) (code-cells--bounds nil nil t)))
        (pycell-eval-region beg end))
      (should (pycell-live-test--wait
               (lambda () (and (pycell-live-test--idle-p)
                               (pycell-live-test--results)))
               60))
      (should (equal (pycell-live-test--text
                      (car (pycell-live-test--results)))
                     "ro"))
      ;; the buffer was not written to
      (should (= (buffer-size) size)))))

(ert-deftest pycell-live-test-a-run-all-stops-at-an-error ()
  "A pass over all the cells stops at the first cell that raises."
  (pycell-live-test--with-notebook
      "# %%\nprint('a')\n\n# %%\nraise ValueError('boom')\n\n# %%\nprint('never')\n"
    (pycell-restart-and-run-all)
    (should (pycell-live-test--wait
             (lambda () (and (pycell-live-test--idle-p)
                             (null (pycell--queued))
                             (= (length (pycell-live-test--results)) 2)))
             60))
    (should (equal (pycell-live-test--text (car (pycell-live-test--results)))
                   "a"))
    (should (string-match-p "ValueError"
                            (pycell-live-test--text
                             (cadr (pycell-live-test--results)))))))

(ert-deftest pycell-live-test-stop-works-while-the-last-cell-runs ()
  "`pycell-stop' during the last cell of a pass leaves nothing queued.
The last cell of a pass is sent with the queue already empty, and the
reader watching a long run has their point anywhere at all — the stop
must resolve the shell rather than fall over the buffer it is called
in.  The running cell runs to its end, and the pass ends clean."
  (pycell-live-test--with-notebook
      "# %%\nprint('a')\n\n# %%\nimport time; time.sleep(1)\n"
    (pycell-restart-and-run-all)
    ;; the last cell is the one running: nothing queued, one cell live
    (should (pycell-live-test--wait
             (lambda ()
               (when-let* ((proc (python-shell-get-process)))
                 (and (null (pycell--queued))
                      (buffer-local-value 'pycell--run
                                          (process-buffer proc)))))
             60))
    ;; from another buffer, as a key bound in some other map would be
    (with-temp-buffer (pycell-stop))
    (should-not (pycell--queued))
    (should (pycell-live-test--wait #'pycell-live-test--idle-p 60))
    (should-not (pycell--queued))
    ;; the running cell was not cut short: both results arrived
    (should (= (length (pycell-live-test--results)) 2))))

(provide 'pycell-live-test)
;;; pycell-live-test.el ends here
