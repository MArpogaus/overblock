;;; overblock-pycell-live-test.el --- Tests against a real IPython -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Marcel Arpogaus

;; Author: Marcel Arpogaus <znepry.necbtnhf@tznvy.pbz>
;; Assisted-by: Claude:claude-opus-5
;; Assisted-by: Claude:claude-fable-5
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

;; Run with: make test-live
;;
;; What only a live interpreter can prove: the batch suite starts no
;; process.  These tests send real cells to a real IPython and read
;; back what the blocks show, for example a result in a read-only
;; notebook and the face of a one-line result.
;;
;; `make test' does not load this file, and the target that does skips
;; with a message where no ipython is installed, as on a batch CI.

;;; Code:

(require 'ert)
(require 'overblock-test-common)
(require 'overblock-pycell)

(defun overblock-pycell-live-test--idle-p ()
  "Return non-nil while the shell is there and runs no cell."
  (when-let* ((proc (python-shell-get-process)))
    (not (buffer-local-value 'overblock-run--state (process-buffer proc)))))

(defmacro overblock-pycell-live-test--with-notebook (text &rest body)
  "Evaluate BODY in a notebook holding TEXT, wired for a real IPython.
The shell of an earlier test is reused where one is alive: each
startup costs seconds, and the package keeps one shell for a session
anyway."
  (declare (indent 1))
  `(let ((python-shell-interpreter "ipython")
         (python-shell-interpreter-args "-i --simple-prompt")
         (python-shell-prompt-detect-failure-warning nil)
         (python-shell-completion-native-enable nil)
         (buffer (generate-new-buffer "overblock-pycell-live.py")))
     (unwind-protect
         (with-current-buffer buffer
           (insert ,text)
           (setq buffer-file-name "/tmp/overblock-pycell-live.py")
           (python-mode)
           (code-cells-mode)
           (overblock-pycell-mode 1)
           (goto-char (point-min))
           ,@body)
       (with-current-buffer buffer
         (overblock-pycell-mode -1)
         (set-buffer-modified-p nil))
       (kill-buffer buffer))))

(ert-deftest overblock-pycell-live-test-a-one-line-result-is-plain ()
  "A cell that prints one line comes back with no prompt face on it.
comint calls a chunk of output that ends without a newline a prompt and
paints it `comint-highlight-prompt', and one printed line arrives as
one such chunk."
  (overblock-pycell-live-test--with-notebook "# %%\nprint('one')\n"
    (pcase-let ((`(,beg ,end) (code-cells--bounds nil nil t)))
      (overblock-pycell-eval-region beg end))
    (should (overblock-test-common-wait
             (lambda () (and (overblock-pycell-live-test--idle-p)
                             (overblock-test-common-results)))
             60))
    (let* ((block (car (overblock-test-common-results)))
           (text (plist-get (overblock-get block :data) :text)))
      (should (equal (substring-no-properties text) "one"))
      (dotimes (i (length text))
        (should-not (memq 'comint-highlight-prompt
                          (ensure-list (get-text-property
                                        i 'font-lock-face text))))))))

(ert-deftest overblock-pycell-live-test-a-read-only-notebook-gets-its-result ()
  "A read-only notebook shows the result and the pass survives.
The result of the last cell of a file without a final newline hangs
on a newline written there.  A read-only buffer refuses the write, and
an error in the process filter would leave the shell busy."
  (overblock-pycell-live-test--with-notebook "# %%\nprint('ro')"   ; no final newline
    (setq buffer-read-only t)
    (let ((size (buffer-size)))
      (pcase-let ((`(,beg ,end) (code-cells--bounds nil nil t)))
        (overblock-pycell-eval-region beg end))
      (should (overblock-test-common-wait
               (lambda () (and (overblock-pycell-live-test--idle-p)
                               (overblock-test-common-results)))
               60))
      (should (equal (overblock-test-common-text
                      (car (overblock-test-common-results)))
                     "ro"))
      ;; The buffer was not written to.
      (should (= (buffer-size) size)))))

(ert-deftest overblock-pycell-live-test-a-run-all-stops-at-an-error ()
  "A pass over all the cells stops at the first cell that raises."
  (overblock-pycell-live-test--with-notebook
      "# %%\nprint('a')\n\n# %%\nraise ValueError('boom')\n\n# %%\nprint('never')\n"
    (overblock-pycell-restart-and-run-all)
    (should (overblock-test-common-wait
             (lambda () (and (overblock-pycell-live-test--idle-p)
                             (null (overblock-run--queued))
                             (= (length (overblock-test-common-results)) 2)))
             60))
    (should (equal (overblock-test-common-text (car (overblock-test-common-results)))
                   "a"))
    (should (string-match-p "ValueError"
                            (overblock-test-common-text
                             (cadr (overblock-test-common-results)))))))

(ert-deftest overblock-pycell-live-test-a-cell-after-a-restart-waits-for-the-prompt ()
  "A cell asked for at once after a restart gets its output, not the banner."
  (overblock-pycell-live-test--with-notebook
      "# %%\nprint('one')\n\n# %%\nprint('two')\n"
    (overblock-pycell-eval-region (point-min) (point-max))
    (should (overblock-test-common-wait
             (lambda () (and (overblock-pycell-live-test--idle-p)
                             (overblock-test-common-results)))
             60))
    (overblock-pycell-restart)
    (goto-char (point-max))
    (pcase-let ((`(,beg ,end) (code-cells--bounds nil nil t)))
      (overblock-pycell-eval-region beg end))
    (should (overblock-test-common-wait
             (lambda () (and (overblock-pycell-live-test--idle-p)
                             (= (length (overblock-test-common-results)) 1)))
             60))
    (should (equal (overblock-test-common-text
                    (car (overblock-test-common-results)))
                   "two"))))

(ert-deftest overblock-pycell-live-test-stop-works-while-the-last-cell-runs ()
  "`overblock-run-stop' during the last cell of a pass leaves nothing queued.
The last cell of a pass is sent with the queue already empty, and the
stop can be called from any buffer, so it must find the shell itself.
The running cell runs to its end, and the pass ends clean."
  (overblock-pycell-live-test--with-notebook
      "# %%\nprint('a')\n\n# %%\nimport time; time.sleep(1)\n"
    (overblock-pycell-restart-and-run-all)
    ;; The last cell runs: nothing queued, one cell live.
    (should (overblock-test-common-wait
             (lambda ()
               (when-let* ((proc (python-shell-get-process)))
                 (and (null (overblock-run--queued))
                      (buffer-local-value 'overblock-run--state
                                          (process-buffer proc)))))
             60))
    ;; From another buffer, as a key of another map would be.
    (with-temp-buffer (overblock-run-stop))
    (should-not (overblock-run--queued))
    (should (overblock-test-common-wait #'overblock-pycell-live-test--idle-p 60))
    (should-not (overblock-run--queued))
    ;; The running cell was not cut short: both results arrived.
    (should (= (length (overblock-test-common-results)) 2))))

(provide 'overblock-pycell-live-test)
;;; overblock-pycell-live-test.el ends here
