;;; overblock-sh-live-test.el --- The example notebook against a real bash -*- lexical-binding: t; -*-

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

;; Run with: make test-live
;;
;; The notebook of examples/overblock-sh.el, sending real cells to a
;; real bash: the result of a cell, a pass that stops at an error, and
;; a restart that forgets what the shell knew.

;;; Code:

(require 'ert)
(require 'sh-script)
(require 'overblock-test-common)
(require 'overblock-sh)

(defun overblock-sh-live-test--idle-p ()
  "Return non-nil while bash is there, runs no cell and has none queued."
  (and (overblock-sh--process)
       (not (overblock-run-running-in-p (point-min) (point-max)))
       (not (overblock-run--queued))))

(defun overblock-sh-live-test--texts ()
  "Wait until bash is idle, and return the text of every result."
  (should (overblock-test-common-wait #'overblock-sh-live-test--idle-p))
  (mapcar #'overblock-test-common-text (overblock-test-common-results)))

(defmacro overblock-sh-live-test--with-script (text &rest body)
  "Evaluate BODY in a shell script holding TEXT, with the notebook on.
The bash of the script goes afterwards."
  (declare (indent 1))
  `(with-temp-buffer
     (insert ,text)
     (sh-mode)
     (overblock-sh-mode 1)
     (goto-char (point-min))
     (unwind-protect (progn ,@body)
       (when-let* ((proc (overblock-sh--process)))
         (delete-process proc)
         (kill-buffer (process-buffer proc))))))

(ert-deftest overblock-sh-live-test-a-cell-comes-back-with-its-output ()
  "A cell of several lines prints under itself, with no prompt in it."
  (overblock-sh-live-test--with-script
      "# %%\nfor i in 1 2; do echo $i; done\nprintf end\n"
    (overblock-run-this)
    (should (equal (overblock-sh-live-test--texts) '("1\n2\nend")))))

(ert-deftest overblock-sh-live-test-a-pass-stops-at-an-error ()
  "A pass over every cell stops at the first that fails, and marks it."
  (overblock-sh-live-test--with-script
      "# %%\necho one\n# %%\nls /nope\n# %%\necho never\n"
    (overblock-run-restart-and-run-all)
    (let ((texts (overblock-sh-live-test--texts)))
      (should (= (length texts) 2))
      (should (equal (car texts) "one"))
      (should (string-suffix-p "[exit 2]" (cadr texts)))
      (should (eq (plist-get (overblock-get (cadr (overblock-test-common-results))
                                            :data)
                             :state)
                  'failed)))))

(ert-deftest overblock-sh-live-test-a-restart-forgets-the-shell ()
  "A restart takes the results down, and the new bash knows nothing."
  (overblock-sh-live-test--with-script "# %%\nx=${x:-0}1\necho $x\n"
    (overblock-run-this)
    (should (equal (overblock-sh-live-test--texts) '("01")))
    (overblock-run-this)
    (should (equal (overblock-sh-live-test--texts) '("011")))
    (overblock-run-restart)
    (should-not (overblock-test-common-results))
    (overblock-run-this)
    (should (equal (overblock-sh-live-test--texts) '("01")))))

(provide 'overblock-sh-live-test)
;;; overblock-sh-live-test.el ends here
