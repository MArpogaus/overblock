;;; overblock-examples-test.el --- Tests of the two example modes -*- lexical-binding: t; -*-

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
;; The two modes of examples/, which docs/custom-mode.org walks
;; through.  The quote mode renders, and the notebook finds its cells
;; and draws its bars.  No bash starts here: the live suite
;; overblock-sh-live-test.el runs real cells.

;;; Code:

(require 'ert)
(require 'sh-script)
(require 'overblock-quote)
(require 'overblock-sh)

(defconst overblock-examples-test--script
  "#!/bin/bash\n# %% one\necho hi\n# %%\nls /nope\n"
  "A script of two cells, the second without a title.")

(defun overblock-examples-test--quotes ()
  "Return what each quote of the buffer shows, in order."
  (mapcar (lambda (block) (substring-no-properties (overblock-get block :over)))
          (sort (overblock-in (point-min) (point-max) 'quote)
                (lambda (a b) (< (overlay-start a) (overlay-start b))))))

(ert-deftest overblock-examples-test-a-quote-renders-without-markers ()
  "Every run of quoted lines away from point renders, without its markers."
  (with-temp-buffer
    (insert "Hi\n> one\n>two\n\ntext\n> last")
    (text-mode)
    (goto-char (point-min))
    (overblock-quote-mode 1)
    (should (equal (overblock-examples-test--quotes) '("one\ntwo\n" "last")))
    (overblock-quote-mode -1)
    (should-not (overblock-in (point-min) (point-max)))))

(ert-deftest overblock-examples-test-the-quote-at-point-stays-source ()
  "The quote point is in is not rendered, so it can be written."
  (with-temp-buffer
    (insert "> one\n\n> two\n")
    (text-mode)
    (goto-char 3)
    (overblock-quote-mode 1)
    (should (equal (overblock-examples-test--quotes) '("two\n")))))

(ert-deftest overblock-examples-test-the-quote-mode-wants-text ()
  "The quote mode refuses a buffer that is not text, and stays off."
  (with-temp-buffer
    (fundamental-mode)
    (should-error (overblock-quote-mode 1) :type 'user-error)
    (should-not overblock-quote-mode)))

(ert-deftest overblock-examples-test-the-cells-and-their-bars ()
  "Each `# %%' line starts a cell with a bar; the code is what follows it."
  (with-temp-buffer
    (insert overblock-examples-test--script)
    (sh-mode)
    (overblock-sh-mode 1)
    (should (equal (mapcar #'marker-position (overblock-sh--starts)) '(13 30)))
    (should (equal (mapcar #'overblock-bar-kind (overblock-bars)) '(sh sh)))
    (goto-char 33)
    (should (equal (overblock-sh--region-at) '(30 . 44)))
    (should (equal (overblock-sh--code-at) '(35 . 44)))
    ;; The shebang is above the first cell, and is no cell.
    (goto-char (point-min))
    (should-not (overblock-sh--code-at))
    (overblock-sh-mode -1)
    (should-not (overblock-bars))))

(ert-deftest overblock-examples-test-a-script-with-a-cell-turns-the-mode-on ()
  "`overblock-sh-mode-maybe' turns the mode on only where there is a cell.
The titles come from the boundary lines."
  (with-temp-buffer
    (insert "echo plain\n")
    (sh-mode)
    (overblock-sh-mode-maybe)
    (should-not overblock-sh-mode)
    (insert overblock-examples-test--script)
    (overblock-sh-mode-maybe)
    (should overblock-sh-mode)
    (should (equal (overblock-sh--title 24 32) "one"))
    (overblock-sh-mode -1)))

(ert-deftest overblock-examples-test-the-output-of-bash-is-cleaned ()
  "The prompt goes from a result, and a failed cell reads as one."
  (should (equal (overblock-sh--clean "1\n2\noverblock-sh$ ") "1\n2"))
  (should (overblock-sh--error-p "ls: no\n[exit 2]"))
  (should (overblock-sh--error-p "bash: syntax error near unexpected token"))
  (should-not (overblock-sh--error-p "[exit 2] is what it said\nok")))

(provide 'overblock-examples-test)
;;; overblock-examples-test.el ends here
