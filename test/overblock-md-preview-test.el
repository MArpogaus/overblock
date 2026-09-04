;;; overblock-md-preview-test.el --- Tests for the markdown preview  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Marcel Arpogaus

;; Author: Marcel Arpogaus <znepry.necbtnhf@tznvy.pbz>
;; Assisted-by: Claude:claude-opus-5
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

;; Run with: make test
;;
;; The tests that need a converter skip themselves where none is
;; installed; `make test STRICT=1' refuses to skip.

;;; Code:

(require 'ert)
(require 'overblock-md-preview)

(defmacro overblock-md-preview-test--with (text &rest body)
  "Evaluate BODY in a buffer holding TEXT with the mode on."
  (declare (indent 1))
  `(with-temp-buffer
     (insert ,text)
     (goto-char (point-min))
     (overblock-md-preview-mode 1)
     (unwind-protect (progn ,@body)
       (overblock-md-preview-mode -1))))

(defun overblock-md-preview-test--blocks ()
  "Return the preview blocks of this buffer."
  (overblock-in (point-min) (point-max) 'md-preview))

(defun overblock-md-preview-test--sources ()
  "Return the source line of every rendered line, in order."
  (mapcar (lambda (block)
            (string-trim (buffer-substring-no-properties
                          (overlay-start block) (overlay-end block))))
          (overblock-md-preview-test--blocks)))

(defun overblock-md-preview-test--texts (beg end)
  "Return the text of every markdown block between BEG and END."
  (mapcar (lambda (region)
            (buffer-substring-no-properties (car region) (cdr region)))
          (overblock-md-preview--regions beg end)))

(ert-deftest overblock-md-preview-test-a-block-is-what-markdown-calls-one ()
  "The run of lines between two blank ones, and a fence whole.
A line of markdown is often not markdown alone: a row of a table needs
the rows around it, and a fenced piece of code keeps the blank lines it
holds."
  (with-temp-buffer
    (insert "# Heading\n\npara one\ncontinues\n\n| a | b |\n|---|---|\n"
            "| 1 | 2 |\n\n```\ncode\n\nwith a blank\n```\n\n- one\n- two\n")
    (should (equal (overblock-md-preview-test--texts (point-min) (point-max))
                   '("# Heading"
                     "para one\ncontinues"
                     "| a | b |\n|---|---|\n| 1 | 2 |"
                     "```\ncode\n\nwith a blank\n```"
                     "- one\n- two")))))

(ert-deftest overblock-md-preview-test-a-table-renders-as-a-table ()
  "A table reaches the converter whole, and comes back with its columns.
Rendered a row at a time, the rule between the head and the body came
back as a row of empty cells and every row as a paragraph of its own."
  (skip-unless (overblock-md-program))
  (with-temp-buffer
    (insert "| a | b |\n|---|---|\n| 1 | 2 |\n")
    (let* ((region (car (overblock-md-preview--regions (point-min)
                                                       (point-max))))
           (block (overblock-md-preview--show (car region) (cdr region)))
           (shown (substring-no-properties (overblock-get block :over))))
      ;; the rule is gone and the cells stand in their columns
      (should-not (string-match-p "---" shown))
      (should (string-match-p "a +b" shown))
      (should (string-match-p "1 +2" shown)))))

(ert-deftest overblock-md-preview-test-a-region-is-read-from-the-top ()
  "A region inside a fence is known to be inside it.
The walk starts at the top of the buffer whatever the region says,
because nothing else can tell whether the region opened in a fence."
  (with-temp-buffer
    (insert "```\none\ntwo\n```\n\nthree\n")
    (let ((inside (progn (goto-char (point-min)) (forward-line 2) (point))))
      ;; the fence is one block and it began before the region, so what
      ;; is left is the paragraph after it
      (should (equal (overblock-md-preview-test--texts inside (point-max))
                     '("three"))))))

(ert-deftest overblock-md-preview-test-every-line-is-rendered ()
  "Each line of markdown carries its own rendering."
  (skip-unless (overblock-md-program))
  (overblock-md-preview-test--with "# A heading\n\nsome *emphasis*\n"
    (goto-char (point-max))
    (overblock-live--render-elsewhere)
    (should (equal (overblock-md-preview-test--sources)
                   '("# A heading" "some *emphasis*")))
    ;; the markup is gone from what the reader sees
    (let ((shown (overblock-get (car (overblock-md-preview-test--blocks))
                                :over)))
      (should (equal (string-trim (substring-no-properties shown))
                     "A heading")))))

(ert-deftest overblock-md-preview-test-the-line-at-point-shows-its-source ()
  "The line point is on is the one being edited, so it is not rendered.
Leaving it renders it again, which is the whole of the cycle."
  (skip-unless (overblock-md-program))
  (overblock-md-preview-test--with "# One\n\ntwo\n\nthree\n"
    (goto-char (point-max))
    (overblock-live--render-elsewhere)
    (should (equal (overblock-md-preview-test--sources)
                   '("# One" "two" "three")))
    (goto-char (point-min))
    (overblock-live-edit)
    (should (equal (overblock-md-preview-test--sources) '("two" "three")))
    (goto-char (point-max))
    (overblock-live--render-elsewhere)
    (should (equal (overblock-md-preview-test--sources)
                   '("# One" "two" "three")))))

(ert-deftest overblock-md-preview-test-an-edit-drops-the-rendering ()
  "An edit the mode did not see coming takes that line's rendering down.
Point landing on a line takes its rendering off, so a reader's own
typing never reaches this; a replacement over the buffer does."
  (skip-unless (overblock-md-program))
  (overblock-md-preview-test--with "# One\n\ntwo\n"
    (goto-char (point-max))
    (overblock-live--render-elsewhere)
    (should (equal (overblock-md-preview-test--sources) '("# One" "two")))
    (goto-char (point-min))
    (while (search-forward "One" nil t) (replace-match "Three"))
    (should (equal (overblock-md-preview-test--sources) '("two")))))

(ert-deftest overblock-md-preview-test-the-mode-leaves-nothing-behind ()
  "Turning the mode off gives the buffer back as it was."
  (skip-unless (overblock-md-program))
  (with-temp-buffer
    (insert "# One\n\ntwo\n")
    (let ((before (buffer-string)))
      (overblock-md-preview-mode 1)
      (goto-char (point-max))
      (overblock-live--render-elsewhere)
      (should (overblock-md-preview-test--blocks))
      (overblock-md-preview-mode -1)
      (should-not (overblock-md-preview-test--blocks))
      ;; and the cycle the layer runs is stopped with it
      (should-not overblock-live--timer)
      (should-not overblock-live--spec)
      (should (equal (buffer-string) before)))))

(provide 'overblock-md-preview-test)
;;; overblock-md-preview-test.el ends here
