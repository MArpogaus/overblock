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

(ert-deftest overblock-md-preview-test-a-fence-is-not-markdown ()
  "The lines of a fenced code block are left alone, the fences too.
A fence line renders as itself, and the code between two of them is
code: neither is a line of markdown, and the line after the closing
fence is one again."
  (with-temp-buffer
    (insert "prose\n\n```\ncode\n```\ntail\n\n~~~\nmore code\n~~~\n")
    (should (equal (mapcar (lambda (line)
                             (buffer-substring-no-properties
                              (car line) (cdr line)))
                           (overblock-md-preview--lines (point-min)
                                                        (point-max)))
                   '("prose" "tail")))))

(ert-deftest overblock-md-preview-test-a-line-is-asked-from-the-top ()
  "A region inside a fence is known to be inside it.
The walk starts at the top of the buffer whatever the region says,
because nothing else can tell whether the region opened in a fence."
  (with-temp-buffer
    (insert "```\none\ntwo\n```\nthree\n")
    (let ((inside (progn (goto-char (point-min)) (forward-line 2) (point))))
      ;; asked from the middle of the fence: "two" is still code, while
      ;; the line after the closing fence is markdown again
      (should (equal (mapcar (lambda (line)
                               (buffer-substring-no-properties
                                (car line) (cdr line)))
                             (overblock-md-preview--lines inside
                                                          (point-max)))
                     '("three"))))))

(ert-deftest overblock-md-preview-test-every-line-is-rendered ()
  "Each line of markdown carries its own rendering."
  (skip-unless (overblock-md-program))
  (overblock-md-preview-test--with "# A heading\n\nsome *emphasis*\n"
    (goto-char (point-max))
    (overblock-md-preview--render-elsewhere)
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
    (overblock-md-preview--render-elsewhere)
    (should (equal (overblock-md-preview-test--sources)
                   '("# One" "two" "three")))
    (goto-char (point-min))
    (overblock-md-preview--reveal-here)
    (should (equal (overblock-md-preview-test--sources) '("two" "three")))
    (goto-char (point-max))
    (overblock-md-preview--render-elsewhere)
    (should (equal (overblock-md-preview-test--sources)
                   '("# One" "two" "three")))))

(ert-deftest overblock-md-preview-test-an-edit-drops-the-rendering ()
  "An edit the mode did not see coming takes that line's rendering down.
Point landing on a line takes its rendering off, so a reader's own
typing never reaches this; a replacement over the buffer does."
  (skip-unless (overblock-md-program))
  (overblock-md-preview-test--with "# One\n\ntwo\n"
    (goto-char (point-max))
    (overblock-md-preview--render-elsewhere)
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
      (overblock-md-preview--render-elsewhere)
      (should (overblock-md-preview-test--blocks))
      (overblock-md-preview-mode -1)
      (should-not (overblock-md-preview-test--blocks))
      (should-not overblock-md-preview--timer)
      (should (equal (buffer-string) before)))))

(provide 'overblock-md-preview-test)
;;; overblock-md-preview-test.el ends here
