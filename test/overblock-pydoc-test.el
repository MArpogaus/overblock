;;; overblock-pydoc-test.el --- Tests for the doc string overlay  -*- lexical-binding: t; -*-

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
(require 'python)
(require 'overblock-pydoc)

(defconst overblock-pydoc-test--source
  "\"\"\"The module.\"\"\"


def f(x):
    \"\"\"Do a thing.

    Parameters
    ----------
    x : int
        the thing to do
    \"\"\"
    return x


class C:
    # a comment between the class and its doc string
    \"\"\"The class.\"\"\"

    def m(self):
        \"\"\"The method.\"\"\"
        s = \"\"\"data, not documentation\"\"\"
        return s
"
  "A Python buffer with a doc string of every kind.
It carries the doc string of a module, of a function, of a class behind
a comment and of a method — and one string that is data rather than
documentation.")

(defmacro overblock-pydoc-test--with (&rest body)
  "Evaluate BODY in a Python buffer holding the source above."
  (declare (indent 0))
  `(with-temp-buffer
     (python-mode)
     (insert overblock-pydoc-test--source)
     (goto-char (point-min))
     ,@body))

(defun overblock-pydoc-test--wait (count)
  "Wait until COUNT doc strings carry a rendering, and return how many do.
The rendering is asked of a process and not waited for, which is the
point of it: a test has to wait where a reader does not."
  (let ((deadline (+ (float-time) 10)))
    (while (and (< (length (overblock-in (point-min) (point-max) 'pydoc))
                   count)
                (< (float-time) deadline))
      (accept-process-output nil 0.05)))
  (length (overblock-in (point-min) (point-max) 'pydoc)))

(defun overblock-pydoc-test--first-lines ()
  "Return the first line of the prose of every doc string found."
  (mapcar (lambda (bounds)
            (car (split-string (overblock-pydoc--source (car bounds)
                                                        (cdr bounds))
                               "\n")))
          (overblock-pydoc--strings (point-min) (point-max))))

(ert-deftest overblock-pydoc-test-a-doc-string-opens-its-line ()
  "Every doc string is found, and a string that is data is not one.
The module, the function, the class behind its comment and the method
are documentation; the string assigned inside the method is a value."
  (overblock-pydoc-test--with
    (should (equal (overblock-pydoc-test--first-lines)
                   '("The module." "Do a thing." "The class."
                     "The method.")))))

(ert-deftest overblock-pydoc-test-a-doc-string-ends-at-its-quotes ()
  "The bounds reach from the opening quotes to past the closing ones.
`scan-sexps' cannot answer this: `python-mode' gives the first of three
quotes the syntax of a plain string delimiter, so a scan from the
start reads the first two as an empty string."
  (overblock-pydoc-test--with
    (let ((first (car (overblock-pydoc--strings (point-min) (point-max)))))
      (should (equal (buffer-substring-no-properties (car first) (cdr first))
                     "\"\"\"The module.\"\"\"")))))

(ert-deftest overblock-pydoc-test-the-prose-loses-its-indentation ()
  "The quotes go, and the indentation the lines share with the code.
A doc string is written where the code stands and reads as prose one
column from the left."
  (overblock-pydoc-test--with
    (let ((bounds (nth 1 (overblock-pydoc--strings (point-min) (point-max)))))
      (should (equal (overblock-pydoc--source (car bounds) (cdr bounds))
                     "Do a thing.\n\nParameters\n----------\nx : int\n    the thing to do")))))

(ert-deftest overblock-pydoc-test-the-markup-is-rendered ()
  "A doc string carries its rendering, and reST is what it is read as."
  (skip-unless (overblock-md-program))
  (overblock-pydoc-test--with
    (overblock-pydoc-mode 1)
    (unwind-protect
        (progn
          (goto-char (point-max))
          (overblock-pydoc-render-buffer)
          (should (= (overblock-pydoc-test--wait 4) 4))
          (let ((blocks (overblock-in (point-min) (point-max) 'pydoc)))
            ;; the roles of a numpydoc section survive, the underline
            ;; that marks them does not
            (let ((shown (substring-no-properties
                          (overblock-get (nth 1 blocks) :over))))
              (should (string-match-p "Parameters" shown))
              (should-not (string-match-p "----" shown)))))
      (overblock-pydoc-mode -1))))

(ert-deftest overblock-pydoc-test-point-inside-shows-the-source ()
  "The doc string point is in shows its source; leaving renders it again."
  (skip-unless (overblock-md-program))
  (overblock-pydoc-test--with
    (overblock-pydoc-mode 1)
    (unwind-protect
        (let ((count (lambda ()
                       (length (overblock-in (point-min) (point-max)
                                             'pydoc)))))
          (goto-char (point-max))
          (overblock-pydoc-render-buffer)
          (should (= (overblock-pydoc-test--wait 4) 4))
          ;; a click takes one rendering off
          (goto-char (point-min))
          (overblock-live-edit)
          (should (= (funcall count) 3))
          ;; and the next pass puts it back
          (goto-char (point-max))
          (overblock-pydoc-render-buffer)
          (should (= (overblock-pydoc-test--wait 4) 4)))
      (overblock-pydoc-mode -1))))

(ert-deftest overblock-pydoc-test-the-mode-leaves-nothing-behind ()
  "Turning the mode off gives the buffer back as it was."
  (skip-unless (overblock-md-program))
  (overblock-pydoc-test--with
    (let ((before (buffer-string)))
      (overblock-pydoc-mode 1)
      (goto-char (point-max))
      (overblock-pydoc-render-buffer)
      (should (= (overblock-pydoc-test--wait 4) 4))
      (overblock-pydoc-mode -1)
      (should-not (overblock-in (point-min) (point-max) 'pydoc))
      (should-not overblock-live--spec)
      (should (equal (buffer-string) before)))))

(provide 'overblock-pydoc-test)
;;; overblock-pydoc-test.el ends here
