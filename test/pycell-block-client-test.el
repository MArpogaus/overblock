;;; pycell-block-client-test.el --- A second client of the block layer  -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Marcel Arpogaus

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

;; The block layer is meant to carry more than the results and the
;; markdown cells of this package.  This file is the second caller that
;; proves it: it renders the math in a Python docstring over the lines of
;; that docstring.  There is no process behind it, no cell and no
;; markdown, and it asks the layer for nothing that is not there.
;;
;; It is a test, not a feature.  What it is worth is the answer to one
;; question: does a caller of another shape need anything the layer does
;; not have?

;;; Code:

(require 'ert)
(require 'pycell)

;;;; The client

(defconst docmath--regexp "\\$\\([^$\n]+\\)\\$"
  "A piece of math between dollars.")

(defun docmath--render (text)
  "Return TEXT with each formula rendered.
A formula becomes an image where Org can make one, and stands out in a
face where it cannot: a caller renders, and a block shows what it gets."
  (replace-regexp-in-string
   docmath--regexp
   ;; The dollars are the first and the last character of the match, so
   ;; the match data is not asked for them.  It is saved all the same: a
   ;; renderer calls Org, Org matches things of its own, and
   ;; `replace-regexp-in-string' reads the match data again after this
   ;; function returns to find where to go on.
   (lambda (match)
     (let ((frag (substring match 1 -1)))
       (save-match-data
         (or (ignore-errors (pycell--md-latex-image (format "\\(%s\\)" frag)))
             (propertize frag 'face 'pycell-md-code)))))
   text t t))

(defun docmath--bounds ()
  "Return the bounds of the docstring at point, or nil.
A docstring here is what lies between two lines of three quotes."
  (save-excursion
    (let ((beg (progn (goto-char (point-min))
                      (when (re-search-forward "^ *\"\"\"\n" nil t) (point)))))
      (when (and beg (re-search-forward "^ *\"\"\"" nil t))
        (list beg (pos-bol))))))

(defun docmath-render ()
  "Show the docstring of the buffer with its math rendered."
  (interactive)
  (pcase-let ((`(,beg ,end) (docmath--bounds)))
    (unless beg (user-error "No docstring here"))
    (pycell--block-show
     beg end
     :kind 'docmath
     :over (docmath--render (buffer-substring-no-properties beg end))
     :header (pycell--bar "docstring" "")
     :keymap (define-keymap "q" #'docmath-plain))))

(defun docmath-plain ()
  "Show the docstring of the buffer as it is written."
  (interactive)
  (pycell--block-remove (point-min) (point-max) 'docmath))

;;;; What the layer had to answer for

(defmacro docmath-test--with-docstring (&rest body)
  "Run BODY in a buffer with a Python docstring in it."
  `(with-temp-buffer
     (insert "def f(x):\n"
             "    \"\"\"\n"
             "    Return $x^2$ for x.\n"
             "\n"
             "    The bound is $c > 0$.\n"
             "    \"\"\"\n"
             "    return x * x\n")
     (goto-char (point-min))
     ,@body))

(ert-deftest docmath-test-renders-over-the-source ()
  "The rendering hangs on the lines of the docstring, a piece to a line.
The source stays as it is: the block shows text, it does not write any."
  (docmath-test--with-docstring
   (let ((before (buffer-string)))
     (docmath-render)
     (let* ((block (car (pycell--block-in (point-min) (point-max) 'docmath)))
            (parts (pycell--block-get block :parts))
            (shown (mapconcat (lambda (ov) (or (overlay-get ov 'display) ""))
                              parts "\n")))
       (should block)
       (should (> (length parts) 1))
       ;; the dollars are gone from what shows, and the math is there
       (should-not (string-match-p "\\$" shown))
       (should (string-match-p "x\\^2" shown))
       (should (equal (buffer-string) before))))))

(ert-deftest docmath-test-header-and-keymap-reach-every-piece ()
  "A caller gives the block a bar and a keymap, and the layer spreads them."
  (docmath-test--with-docstring
   (docmath-render)
   (let* ((block (car (pycell--block-in (point-min) (point-max) 'docmath)))
          (nl (pycell--block-get block :newline)))
     (should (string-match-p "docstring" (overlay-get nl 'before-string)))
     (should (keymapp (overlay-get block 'keymap)))
     (should (seq-every-p (lambda (ov) (keymapp (overlay-get ov 'keymap)))
                          (pycell--block-get block :parts))))))

(ert-deftest docmath-test-plain-again ()
  "Taking the block away leaves the buffer as it was."
  (docmath-test--with-docstring
   (let ((before (buffer-string)))
     (docmath-render)
     (docmath-plain)
     (should-not (pycell--block-in (point-min) (point-max) 'docmath))
     (should (equal (buffer-string) before))
     ;; and nothing of the block is left behind
     (should-not (seq-some (lambda (ov) (overlay-get ov 'display))
                           (overlays-in (point-min) (point-max)))))))

(ert-deftest docmath-test-a-rendered-image-rides-a-string ()
  "A piece with an image in it hides its line and shows the image beside.
The layer decides that, not the caller: display properties do not nest."
  (docmath-test--with-docstring
   (cl-letf (((symbol-function 'pycell--md-latex-image)
              (lambda (_frag)
                (propertize " " 'display '(image :type png :data "x")))))
     (docmath-render)
     (let* ((block (car (pycell--block-in (point-min) (point-max) 'docmath)))
            (withimage (seq-filter
                        (lambda (ov)
                          (pycell--image (or (overlay-get ov 'after-string) "")))
                        (pycell--block-get block :parts))))
       (should (= (length withimage) 2))
       (should (seq-every-p (lambda (ov) (equal (overlay-get ov 'display) ""))
                            withimage))))))

(provide 'pycell-block-client-test)
;;; pycell-block-client-test.el ends here
