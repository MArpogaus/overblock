;;; overblock-md-test.el --- Tests for overblock-md -*- lexical-binding: t; -*-

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

;; Run with: make test
;;
;; Markdown in, one propertized string out: the converter, the math
;; previews, the tables and the images.

;;; Code:

(require 'ert)
(require 'overblock-md)

(defconst overblock-md-test--png
  (base64-decode-string
   (concat "iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAYAAAAfFcSJAAAADUlEQVR42mP8"
           "z8DAwAAABQABDQottAAAAABJRU5ErkJggg=="))
  "One pixel of PNG, to write to a file a markdown cell can name.")

(defmacro overblock-md-test--with-image-file (name &rest body)
  "Run BODY in a directory with a PNG file, bound to NAME."
  (declare (indent 1))
  `(let* ((dir (make-temp-file "pycell-img" t))
          (,name (expand-file-name "figure.png" dir)))
     (unwind-protect
         (progn
           (let ((coding-system-for-write 'no-conversion))
             (write-region overblock-md-test--png nil ,name nil 'quiet))
           ,@body)
       (delete-directory dir t))))

(ert-deftest overblock-md-test-image-file-reads-a-path ()
  "A local path names a file; another scheme, or nothing readable, does not.
An Emacs that cannot draw a PNG answers nil for every path, and rightly:
the image then belongs to shr, which says so with a box of its own."
  (skip-unless (image-type-available-p 'png))
  (overblock-md-test--with-image-file file
    (let ((default-directory (file-name-directory file)))
      (should (equal (overblock-md--image-file "figure.png") file))
      (should (equal (overblock-md--image-file "./figure.png") file))
      (should (equal (overblock-md--image-file file) file))
      (should (equal (overblock-md--image-file (concat "file://" file)) file))
      (should-not (overblock-md--image-file "https://example.org/figure.png"))
      (should-not (overblock-md--image-file "does-not-exist.png"))
      (should-not (overblock-md--image-file "")))))

(ert-deftest overblock-md-test-names-an-image-without-a-display ()
  "Where no image can be drawn, the cell says which figure is there.
shr\='s own placeholder is an image, and its display property swallows the
text under it: a terminal would show a blank row where a figure belongs."
  (skip-unless (overblock-md-program))
  (overblock-md-test--with-image-file file
    (let* ((default-directory (file-name-directory file))
           (shown (overblock-md-rendered "![a figure](figure.png)")))
      ;; batch draws nothing, so the label stands on its own
      (should-not (display-images-p))
      (should-not (overblock-image-in shown))
      (should (string-match-p "a figure" (substring-no-properties shown))))
    ;; and with no alt text, the file names itself
    (let* ((default-directory (file-name-directory file))
           (shown (overblock-md-rendered "![](figure.png)")))
      (should (string-match-p "\\[figure.png\\]"
                              (substring-no-properties shown))))))

(ert-deftest overblock-md-test-draws-a-local-image ()
  "A markdown cell draws the image it names, rather than shr's placeholder.
shr fetches an image through `url-queue-retrieve\=', which answers after
the rendering is over; a file on disk is drawn here and now."
  (skip-unless (overblock-md-program))
  (skip-unless (image-type-available-p 'png))
  (overblock-md-test--with-image-file file
    (cl-letf* (((symbol-function 'display-images-p) (lambda (&rest _) t))
               (default-directory (file-name-directory file))
               (shown (overblock-md-rendered "![a figure](figure.png)"))
               (spec (overblock-image-in shown)))
      (should spec)
      (should (equal (plist-get (cdr spec) :file) file))
      ;; the alt text carries it, so a terminal still says what is there
      (should (string-match-p "a figure" (substring-no-properties shown))))))

(ert-deftest overblock-md-test-keeps-a-link-on-an-image ()
  "An image inside a link keeps the link: a click follows the URL.
`overblock-fill-props\=' leaves the properties shr gave the link alone."
  (skip-unless (overblock-md-program))
  (skip-unless (image-type-available-p 'png))
  (overblock-md-test--with-image-file file
    (cl-letf* (((symbol-function 'display-images-p) (lambda (&rest _) t))
               (default-directory (file-name-directory file))
               (shown (overblock-md-rendered
                       "[![a figure](figure.png)](https://example.org)"))
               (pos (text-property-not-all 0 (length shown) 'shr-url nil shown)))
      (should pos)
      (should (equal (get-text-property pos 'shr-url shown) "https://example.org"))
      (should (eq (car-safe (get-text-property pos 'display shown)) 'image))
      (should (eq (keymap-lookup (get-text-property pos 'keymap shown) "RET")
                  #'shr-browse-url)))))

(ert-deftest overblock-md-test-a-remote-image-stays-with-shr ()
  "An image on the network is shr's business, and it says so with a box."
  (skip-unless (overblock-md-program))
  (let* ((shown (overblock-md-rendered "![a figure](https://example.org/f.png)"))
         (spec (overblock-image-in shown)))
    ;; shr leaves a placeholder of its own making, and it is not a file
    (should (or (null spec) (null (plist-get (cdr spec) :file))))))

(ert-deftest overblock-md-test-program-takes-a-string-or-a-list ()
  "A string and a list of candidates both resolve to a program.
Only where there is a parser to read the converter's HTML with: see
`pycell-test-md-program-needs-libxml' for the other direction."
  (skip-unless (fboundp 'libxml-parse-html-region))
  (let ((overblock-md-command "definitely-not-installed-xyz"))
    (should-not (overblock-md-program)))
  (let ((overblock-md-command (list "definitely-not-installed-xyz"
                                    (concat (car (split-string-shell-command
                                                  "emacs"))
                                            " --version"))))
    (should (equal (car (overblock-md-program)) "emacs"))))

(ert-deftest overblock-md-test-rendered-gives-text-not-markup ()
  "Markdown becomes text, when a converter is installed.
Pixel filling needs font metrics, which a batch session has none of."
  (skip-unless (overblock-md-program))
  (let* ((shr-use-fonts nil)
         (shr-width 60)
         (out (overblock-md-rendered "# Title\n\nSome *text* here.\n")))
    (should (string-match-p "Title" out))
    (should (string-match-p "Some text here" out))))

(ert-deftest overblock-md-test-html-batch-gives-one-piece-per-text ()
  "The cells come back from one converter call, one piece each."
  (skip-unless (overblock-md-program))
  (let ((htmls (overblock-md-html-batch '("# One\n\nfirst" "second" "*third*"))))
    (should (= (length htmls) 3))
    (should (string-match-p "first" (nth 0 htmls)))
    (should (string-match-p "second" (nth 1 htmls)))
    (should (string-match-p "third" (nth 2 htmls)))
    ;; nothing of one cell leaks into the next
    (should-not (string-match-p "second" (nth 0 htmls))))
  ;; a cell that holds the marker sends everyone the ordinary way
  (should-not (overblock-md-html-batch (list "text" overblock-md--marker))))

(ert-deftest overblock-md-test-html-batch-gives-up-when-the-marker-changes ()
  "A converter that reshapes the marker sends every cell its own way.
The batch is only safe while the pieces come back one to a cell, and
nothing but their number says whether they did."
  ;; one piece for each cell, and the pieces are the cells
  (cl-letf (((symbol-function 'overblock-md--html)
             (lambda (md) (replace-regexp-in-string
                           "\\([^\n]+\\)" "<p>\\1</p>" md))))
    (let ((pieces (overblock-md-html-batch '("one" "two"))))
      (should (= (length pieces) 2))
      (should (string-match-p "one" (nth 0 pieces)))
      (should (string-match-p "two" (nth 1 pieces)))))
  ;; and nothing at all when the marker does not come back
  (cl-letf (((symbol-function 'overblock-md--html)
             (lambda (_md) "<h1>one</h1>\n<h1>two</h1>")))
    (should-not (overblock-md-html-batch '("one" "two")))))

(ert-deftest overblock-md-test-no-previews-without-images ()
  "A display that cannot draw images gets no preview substitution.
One image in the rendered text costs the cell its piece-per-line
scrolling, and a terminal cannot even show it — so where
`display-images-p\=' says no, the fragments stay text, untouched."
  (cl-letf (((symbol-function 'display-images-p) #'ignore)
            ;; A LaTeX that would succeed, to prove it is never asked.
            ((symbol-function 'overblock-md--latex-image)
             (lambda (&rest _) (error "the terminal asked for an image"))))
    (let ((text "before $x^2$ after"))
      (should (equal (overblock-md--mathify text) text)))))

(ert-deftest overblock-md-test-verbatim-math-keeps-lines ()
  "Display math keeps its line structure, whatever the display draws.
shr fills paragraphs, so a $$ block is wrapped in <pre> before the
converter.  It is wrapped on a display that draws images as well: a
frame can draw one and still have no LaTeX to make it with, and a
fragment LaTeX cannot compile stays text anywhere."
  (let ((md "prose\n$$\na &= b \\\\\nc &= d\n$$\nmore"))
    (dolist (images (list #'ignore (lambda (&rest _) t)))
      (cl-letf (((symbol-function 'display-images-p) images))
        (should (string-search "<pre>$$\na &= b"
                               (overblock-md--verbatim-math md)))))))

(ert-deftest overblock-md-test-a-wrapped-block-still-gets-its-preview ()
  "A block that keeps its lines is still replaced by one preview.
The fragment is matched across its lines, so the wrapping in <pre>
costs the preview nothing."
  (skip-unless (overblock-md-program))
  (cl-letf (((symbol-function 'display-images-p) (lambda (&rest _) t))
            ((symbol-function 'overblock-md--latex-image)
             (lambda (&rest _) '(image :type png :data "x"))))
    (let ((rendered (overblock-md-rendered "prose\n\n$$\na = b\n$$\n")))
      (should (string-match-p "a = b" (substring-no-properties rendered)))
      ;; one image over the whole block, and the prose untouched
      (should (eq (car-safe (get-text-property
                             (string-match "\\$\\$" rendered) 'display
                             rendered))
                  'image)))))

(ert-deftest overblock-md-test-table-columns-are-literal ()
  "A rendered table aligns with real spaces, not display specs.
shr\='s `:align-to\=' counts from the line\='s visual start, and a cell is
shown indented, so only literal columns survive.  The second row\='s
cells must start where the header\='s do."
  (skip-unless (overblock-md-program))
  (let* ((rendered (overblock-md-rendered
                    "| node | form |\n|------|------|\n| X1 | h1 |\n"))
         (lines (split-string (substring-no-properties rendered) "\n"))
         (header (seq-find (lambda (l) (string-search "node" l)) lines))
         (row (seq-find (lambda (l) (string-search "X1" l)) lines)))
    (should header)
    (should row)
    (should-not (text-property-not-all 0 (length rendered)
                                       'display nil rendered))
    (should (= (string-search "form" header)
               (string-search "h1" row)))))

(ert-deftest overblock-md-test-a-header-cell-and-code-have-a-face ()
  "A header cell is bold and inline code wears the face of code.
shr has no function for a =th=, and it draws code in a fixed pitch
face, which says nothing where the rendering runs with
`shr-use-fonts\=' nil."
  (skip-unless (overblock-md-program))
  (let* ((rendered (overblock-md-rendered
                    "| head | x |\n|------|---|\n| `code_here` | y |\n"))
         (faces (lambda (word)
                  (let ((at (string-search word rendered)))
                    (and at (get-text-property at 'face rendered))))))
    (should (memq 'bold (ensure-list (funcall faces "head"))))
    (should (memq 'overblock-md-code (ensure-list (funcall faces "code_here"))))))

(ert-deftest overblock-md-test-math-in-a-table-stays-text ()
  "A formula in a table cell keeps its text, so the columns hold.
A preview image is never as wide as the text it replaces, and a table
is padded for the text.  Outside a table the same formula becomes an
image."
  (skip-unless (overblock-md-program))
  (cl-letf (((symbol-function 'display-images-p) (lambda (&rest _) t))
            ((symbol-function 'overblock-md--latex-image)
             (lambda (_frag) '(image :type png :data "x"))))
    ;; A formula the converter cannot render itself is the one that
    ;; reaches this package: pandoc renders simple math as text.
    (let ((in-table (overblock-md-rendered
                     "| a | b |\n|---|---|\n| $\\frac{a}{b}$ | y |\n"))
          (outside (overblock-md-rendered
                    "The formula $\\frac{a}{b}$ stands alone.\n")))
      (should-not (overblock-image-in in-table))
      (should (overblock-image-in outside)))))

(provide 'overblock-md-test)
;;; overblock-md-test.el ends here
