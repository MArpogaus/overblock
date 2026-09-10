;;; overblock-md-test.el --- Tests for overblock-md -*- lexical-binding: t; -*-

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

;; Run with: make test
;;
;; Markdown in, one propertized string out: the converter, the math
;; previews, the tables and the images.

;;; Code:

(require 'cl-lib)
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
  `(let* ((dir (make-temp-file "overblock-img" t))
          (,name (expand-file-name "figure.png" dir)))
     (unwind-protect
         (progn
           (let ((coding-system-for-write 'no-conversion))
             (write-region overblock-md-test--png nil ,name nil 'quiet))
           ,@body)
       (delete-directory dir t))))

(defun overblock-md-test--math (text)
  "Return TEXT with its LaTeX fragments taken out and put back.
The renderer takes the fragments out of the parsed document and puts
them back once shr has laid it out; this does both to a string, which
is what the fragments themselves see."
  (overblock-md--unstow-math (overblock-md--stow-in-string text)))

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
shr's own placeholder is an image, and its display property swallows the
text under it: a terminal would show a blank row where a figure belongs.

The label is drawn for a file this Emacs knows how to read, and
`overblock-md--image-file' answers nil for every path where it knows
none -- so an Emacs built without image support has nothing to label."
  (skip-unless (overblock-md-program))
  (skip-unless (image-type-available-p 'png))
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
shr fetches an image through `url-queue-retrieve', which answers after
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
`overblock-fill-props' leaves the properties shr gave the link alone."
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
  "An image on the network is shr's business where the package leaves it.
`overblock-md-remote-images' is off here and shr is told to fetch
nothing: a test asks the network for nothing at all."
  (skip-unless (overblock-md-program))
  ;; A display that draws images, or the option under test says nothing:
  ;; `overblock-md--remote-file' answers nil in a batch session whatever
  ;; the option is, and the test then passed with the option on.
  (let ((fetches 0))
    (cl-letf (((symbol-function 'display-images-p) (lambda (&rest _) t))
              ((symbol-function 'url-copy-file)
               (lambda (&rest _) (setq fetches (1+ fetches)))))
      (let* ((overblock-md-remote-images nil)
             (shr-blocked-images ".")
             (shown (overblock-md-rendered
                     "![a figure](https://example.org/f.png)"))
             (spec (overblock-image-in shown)))
        ;; nothing was asked of the network, which is the option's whole
        ;; promise
        (should (= fetches 0))
        ;; shr leaves a placeholder of its own making, and it is not a file
        (should (or (null spec) (null (plist-get (cdr spec) :file))))))))

(ert-deftest overblock-md-test-a-remote-image-is-fetched-once ()
  "An image named by URL is fetched once and drawn from the file.
A badge in a link — the Colab badge of a notebook — stayed its alt text,
because shr fetches with `url-queue-retrieve' and answers into a buffer
that the rendering has already left."
  (let* ((cache (make-temp-file "overblock-images" t))
         (process-environment (cons (concat "XDG_CACHE_HOME=" cache)
                                    process-environment))
         (overblock-md-remote-images t)
         (overblock-md--remote-failed (make-hash-table :test #'equal))
         (url "https://example.org/badge.svg")
         (fetches 0))
    (unwind-protect
        (cl-letf (((symbol-function 'display-images-p) (lambda (&rest _) t))
                  ((symbol-function 'image-supported-file-p) (lambda (_) t))
                  ((symbol-function 'url-copy-file)
                   (lambda (_url file &rest _)
                     (setq fetches (1+ fetches))
                     (with-temp-file file (insert "not really a png")))))
          (let ((first (overblock-md--remote-file url)))
            (should first)
            (should (file-readable-p first))
            (should (equal (overblock-md--remote-file url) first))
            ;; Once for the session and once for the machine.
            (should (= fetches 1))))
      (delete-directory cache t))))

(ert-deftest overblock-md-test-a-remote-image-that-fails-is-not-asked-again ()
  "A URL that could not be fetched is left alone for the session."
  (let* ((cache (make-temp-file "overblock-images" t))
         (process-environment (cons (concat "XDG_CACHE_HOME=" cache)
                                    process-environment))
         (overblock-md-remote-images t)
         (overblock-md--remote-failed (make-hash-table :test #'equal))
         (fetches 0))
    (unwind-protect
        (cl-letf (((symbol-function 'display-images-p) (lambda (&rest _) t))
                  ((symbol-function 'url-copy-file)
                   (lambda (&rest _) (setq fetches (1+ fetches))
                     (error "No network"))))
          (should-not (overblock-md--remote-file "https://example.org/x.png"))
          (should-not (overblock-md--remote-file "https://example.org/x.png"))
          (should (= fetches 1)))
      (delete-directory cache t))))

(ert-deftest overblock-md-test-no-parser-no-program ()
  "Without the parser there is no converter worth naming.
shr reads the converter's HTML with `libxml-parse-html-region', which
an Emacs built without libxml2 does not have.  Rendering signalled a
void function there instead of answering nil, so a caller could not
leave the text plain and say which part was missing."
  (cl-letf (((symbol-function 'libxml-parse-html-region) nil))
    (should-not (fboundp 'libxml-parse-html-region))
    (should-not (overblock-md-program))
    (should-not (overblock-md-rendered "# heading"))))

(ert-deftest overblock-md-test-program-takes-a-string-or-a-list ()
  "A string and a list of candidates both resolve to a program.
Only where there is a parser to read the converter's HTML with: see
`overblock-md-test-no-parser-no-program' for the other direction."
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

(ert-deftest overblock-md-test-a-warning-stays-on-standard-error ()
  "What the converter writes on standard error is not part of the HTML.
`:stderr nil' mixes standard error into the output rather than
throwing it away, and pandoc writes there: it warns about the math it
leaves alone and about the arguments it means to retire, and one such
line came back as the first paragraph of every rendering."
  (skip-unless (executable-find "sh"))
  (let ((overblock-md-command "sh -c 'echo [WARNING] noise >&2; cat'")
        (answered 'not-yet))
    (overblock-md-html-batch-async
     '("<p>the whole answer</p>")
     (lambda (htmls) (setq answered htmls)))
    (let ((deadline (+ (float-time) 10)))
      (while (and (eq answered 'not-yet) (< (float-time) deadline))
        (accept-process-output nil 0.05)))
    (should (equal answered '("<p>the whole answer</p>")))))

(ert-deftest overblock-md-test-a-nested-list-is-one-list ()
  "A list with a nested one in it renders as tall as its source.
shr opens a paragraph — a blank line — before and after every list,
and a nested list came back in three groups the writer never wrote."
  (skip-unless (overblock-md-program))
  (let* ((shr-width 60)
         (source (concat "- a first item\n- a second item\n"
                         "  - a nested item\n  - and another\n"
                         "- a third item\n"))
         (shown (substring-no-properties (overblock-md-rendered source))))
    (should (string-match-p
             "a second item *\n +. a nested item\n +. and another\n. a third"
             shown))
    (should (<= (length (split-string shown "\n"))
                (length (split-string source "\n"))))))

(ert-deftest overblock-md-test-a-definition-stands-under-its-term ()
  "A definition renders under its term, with no blank line between.
Which is what a numpydoc section is made of, and what keeps a rendered
doc string from standing taller than the source it covers: pandoc
wraps every description in a paragraph, and a paragraph opens with a
blank line."
  (skip-unless (executable-find "pandoc"))
  (let* ((overblock-md-command "pandoc --mathjax --no-highlight -f rst")
         (shr-width 60)
         (source (concat "Parameters\n----------\n"
                         "xs : list of float\n    the values to measure\n"
                         "unit : str\n    the unit to answer in\n"))
         (shown (substring-no-properties (overblock-md-rendered source))))
    (should (string-match-p
             "xs : list of float\n +the values to measure *\nunit : str"
             shown))
    ;; and the whole of it fits in the lines it stands on
    (should (<= (length (split-string shown "\n"))
                (length (split-string source "\n"))))))

(ert-deftest overblock-md-test-the-converter-paints-no-code ()
  "Every pandoc the defaults name is told to leave the code alone.
shr reads no CSS class, so the colours pandoc encodes in them are
dropped and only the line anchors it hangs on every row survive.
Measured on a document of three fenced blocks, pandoc spent 785
milliseconds of which 720 were the syntax definitions it loaded to
paint them."
  (dolist (command (ensure-list overblock-md-command))
    (when (string-prefix-p "pandoc" command)
      (should (string-search "--no-highlight" command)))))

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
`display-images-p' says no, the fragments stay text, untouched."
  (cl-letf (((symbol-function 'display-images-p) #'ignore)
            ;; A LaTeX that would succeed, to prove it is never asked.
            ((symbol-function 'overblock-md--latex-image)
             (lambda (&rest _) (error "The terminal asked for an image"))))
    (let ((text "before $x^2$ after"))
      (should (equal (overblock-md-test--math text) text)))
    ;; and the MathJax delimiters still come off, or the terminal reads
    ;; every formula of the cell as \\(x_1\\)
    (should (equal (overblock-md-test--math "before \\(x^2\\) after")
                   "before x^2 after"))))

(ert-deftest overblock-md-test-a-fenced-block-wears-the-faces-of-its-language ()
  "A block that opens with ```python wears the faces of `python-mode'.
A block without a language wears none of them.
The language stands in the class of the <pre> or its <code>, however
the converter spells it, and `overblock-md-code-modes' set to nil turns
the painting off."
  (skip-unless (overblock-md-program))
  (let* ((md "```python\ndef f():\n    pass\n```\n\n```\nplain\n```\n")
         (faces (lambda (rendered word)
                  (ensure-list (get-text-property (string-search word rendered)
                                                  'face rendered)))))
    (should (memq 'font-lock-keyword-face
                  (funcall faces (overblock-md-rendered md) "def")))
    (should-not (memq 'font-lock-keyword-face
                      (funcall faces (overblock-md-rendered md) "plain")))
    (let ((overblock-md-code-modes nil))
      (should-not (memq 'font-lock-keyword-face
                        (funcall faces (overblock-md-rendered md) "def"))))
    ;; the three spellings of a language class
    (dolist (html '("<pre class=\"python\"><code>x</code></pre>"
                    "<pre><code class=\"language-python\">x</code></pre>"
                    "<pre class=\"sourceCode python\"><code class=\"sourceCode python\">x</code></pre>"))
      (should (eq (overblock-md--code-mode
                   (with-temp-buffer
                     (insert html)
                     (dom-child-by-tag
                      (dom-child-by-tag
                       (libxml-parse-html-region (point-min) (point-max)) 'body)
                      'pre)))
                  (alist-get 'python-mode major-mode-remap-alist 'python-mode))))
    (should-not (overblock-md--code-mode
                 (with-temp-buffer
                   (insert "<pre class=\"nosuchlanguage\"><code>x</code></pre>")
                   (dom-child-by-tag
                    (dom-child-by-tag
                     (libxml-parse-html-region (point-min) (point-max)) 'body)
                    'pre))))))

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
  "A block that keeps its lines is replaced by one preview, drawn once.
The fragment is matched across its lines, so the wrapping in <pre>
costs the preview nothing — and the image takes one row, not the rows
the source had: a display property is drawn once for every screen line
its run reaches, and the rows left under a figure stood empty.
Measured, two blank lines after every displayed formula."
  (skip-unless (overblock-md-program))
  (cl-letf (((symbol-function 'display-images-p) (lambda (&rest _) t))
            ((symbol-function 'overblock-md--latex-image)
             (lambda (&rest _) '(image :type png :data "x"))))
    (let* ((rendered (overblock-md-rendered "prose\n\n$$\na = b\n$$\n"))
           (runs 0)
           (pos 0))
      (while (< pos (length rendered))
        (let ((next (or (next-single-property-change pos 'display rendered)
                        (length rendered))))
          (when (eq (car-safe (get-text-property pos 'display rendered))
                    'image)
            (setq runs (1+ runs)))
          (setq pos next)))
      ;; one image, and the prose beside it untouched
      (should (= runs 1))
      (should (string-match-p "prose" (substring-no-properties rendered)))
      ;; the formula is one row, and the rows under it are gone
      (should (= (length (split-string rendered "\n")) 3)))))

(ert-deftest overblock-md-test-table-columns-are-literal ()
  "A rendered table aligns with real spaces, not display specs.
shr's `:align-to' counts from the line's visual start, and a cell is
shown indented, so only literal columns survive.  The second row's
cells must start where the header's do."
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
`shr-use-fonts' nil."
  (skip-unless (overblock-md-program))
  (let* ((rendered (overblock-md-rendered
                    "| head | x |\n|------|---|\n| `code_here` | y |\n"))
         (faces (lambda (word)
                  (let ((at (string-search word rendered)))
                    (and at (get-text-property at 'face rendered))))))
    (should (memq 'bold (ensure-list (funcall faces "head"))))
    (should (memq 'overblock-md-code (ensure-list (funcall faces "code_here"))))))

(ert-deftest overblock-md-test-a-wrapped-formula-keeps-its-row ()
  "The converter's own line break inside a formula is no row of the rendering.
pandoc wraps its HTML at 72 columns, in the middle of a formula where
that is where the column falls, and the fragment then carries a newline
of its own.  Hung on the fragment as it came, the image ended the row
there and the full stop after the formula stood on a row of its own."
  (skip-unless (overblock-md-program))
  (cl-letf (((symbol-function 'display-images-p) (lambda (&rest _) t))
            ((symbol-function 'overblock-md--latex-image)
             (lambda (&rest _) '(image :type png :data "x"))))
    (let* ((overblock-md-width 200)
           (rendered (overblock-md-rendered
                      "The cells are comments, so the file runs as a \
script as well, and a markdown cell renders its math: \
$\\sin^2 x + \\cos^2 x = 1$."))
           (image (text-property-any 0 (length rendered) 'display
                                     (get-text-property
                                      (or (text-property-not-all
                                           0 (length rendered) 'display nil
                                           rendered)
                                          0)
                                      'display rendered)
                                     rendered))
           (after (next-single-property-change image 'display rendered)))
      (should image)
      ;; the full stop follows the image on its row
      (should (eq (aref rendered after) ?.)))))

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

(ert-deftest overblock-md-test-a-preview-is-asked-for-and-arrives-later ()
  "A fragment without a preview is asked for, and shown as text meanwhile.
Nothing waits for LaTeX: the engine answers nil for an equation it has
not compiled and calls back when it has, and the caller reads that as
`pending\' and shows the fragment as text until then.  The buffer the
rendering is for is carried into the callback, because the engine calls
it from a sentinel where the current buffer is its own."
  (let* ((buffer (generate-new-buffer "overblock-md-preview"))
         (overblock-md--buffer buffer)
         (overblock-md--latex-arrivals nil)
         (overblock-md--latex-arrival-timer nil)
         (asked nil)
         (callback nil))
    (unwind-protect
        (cl-letf (((symbol-function 'latex-to-svg-backend-available-p)
                   (lambda () t))
                  ((symbol-function 'run-with-idle-timer)
                   (lambda (&rest _) 'timer))
                  ((symbol-function 'timerp) (lambda (x) (eq x 'timer)))
                  ((symbol-function 'latex-to-svg-backend)
                   (lambda (latex &rest keys)
                     (push latex asked)
                     (setq callback (plist-get keys :callback))
                     ;; the colour and the font of the buffer that asked,
                     ;; and not of the temporary one shr renders in
                     (should (equal (plist-get keys :color)
                                    (with-current-buffer buffer
                                      (face-attribute 'default :foreground
                                                      nil t))))
                     nil)))
          ;; no image yet: the fragment stands as text and a callback is left
          (should (eq (overblock-md--latex-image "$x$") 'pending))
          (should (equal asked '("$x$")))
          (should (functionp callback))
          ;; the callback notes the buffer rather than drawing there and then
          (funcall callback)
          (should (equal overblock-md--latex-arrivals (list buffer)))
          ;; and an image, once there is one, comes back capped
          (cl-letf (((symbol-function 'latex-to-svg-backend)
                     (lambda (&rest _) '(image :type svg :data "x")))
                    ((symbol-function 'overblock-image-limit) (lambda () 40)))
            (let ((image (overblock-md--latex-image "$x$")))
              (should (eq (car image) 'image))
              (should (= (plist-get (cdr image) :max-height) 40)))))
      (kill-buffer buffer))))

(ert-deftest overblock-md-test-no-engine-no-preview ()
  "Where equations cannot be drawn at all, a fragment stays text.
A terminal and an Emacs without SVG both answer so, and neither is a
failure to report: the reader sees the formula as it was written."
  (cl-letf (((symbol-function 'latex-to-svg-backend-available-p) #'ignore))
    (should-not (overblock-md--latex-image "$x$"))))

(ert-deftest overblock-md-test-a-price-is-not-a-formula ()
  "Two prices in a sentence are not a LaTeX fragment.
The pattern guarded the opening delimiter and not the closing one, so
\"costs $100 and that one $200\" made a formula of the prose between
them, and so did \"`$HOME` and then `$PATH`\"."
  (dolist (text '("This item costs $100 and that one $200 today."
                  "Set $HOME and then $PATH for the run."
                  "A $ on its own and another $ later."))
    (should-not (string-match-p overblock-md--math-regexp text)))
  ;; And what is a formula still is one.
  (dolist (text '("$x$" "$x^2$" "$a + b$" "$\\frac{a}{b}$" "$ab$$cd$"
                  "$$\na = b\n$$"))
    (should (string-match-p overblock-md--math-regexp text))))

(ert-deftest overblock-md-test-a-converter-that-fails-keeps-the-cell-plain ()
  "A converter that exits non-zero answers nil rather than raising.
A caller renders from the body of a minor mode, so a signal here left
the mode on with nothing rendered and took the rest of the hook that
turned it on with it."
  (let ((overblock-md-command "false"))
    (should-not (overblock-md--html "# heading"))
    (should-not (overblock-md-rendered "# heading"))
    (should-not (overblock-md-html-batch '("a" "b"))))
  ;; And with no converter at all, which has always answered nil.
  (let ((overblock-md-command "there-is-no-such-program-here"))
    (should-not (overblock-md--html "# heading"))
    (should-not (overblock-md-html-batch '("a" "b")))))

(ert-deftest overblock-md-test-a-link-keeps-its-keymap-through-fill-props ()
  "A link in a rendered cell keeps its own keymap when the block fills one.
The block gives every row its keymap with `overblock-fill-props', which
must leave what shr put on the link alone — this test used to assert on
shr's own output and never called the function it names."
  (skip-unless (overblock-md-program))
  (let* ((shown (overblock-md-rendered "[text](https://example.org/)"))
         (pos (and shown (text-property-not-all 0 (length shown) 'keymap nil
                                                shown))))
    (skip-unless pos)
    (overblock-fill-props shown 'keymap (define-keymap "RET" #'ignore))
    (should (eq (keymap-lookup (get-text-property pos 'keymap shown) "RET")
                #'shr-browse-url))))

(ert-deftest overblock-md-test-math-that-stays-text-loses-its-braces ()
  "A fragment no image was made for shows without MathJax delimiters.
A formula in a table stays text, and pandoc with MathJax writes
\\(x_1\\): the parentheses are noise on the screen.  The place they held
is padded, because a table is padded to the width of its text and a
narrower cell would pull the columns of its row out of line.

The text itself, not a display property: a piece hangs its whole row on
one display property, and a display property inside a display string is
never looked at."
  (cl-letf (((symbol-function 'display-images-p) (lambda (&rest _) t))
            ((symbol-function 'overblock-md--latex-image) #'ignore))
    ;; in a table, where no preview is ever made
    (let* ((cell (propertize "\\(x_1\\)  a source" 'overblock-md--table t))
           (shown (substring-no-properties (overblock-md-test--math cell))))
      (should (string-prefix-p "x_1" shown))
      (should-not (string-search "\\(" shown))
      ;; as wide as before, or the columns move
      (should (= (length shown) (length "\\(x_1\\)  a source"))))
    ;; outside a table nothing needs padding
    (should (equal (overblock-md-test--math "before \\(x^2\\) after")
                   "before x^2 after"))
    ;; dollars are how a notebook writes a formula: they stay
    (let ((text "before $x^2$ after"))
      (should (equal (overblock-md-test--math text) text)))
    ;; display math over several rows keeps them
    (let ((text "\\[\na = b\n\\]"))
      (should (equal (overblock-md-test--math text) text)))))

(ert-deftest overblock-md-test-the-theme-hooks-wait-for-a-rendering ()
  "Loading the file installs no hook; the first rendering installs two.
A package of this repository does not change how Emacs behaves by
being loaded, which the two notebook modes promise in their own
docstrings, and a buffer that renders nothing needs no theme watch."
  (let ((overblock-md--watching-themes nil)
        (enable-theme-functions nil)
        (disable-theme-functions nil))
    (should-not (memq #'overblock-md--theme-changed enable-theme-functions))
    (overblock-md--watch-themes)
    (should (memq #'overblock-md--theme-changed enable-theme-functions))
    (should (memq #'overblock-md--theme-changed disable-theme-functions))))

(ert-deftest overblock-md-test-a-theme-change-draws-the-formulas-again ()
  "A rendering that holds a preview comes down when the theme changes.
The preview is drawn in the foreground of the theme, so the one on the
screen is in the old colour until it is rendered again; a rendering
without a formula is left alone."
  (with-temp-buffer
    (setq-local overblock-live--specs (list (list 'md-preview #'ignore 0)))
    (insert "one\ntwo\n")
    (overblock-show 1 4 :kind 'md-preview
                    :over (propertize "x" 'overblock-md-math t))
    (overblock-show 5 8 :kind 'md-preview :over "plain")
    (overblock-md--theme-changed)
    (should (equal (mapcar (lambda (b) (overblock-get b :over))
                           (overblock-in (point-min) (point-max) 'md-preview))
                   '("plain")))))

(ert-deftest overblock-md-test-a-remote-image-is-not-fetched-when-off ()
  "With `overblock-md-remote-images' off, nothing reaches the network.
`shr-tag-img' fetches with `url-queue-retrieve' whatever this package
decided, so the option that says to ask for nothing asked anyway, and
the answer arrived long after the cell had been rendered."
  (let ((overblock-md-remote-images nil)
        (asked nil))
    (cl-letf (((symbol-function 'url-queue-retrieve)
               (lambda (&rest _) (setq asked t)))
              ((symbol-function 'url-retrieve)
               (lambda (&rest _) (setq asked t))))
      (with-temp-buffer
        (overblock-md--tag-img
         (dom-node 'img '((src . "https://example.org/badge.svg")
                          (alt . "the badge"))))
        (should (equal (string-trim (buffer-string)) "the badge")))
      (should-not asked))))

(provide 'overblock-md-test)
(ert-deftest overblock-md-test-a-painted-block-is-a-rectangle ()
  "A run of rows that wears a background is squared off to its longest.
shr ends a row where its text ends, so a fenced block came out as a
ragged patch of colour.  A blank line inside the block belongs to it;
the one that closes it does not."
  (skip-unless (overblock-md-program))
  (let* ((custom--inhibit-theme-enable nil)
         (rendered
          (progn
            (custom-set-faces '(overblock-md-code ((t :background "#eee"))))
            (overblock-md-rendered
             "text\n\n```python\ndef f():\n    x = 1\n\n    return x\n```\n\nend\n")))
         (lines (split-string rendered "\n"))
         (painted (seq-filter (lambda (line)
                                (and (> (length line) 0)
                                     (overblock-md--background
                                      (get-text-property 0 'face line))))
                              lines)))
    ;; the four rows of the block, the blank line among them
    (should (= (length painted) 4))
    ;; one width for all of them, and it is the longest row's
    (should (= 1 (length (seq-uniq (mapcar #'string-width painted)))))
    (should (equal (seq-map #'string-trim-right
                            (mapcar #'substring-no-properties painted))
                   '("def f():" "    x = 1" "" "    return x")))
    ;; and the rows around it are left as they are
    (should (member "text" (mapcar #'substring-no-properties lines)))
    (should (member "end" (mapcar #'substring-no-properties lines)))))

(ert-deftest overblock-md-test-a-broken-formula-shows-one-image ()
  "A formula the fill broke over two rows draws its preview once.
A display property is drawn once for every screen line its run
reaches, so an image hung on the whole fragment came out twice — at
the end of one row and again at the start of the next.  The image goes
on the part before the break, and the rest is drawn as nothing."
  (let* ((image '(image :type png :file "nowhere.png"))
         (whole (overblock-md--place-image "\\(x + y\\)" image))
         (broken (overblock-md--place-image "\\(x +\ny\\)" image)))
    ;; unbroken: the image on the whole fragment, as it always was
    (should (eq (get-text-property 0 'display whole) image))
    ;; broken: the image once, on the first row
    (should (eq (get-text-property 0 'display broken) image))
    (let ((rows (split-string broken "\n")))
      (should (= (length rows) 2))
      (should (eq (get-text-property 0 'display (nth 0 rows)) image))
      ;; the second row keeps its place and nothing else: a display
      ;; property inside a display string is never looked at, so
      ;; hiding the rest would have left the raw LaTeX on the screen
      (should (equal (nth 1 rows) "")))
    ;; the fragment that goes to LaTeX is the whole formula
    (should (equal (overblock-md--one-line "\\(x +\n  y\\)") "\\(x + y\\)"))))

(ert-deftest overblock-md-test-math-is-taken-out-of-the-parsed-document ()
  "A dollar pattern cannot span the tags of the document any more.
The fragments were cut out of the HTML before it was parsed, and the
inline pattern only forbids a space beside a delimiter, so any tag
between two dollars closed the gap: `Set `$PWD` then use `$x$`' lost
a <code> pair and handed 25 characters of prose to LaTeX.  Text nodes
end at a tag, so the pattern cannot reach across one."
  (skip-unless (overblock-md-program))
  (let ((rendered (substring-no-properties
                   (overblock-md-rendered "Set `$PWD` then use `$x$` in shell."))))
    (should-not (string-search "</code>" rendered))
    (should (string-search "$PWD" rendered))))

(ert-deftest overblock-md-test-a-fragment-shr-drops-shifts-nothing ()
  "A formula the rendering never shows takes nothing with it.
The runs of marks were paired with the fragments by counting, so a
fragment stowed out of a part shr drops — an HTML comment, the title
of a link — left no run, every later formula showed the one before
it, and the last was dropped.  Each run carries its own fragment now."
  (skip-unless (overblock-md-program))
  (cl-letf (((symbol-function 'display-images-p) #'ignore))
    (should (string-search
             "beta"
             (substring-no-properties
              (overblock-md-rendered
               "<!-- note: $\\alpha$ -->\n\nThe value $\\beta$ ends it."))))
    (should (string-search
             "m"
             (substring-no-properties
              (overblock-md-rendered
               "[docs](https://x.org \"why $n$ matters\") and $m$ here."))))
    (should-not (string-search
                 "$n$"
                 (substring-no-properties
                  (overblock-md-rendered
                   "[docs](https://x.org \"why $n$ matters\") and $m$ here."))))))

(ert-deftest overblock-md-test-a-fence-keeps-its-dollars ()
  "Display math inside a fenced block is shown, not wrapped.
`overblock-md--verbatim-math' wraps a $$ block in <pre> so shr keeps
its lines; inside a fence that wrapper reached the reader as literal
tags, in a document whose subject is display math."
  (skip-unless (overblock-md-program))
  (let ((rendered (substring-no-properties
                   (overblock-md-rendered "Example:\n\n```\n$$\na = b\n$$\n```\n"))))
    (should-not (string-search "<pre>" rendered))
    (should (string-search "$$" rendered))))

(ert-deftest overblock-md-test-two-display-formulas-both-arrive ()
  "Two display formulas with a blank line between them are two formulas.
The run of marks a fragment leaves may be broken over two rows by the
fill, so a run crosses a newline — but only with marks on both sides
of it.  The class that held the mark and the newline together was
greedy and read both formulas as one run: the second was popped off
nothing, dropped, and every fragment after it in the text came back as
the one before it."
  (skip-unless (overblock-md-program))
  (let ((rendered (overblock-md-rendered "$$a+1$$\n\n$$b+2$$")))
    (dolist (formula '("a+1" "b+2"))
      (should (string-match-p (regexp-quote formula) rendered))))
  ;; and three of them, which is where the shift showed
  (let ((rendered (overblock-md-rendered "$$a+1$$\n\n$$b+2$$\n\n$$c+3$$")))
    (dolist (formula '("a+1" "b+2" "c+3"))
      (should (string-match-p (regexp-quote formula) rendered))))
  ;; the run itself: marks on both sides of a newline are one fragment,
  ;; marks with a blank line between them are two
  (let ((mark (string overblock-md--math-mark)))
    (should (string-match-p (concat "\\`" overblock-md--math-run "\\'")
                            (concat mark mark "\n" mark)))
    (should-not (string-match-p (concat "\\`" overblock-md--math-run "\\'")
                                (concat mark "\n\n" mark)))))

(ert-deftest overblock-md-test-a-terminal-reads-inline-math-on-one-line ()
  "A display without images gets inline math joined and undelimited.
The converter wraps its own output, so a fragment carries whatever line
breaks pandoc put in it; read as text, a formula broken at a backslash
reads worse than the same formula on one line.  Display math keeps its
rows, which is what it was written for."
  (cl-letf (((symbol-function 'display-images-p) #'ignore))
    (let ((shown (overblock-md-test--math "before \\(a +\nb\\) after")))
      (should (equal (substring-no-properties shown) "before a + b after")))
    (let ((shown (overblock-md-test--math "$$\na = b\n$$")))
      ;; the rows are the rows it was written with
      (should (= (length (split-string shown "\n")) 3)))))

(ert-deftest overblock-md-test-a-fragment-is-replaced-not-doubled ()
  "Taking a formula out of the HTML removes it and leaves its marks.
Asking how wide the preview will be goes into the engine, which
searches on its own account, and `replace-regexp-in-string\' reads the
match back after the replacement returns: without `save-match-data\'
the marks were inserted and the fragment left standing beside them, so
every formula showed its image and its own LaTeX next to it."
  (cl-letf (((symbol-function 'overblock-md--math-columns)
             (lambda (frag)
               ;; what the engine does to the match data on the way
               (string-match "x+" "xxx")
               (string-width frag))))
    (let ((out (overblock-md--stow-in-string "a \\(\\varphi\\) b")))
      (should-not (string-search "varphi" out))
      (should (string-search (string overblock-md--math-mark) out))
      ;; and the fragment rides on the marks it left
      (should (equal (get-text-property
                      (string-search (string overblock-md--math-mark) out)
                      'overblock-md--frag out)
                     "\\(\\varphi\\)")))))

(ert-deftest overblock-md-test-a-mark-in-the-source-is-left-standing ()
  "A mark the writer typed carries no fragment and stands for itself.
An object replacement character is what a paste out of a word
processor brings.  Read as a stowed formula, it was dropped and the
formulas after it came back one run late."
  (skip-unless (overblock-md-program))
  (let ((rendered (overblock-md-rendered
                   (concat "a " (string overblock-md--math-mark)
                           " b $x+1$ c"))))
    (should (string-search (string overblock-md--math-mark) rendered))
    (should (string-search "x+1" rendered))
    ;; and the prose is in the order it was written in
    (should (string-match-p "a .* b .* c" rendered))))

;;; overblock-md-test.el ends here
