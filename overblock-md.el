;;; overblock-md.el --- Markdown rendered for a block  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Marcel Arpogaus

;; Author: Marcel Arpogaus <znepry.necbtnhf@tznvy.pbz>
;; Assisted-by: Claude:claude-opus-5
;; Assisted-by: Claude:claude-fable-5
;; Keywords: convenience, tools
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

;; Markdown in, one propertized string out:
;;
;;     (overblock-md-rendered "# A heading\n\nwith $x^2$ in it.")
;;
;; An external program turns the markdown into HTML, shr renders the
;; HTML, and LaTeX fragments become preview images by way of org.  What
;; comes back is a string that a block can show, and nothing here shows
;; anything itself.
;;
;; A rendered table is laid out in characters rather than pixels, so its
;; columns line up over the fixed-pitch lines of a buffer, and a local
;; image is drawn on the spot rather than fetched.
;;
;; Rendering a whole buffer of cells calls the program once, with
;; `overblock-md-html-batch'.

;;; Code:

(require 'overblock)
;; shr renders the HTML and dom reads the tags out of it: this file is
;; the markdown renderer and has no use without them.
(require 'shr)
;; `url-copy-file' fetches the image a cell names by URL.
(require 'url-handlers)
(require 'dom)
;; `xdg-cache-home' is where the LaTeX previews and the fetched images
;; live.  It reads XDG_CACHE_HOME as the specification says to, which a
;; bare `getenv' does not: a relative value there names no directory.
(require 'xdg)
(require 'seq)
(require 'subr-x)

;; Org supplies the LaTeX preview machinery.  It is loaded on demand, in
;; `overblock-md--latex-image', so the symbols are declared rather than
;; required.
(declare-function org-create-formula-image "org"
                  (string tofile options buffer &optional type))
(declare-function org-combine-plists "org-macs" (&rest plists))
(defvar org-preview-latex-default-process)
(defvar org-preview-latex-process-alist)
(defvar org-format-latex-options)

;; New in Emacs 31, and bound while shr renders: declared so the file
;; compiles on an Emacs that has never heard of it.
(defvar shr-sliced-image-height)

;; shr parses the converter's HTML with this, and an Emacs built
;; without libxml2 does not have it; `overblock-md-program' answers nil
;; there and no markdown cell is rendered at all.  Declared so the
;; file still compiles on such a build.
(declare-function libxml-parse-html-region "ext:xml.c"
                  (start end &optional base-url discard-comments))

(defgroup overblock-md nil
  "Markdown rendered for a block."
  :group 'overblock
  :prefix "overblock-md-")

(defface overblock-md-code '((t :inherit font-lock-constant-face))
  "Face for inline code in a rendered markdown cell.
shr draws code in a fixed pitch, and a rendered cell hangs on the
lines of a Python buffer, which is fixed pitch throughout: a pitch
says nothing there, so this face says it with a color.")

(defcustom overblock-md-command
  ;; In the order of what they can do, and each one told to leave the
  ;; math alone.  Measured: `pandoc' on its own renders a formula itself
  ;; — "$x_1 \\to x_2$" comes back as markup for "x1 → x2" — so nothing
  ;; reaches the preview machinery and a notebook loses every inline
  ;; formula; with `--mathjax' the fragment is passed through as
  ;; "\\(x_1 \\to x_2\\)", which `overblock-md--math-regexp' reads.
  ;; `markdown_py' without its extensions turns a table into one line of
  ;; pipes and a fenced block into one line of words, and `cmark' and
  ;; Perl `markdown' can do neither at all.
  ;;
  ;; `--no-highlight' because nothing here reads what the highlighting
  ;; says: shr knows no CSS class, so the colours pandoc encodes in
  ;; them are dropped and only the line anchors it hangs on every row
  ;; survive, as links to nowhere.  Measured on a document of three
  ;; fenced blocks, pandoc spent 785 milliseconds of which 720 were
  ;; the syntax definitions it loaded to paint them.
  '("pandoc --mathjax --no-highlight -f markdown-implicit_figures"
    "markdown_py -x tables -x fenced_code"
    "cmark-gfm -e table" "markdown" "cmark")
  "How to turn Markdown into HTML.
Either one shell command as a string, or a list of candidates, of
which the first one found in the variable `exec-path' is used.  The
program reads Markdown on standard input and writes HTML on standard
output, so arguments are allowed: \"pandoc -f gfm -t html\".

Markdown cells stay plain text while no candidate is installed.

Leave the math alone when choosing arguments.  Pandoc, for one, turns
simple formulas into text on its own and passes the rest through, and
`overblock-md--mathify' then makes preview images of what is left."
  :type '(choice (string :tag "Shell command")
                 (repeat (string :tag "Candidate command")))
  :group 'overblock-md)

(defcustom overblock-md-remote-images t
  "Whether to fetch the images a markdown cell names by URL.
A cell that opens with a badge names an image on the web, as the Colab
badge of a notebook does.  shr fetches such an image
with `url-queue-retrieve', which answers long after the rendering is
over and into a buffer that is gone by then, so the badge stayed its alt
text.  With this on, the file is fetched once, kept in the cache beside
the LaTeX previews, and drawn from there like a local one; the link
around it keeps its click either way.

Nil renders such an image as its alt text and asks the network for
nothing."
  :type 'boolean
  :group 'overblock-md)

(defvar overblock-md--remote-failed (make-hash-table :test #'equal)
  "The image URLs that could not be fetched in this session.
A URL that failed is not asked for again: the cell renders on every
`pycell-md-render-all', and a notebook that opens with a badge would
otherwise wait for the network every time.")

(defun overblock-md--fetchable-p (url)
  "Return non-nil where URL is an image this session may go and get."
  (and overblock-md-remote-images
       ;; Only where an image can be drawn: a terminal shows the alt
       ;; text whatever is fetched, and a batch session — the tests
       ;; among them — must not reach the network at all.
       (display-images-p)
       (string-match-p "\\`https?://" url)
       ;; One that failed is not asked for twice in a session.
       (not (gethash url overblock-md--remote-failed))))

(defun overblock-md--cache-name (url)
  "Return the name the image at URL is cached under.
The digest of the URL, and its own extension where it has a plain one:
a name taken from the URL itself could be anything at all, and the
extension is what tells Emacs which kind of image it holds."
  (let ((extension (file-name-extension url)))
    (concat (md5 url)
            (if (and extension
                     (string-match-p "\\`[a-zA-Z0-9]+\\'" extension))
                (concat "." (downcase extension))
              ".img"))))

(defun overblock-md--remote-file (url)
  "Return the local file the image URL was fetched into, or nil.
The file is kept in the cache beside the LaTeX previews, named after the
URL, so a badge is fetched once for the session and once for the
machine.  See `overblock-md--fetchable-p' for what is fetched at all."
  (when (overblock-md--fetchable-p url)
    (let* ((dir (expand-file-name "overblock-images/" (xdg-cache-home)))
           (file (expand-file-name (overblock-md--cache-name url) dir)))
      (if (file-readable-p file)
          file
        (condition-case error
            (progn
              (make-directory dir t)
              ;; Three seconds: the fetch happens while the cell
              ;; renders, so this is time the reader waits.  A fetch
              ;; that times out leaves the alt text, and the URL is not
              ;; asked for again in this session.
              (with-timeout (3 (error "Timed out"))
                (url-copy-file url file t))
              ;; Not `image-supported-file-p': it answers from the
              ;; name, and a URL with a query string caches as
              ;; `<md5>.img'.  It said no on the fetch and the
              ;; cache-hit path above never asked, so the same badge
              ;; showed alt text once and a picture every time after.
              ;; `create-image' identifies the file from its header.
              (and (file-readable-p file) file))
          (error (puthash url t overblock-md--remote-failed)
                 (ignore-errors (delete-file file))
                 (message "overblock-md: no image from %s (%s)"
                          url (error-message-string error))
                 nil))))))

(defvar overblock-md--latex-warned nil
  "Non-nil once a failed LaTeX preview was reported in this session.")

(defvar overblock-md--latex-failed (make-hash-table :test #'equal)
  "The previews LaTeX failed to make, keyed by image file name.
What caches a preview is the image file, so a failure caches nothing
and every render of the cell runs LaTeX again for the same fragment: a
process per fragment per render, for an answer that is already known.
`overblock-md-forget-failed-previews' empties this.")

(defun overblock-md--latex-render (frag file fg)
  "Render the LaTeX fragment FRAG into FILE, drawn in the colour FG.
Nothing to do where the file is there already: the file is the cache.
A run that failed is remembered, and a fragment that failed before
signals at once rather than costing another process."
  (unless (file-exists-p file)
    ;; Asked only where the image is not there already, and keyed like
    ;; the file — by content and colour — so a theme change asks again.
    ;; A LaTeX run that failed is the one thing worth remembering: what
    ;; caches a preview is the file, so without the memo a cell costs a
    ;; process per fragment on every render.
    (when (gethash file overblock-md--latex-failed)
      (error "LaTeX failed for this fragment before"))
    (let ((dir (file-name-directory file)))
      (make-directory dir t)
      (condition-case latex
          ;; Org runs LaTeX in the directory it writes to: a LaTeX in a
          ;; container reaches the home directory, but not the host's
          ;; /tmp.
          (let ((temporary-file-directory dir))
            (org-create-formula-image
             frag file
             (org-combine-plists
              org-format-latex-options
              (list :foreground fg :background "Transparent"))
             (current-buffer)))
        (error (puthash file t overblock-md--latex-failed)
               (signal (car latex) (cdr latex)))))
    ;; Org does not always signal when its process leaves nothing
    ;; behind, and `create-image' on a name reads the name: without
    ;; this a spec pointing at no file went on the screen, and the memo
    ;; never learnt anything.
    (unless (file-exists-p file)
      (puthash file t overblock-md--latex-failed)
      (error "LaTeX produced no image"))))

(defun overblock-md--latex-image (frag)
  "Return a preview image for the LaTeX fragment FRAG, or nil.
Org's formula machinery renders it.  The cache lives under ~/.cache,
keyed by content and theme color.  Org runs LaTeX in that directory
as well: a LaTeX in a container reaches the home directory, but not
the host's /tmp."
  (when (and (require 'org nil t) (fboundp 'org-create-formula-image))
    ;; Everything below is inside the handler, the bindings included: the
    ;; contract of this function is an image or nil, and a variable org
    ;; had not defined yet would otherwise raise from the middle of
    ;; shr's rendering.
    (let* ((fg (face-attribute 'default :foreground))
           (ext (or (plist-get
                     (cdr (assq org-preview-latex-default-process
                                org-preview-latex-process-alist))
                     :image-output-type)
                    "png"))
           (dir (expand-file-name "overblock-math/" (xdg-cache-home)))
           (file (expand-file-name
                  (concat (md5 (concat fg frag)) "." ext) dir)))
      (condition-case err
          (progn
            (overblock-md--latex-render frag file fg)
            ;; Past the memo: a failure below is `create-image' or the
            ;; file system, not LaTeX, and remembering it would keep a
            ;; fragment as text with a good image sitting in the cache.
            (apply #'create-image file nil nil :ascent 'center
                   ;; Capped like the images of a result and of an
                   ;; `![](file)': a display-math block can be taller
                   ;; than the window, and a block the wheel cannot get
                   ;; past is what `overblock-image-height' exists for.
                   (when-let* ((limit (overblock-image-limit)))
                     (list :max-height limit))))
        ;; Report once: without a LaTeX installation, every fragment of
        ;; every cell would report the same thing.  Org blames its own
        ;; process alist for a LaTeX run that produced nothing, where
        ;; the reason is in the log LaTeX left in DIR — a package the
        ;; preamble asks for and the installation does not have, most
        ;; often — so the message says where to look.
        (error (unless overblock-md--latex-warned
                 (setq overblock-md--latex-warned t)
                 (message "overblock-md: no LaTeX preview (%s); \
formulas stay as text, and LaTeX left its log in %s"
                          (error-message-string err) dir))
               nil)))))

;;;###autoload
(defun overblock-md-forget-failed-previews ()
  "Ask LaTeX again for the fragments whose preview failed.
A failure is remembered for the session, so that a cell costs one LaTeX
run per fragment rather than one per render.  Install LaTeX, call this,
and render the cells again."
  (interactive)
  (clrhash overblock-md--latex-failed)
  (clrhash overblock-md--remote-failed)
  (setq overblock-md--latex-warned nil))

(defconst overblock-md--math-regexp
  (rx (or (seq "$$" (+? anychar) "$$")
          ;; No space just inside either delimiter, which is the rule
          ;; CommonMark and GitHub use: guarding the opening one alone
          ;; made a formula of the prose between two prices — "costs $100
          ;; and that one $200" — and of "`$HOME` and then `$PATH`".  The
          ;; `opt' is what keeps "$x$" matching.
          ;; `in' rather than `any': the two are one rx form, and
          ;; package-lint reads the `any' inside a `not' as the Emacs
          ;; 31.1 function of that name, while `(not (or ...))' is a
          ;; character set only from Emacs 30 — 29.1 answers "Bad
          ;; character set: space" and compiles nothing in the file.
          (seq "$" (not (in "$" space))
               (opt (*? (not (in "$" "\n"))) (not (in "$" space)))
               "$")
          (seq "\\(" (+? anychar) "\\)")
          (seq "\\[" (+? anychar) "\\]")))
  "What a LaTeX fragment looks like in rendered markdown.
Most converters leave the dollar delimiters alone.  Pandoc renders
simple formulas as text and passes the rest through, either in dollars
or, when told to use MathJax, in parentheses and brackets.")

(defun overblock-md--bare-math (frag)
  "Return FRAG with its MathJax delimiters taken off.
A fragment that stays text is read as text, and pandoc with MathJax
wrote \\(x_1\\): the parentheses say nothing to a reader.  Dollars are
how a notebook writes a formula and read as themselves, so those stay.

The text and not a display property: a piece hangs a whole row on one
display property, and display properties do not nest — a property
inside that string is never looked at.

In a table the place the delimiters held is padded with spaces.  A
table is padded to the width of its text, so a cell that lost four
characters would pull the columns of its row out of line.

A fragment with a line break in it is left alone.  Display math that
stays text keeps its rows."
  (if (or (string-search "\n" frag)
          (not (or (string-prefix-p "\\(" frag)
                   (string-prefix-p "\\[" frag))))
      frag
    (let ((bare (substring frag 2 -2)))
      (if (get-text-property 0 'overblock-md--table frag)
          (concat bare (make-string (- (length frag) (length bare)) ?\s))
        bare))))

(defun overblock-md--mathify (text)
  "Replace the LaTeX fragments in TEXT with preview images.
Only fragments the converter left behind reach this function; a
fragment that fails to render here stays plain, and so does one
inside a table \(see `overblock-md--tag-table').

Only where the display can draw an image: a preview made in a
terminal cannot be seen.  A terminal still has the delimiters taken
off, or every formula of the cell reads \\(x_1\\)."
  ;; `replace-regexp-in-string' copies its argument twice whether it
  ;; matches or not, so a search stands in front of it.  Measured over a
  ;; rendered cell of two hundred lines with no formula in it: 0.016
  ;; milliseconds guarded against 0.871 unguarded, which is the guard
  ;; earning fifty times its keep.  (Two earlier comments here had this
  ;; wrong in both directions; the numbers come from an interleaved run
  ;; on a real rendering.)
  (if (not (string-match-p "[$\\]" text))
      text
    (replace-regexp-in-string
     overblock-md--math-regexp
     (lambda (frag)
       ;; `replace-regexp-in-string' uses the match data after the
       ;; replacement function returns; rendering must not touch it.
       (save-match-data
         (if-let* (((display-images-p))
                   ((not (get-text-property 0 'overblock-md--table frag)))
                   (img (overblock-md--latex-image frag)))
             (propertize frag 'display img)
           (overblock-md--bare-math frag))))
     text t t)))

(defun overblock-md-program ()
  "Return the markdown converter as a list of program and arguments.
The first candidate of `overblock-md-command' that is installed
wins; the result is nil when none of them is, and nil as well where
this Emacs cannot read the HTML that comes back: shr parses it with
`libxml-parse-html-region', which a build without libxml2 does not
have."
  (and (fboundp 'libxml-parse-html-region)
       (seq-some (lambda (command)
                   (let ((argv (split-string-shell-command command)))
                     (and (executable-find (car argv)) argv)))
                 (ensure-list overblock-md-command))))

(defconst overblock-md--marker "overblockcellbreak8f2b1c"
  "What stands between cells when they go to the converter together.
A word of its own in a paragraph of its own: every converter passes
that through as a paragraph, where anything with markup would be
reshaped into something else.")

(defun overblock-md--html (md)
  "Return the HTML `overblock-md-command' makes of MD, or nil.
Nil where no converter is installed and nil where the one that is
exits non-zero: this is the one caller-visible answer that keeps a cell
plain text rather than raising.  The check belongs here, where the
program is called, and not in each caller — `pycell-md-render-all' is
the body of `pycell-mode', and a signal from here left the mode on with
nothing rendered and took the rest of `code-cells-mode-hook' with it.
One wrong argument in `overblock-md-command' was enough."
  (when-let* ((program (overblock-md-program)))
    (let ((errors (make-temp-file "overblock-md-stderr")))
      (unwind-protect
          (with-temp-buffer
            (insert md)
            ;; Standard error to a file of its own: pandoc warns there
            ;; about the math it will not convert, and that text would
            ;; land in the HTML.  It is also the only place the reason
            ;; for a failure lives, so a failure says its last line.
            (let ((status (apply #'call-process-region
                                 (point-min) (point-max) (car program)
                                 t (list t errors) nil (cdr program))))
              (if (eq status 0)
                  (buffer-string)
                (message "overblock-md: %s exited with status %s%s"
                         (car program) status
                         (let ((reason (with-temp-buffer
                                         (ignore-errors
                                           (insert-file-contents errors))
                                         (string-trim (buffer-string)))))
                           (if (string-empty-p reason)
                               ""
                             (concat ": " (car (last (split-string
                                                      reason "\n" t)))))))
                nil)))
        (ignore-errors (delete-file errors))))))

(defun overblock-md-html-batch (texts)
  "Return the HTML of each of TEXTS, converted in one go.
Opening a notebook renders every markdown cell, and a converter
process costs more than the markdown: 44 milliseconds a cell with
`markdown_py', which is two seconds for fifty cells and nine for two
hundred.  One process for the buffer costs that once.

Nil when the marker does not come back once between every pair of
cells, or when a cell holds it already; the caller then asks for one
call per cell, as it always did."
  (when-let* ((joined (overblock-md--batch-text texts)))
    ;; Nil where the converter is missing or failed, which is what
    ;; `overblock-md--html' answers and what this function's own
    ;; docstring promises.
    (overblock-md--batch-pieces (overblock-md--html joined) texts)))

(defun overblock-md--batch-text (texts)
  "Return TEXTS joined for one call of the converter, or nil.
Nil where a text holds the marker that tells them apart, which is what
`overblock-md-html-batch\' answers nil for."
  (unless (seq-some (lambda (text) (string-search overblock-md--marker text))
                    texts)
    (string-join (mapcar #'overblock-md--verbatim-math texts)
                 (format "\n\n%s\n\n" overblock-md--marker))))

(defun overblock-md--batch-pieces (page texts)
  "Return the HTML of each of TEXTS out of PAGE, or nil.
Nil where the marker did not come back once between every pair, which
is the one answer a caller has to be ready for."
  (when-let* ((page)
              (pieces (split-string
                       page
                       (format "<p>[ \t\n]*%s[ \t\n]*</p>"
                               overblock-md--marker))))
    (and (= (length pieces) (length texts)) pieces)))

(defun overblock-md-html-batch-async (texts callback)
  "Convert TEXTS in one process and hand the HTML of each to CALLBACK.
CALLBACK is called with the list, in the order of TEXTS, or with nil
where the converter is missing, failed, or answered without its marker
between every pair — the same answers `overblock-md-html-batch\' gives,
and a caller has to be ready for nil either way.

The point of it is that nothing waits: a buffer of doc strings costs a
process, and a process that is waited for is a frozen Emacs.  Measured
with pandoc on eight doc strings, the call took 145 milliseconds of
which the reader felt every one; asked for like this the reader feels
none, and the renderings arrive a moment later.

The process is killed where the buffer that asked dies first."
  (if-let* ((program (overblock-md-program))
            (joined (overblock-md--batch-text texts)))
      (let* ((output (generate-new-buffer " *overblock-md*"))
             (buffer (current-buffer))
             (process
              (make-process
               :name "overblock-md"
               :buffer output
               :command program
               :noquery t
               :connection-type 'pipe
               ;; Standard error to a pipe that throws it away.  Not
               ;; `:stderr nil', which mixes it into the output: pandoc
               ;; warns there about the math it leaves alone and about
               ;; the arguments it means to retire, and one such line
               ;; came back as the first paragraph of the rendering.
               :stderr (make-pipe-process
                        :name "overblock-md-stderr"
                        :buffer nil
                        :noquery t
                        :filter #'ignore
                        :sentinel #'ignore)
               :sentinel
               (lambda (process _event)
                 (unless (process-live-p process)
                   (let ((page (and (eq (process-exit-status process) 0)
                                    (with-current-buffer output
                                      (buffer-string)))))
                     (kill-buffer output)
                     (when (buffer-live-p buffer)
                       (with-current-buffer buffer
                         (funcall callback
                                  (overblock-md--batch-pieces page
                                                              texts))))))))))
        (process-send-string process joined)
        (process-send-eof process)
        process)
    (funcall callback nil)
    nil))

(defun overblock-md--verbatim-math (md)
  "Return MD with its display-math blocks wrapped in <pre>.
A $$ block carries its line structure on purpose, one equation to a
line, and shr fills a paragraph: math that stays text comes back as
one rewrapped soup.  <pre> passes through every converter as raw HTML
and shr keeps its lines.

Whatever the display can draw, because a fragment stays text for more
reasons than that: a display can draw images and still have no LaTeX
to make one with, and a fragment LaTeX cannot compile stays text on
any display.  The wrapping costs a preview nothing, since the block is
matched across its lines and replaced whole."
  ;; A cell without display math is the common one, and the replacement
  ;; would copy it twice to find that out.
  (if (not (string-search "$$" md))
      md
    (replace-regexp-in-string
     "^\\$\\$\n\\(\\(?:.*\n\\)*?\\)\\$\\$$"
     "<pre>$$\n\\1$$</pre>"
     md)))

(defun overblock-md--tag-table (dom)
  "Render the table DOM and mark the text it covers.
`overblock-md--mathify' leaves marked text alone.  A table is padded to
the width of its text, and a preview image is never as wide as the
text it replaces, so a formula in a cell would pull the columns of its
row out of line."
  (let ((start (point)))
    (shr-tag-table dom)
    (put-text-property start (point) 'overblock-md--table t)))

(defun overblock-md--image-file (src)
  "Return the readable local image file that SRC names, or nil.
A markdown cell writes `![a figure](figure.png)', and a path like that
belongs to the directory of the notebook.  An absolute path and a
`file://' URL name the file directly; anything with another scheme is
not ours to open."
  (when-let* ((path (cond ((string-prefix-p "file://" src)
                           (url-unhex-string (substring src 7)))
                          ((not (string-match-p "\\`[a-zA-Z][a-zA-Z0-9+.-]*:"
                                                src))
                           src)))
              ((not (string-empty-p path)))
              (file (expand-file-name path))
              ((file-readable-p file))
              ((image-supported-file-p file)))
    file))

(defun overblock-md--tag-list (dom)
  "Render the list DOM, and open a paragraph only where no list is open.
A list nested in a list is one list to the reader, and shr opens a
paragraph — a blank line — before and after every list: five source
lines of a list with two nested items came back as seven, in three
groups the writer never wrote, and the rendering stood taller than the
text it covers.  `shr-list-mode' is what says a list is open already.

Outside a list, shr's own handler and its blank lines: a list wants
air between itself and the prose around it."
  (let ((ordered (eq (dom-tag dom) 'ol)))
    (if (not shr-list-mode)
        (if ordered (shr-tag-ol dom) (shr-tag-ul dom))
      (shr-ensure-newline)
      (let ((shr-list-mode
             (if ordered
                 (max 1 (string-to-number (or (dom-attr dom 'start) "1")))
               'ul)))
        (shr-generic dom))
      (shr-ensure-newline))))

(defun overblock-md--tag-dd (dom)
  "Render the definition DOM under its term, with no blank line between.
Pandoc wraps the description of a definition in a paragraph and shr
opens a paragraph with a blank line, so every entry of a numpydoc
section came out three lines where its source is two: a rendered doc
string stood half again as tall as the one the writer wrote, and the
code below it was pushed that far down the screen.  Only the first
paragraph is unwrapped — a description of two paragraphs still reads as
two.

Otherwise `shr-tag-dd', whose indentation this keeps: the description
stands four columns in from its term, which is where numpydoc puts it
in the source as well."
  (shr-ensure-newline)
  (let ((shr-indentation (+ shr-indentation
                            (* 4 shr-table-separator-pixel-width)))
        (opening t))
    (dolist (child (dom-children dom))
      (cond ((stringp child) (shr-insert child))
            ((and opening (eq (dom-tag child) 'p))
             (setq opening nil)
             (shr-generic child))
            (t (shr-descend child))))))

(defun overblock-md--tag-img (dom)
  "Draw the image DOM names when it is a file, and leave the rest to shr.
shr fetches an image with `url-queue-retrieve', which answers long
after the cell is rendered, so the rendering keeps the grey placeholder
that shr leaves in the meantime: measured with a relative path, an
absolute one and a `file://' URL alike, every local image stayed a
placeholder.  A file on disk needs no fetching.

The alt text carries the image, and the figure is capped like a
result's.  Where the alt text is empty the file's name stands in, so a
display that draws no image still says which figure is there; a terminal
gets that label with no display property at all, since shr's own
placeholder is an image and would swallow it."
  (let* ((src (or (dom-attr dom 'src) ""))
         (alt (dom-attr dom 'alt))
         (file (or (overblock-md--image-file src)
                   (overblock-md--remote-file src))))
    (cond
     (file
      (let ((label (if (and alt (not (string-empty-p alt)))
                       alt
                     (format "[%s]" (file-name-nondirectory file))))
            (limit (overblock-image-limit)))
        (insert (if (display-images-p)
                    (propertize label 'display
                                (apply #'create-image file nil nil
                                       (and limit (list :max-height limit))))
                  ;; A terminal draws no image, and shr's placeholder is
                  ;; itself an image: its display property would swallow
                  ;; the label under it and leave a blank row.
                  label))))
     ;; A remote image this package did not fetch — the option is off,
     ;; the display draws none, or the fetch failed before — stays its
     ;; alt text.  Not handed to shr: `shr-tag-img' fetches it with
     ;; `url-queue-retrieve' whatever this package decided, so the
     ;; option that says to ask the network for nothing asked anyway,
     ;; and the answer came long after the cell was rendered.
     ((string-match-p "\\`https?://" src)
      (insert (or alt "")))
     (t (shr-tag-img dom)))))

(defun overblock-md--shown-p (pos)
  "Return non-nil where the text at POS is worth showing to a reader.
Two markups say the same thing twice.  A run the mode marked
`invisible\' is markup a markdown mode has already replaced with a
face — that is how eglot renders documentation, and what it hides is
the asterisks and the backticks.  The adornment under a
reStructuredText section is the other: the face on the title says it is
a title, and the row of dashes under it says it again."
  (and (not (get-text-property pos 'invisible))
       (not (memq 'rst-adornment
                  (ensure-list (get-text-property pos 'face))))))

(defun overblock-md-fontified (text mode)
  "Return TEXT fontified by MODE, as a rendering of its markup.
No process: MODE\'s own font lock is the renderer, which is how eldoc
shows what a language server sends it — see `eglot--format-markup\'.
`rst-mode\' knows a reStructuredText section and `gfm-view-mode\' the
markup of markdown, and each leaves the text where the writer put it,
so the rendering is exactly as tall as the source and no code below it
moves.

What a converter does better: it knows the markup it is given, lays a
table out in columns and fills a paragraph to the window.  What this
does better: it costs no process and keeps the lines as written."
  (with-temp-buffer
    (setq-local markdown-fontify-code-blocks-natively t)
    (insert text)
    (let ((inhibit-message t)
          (message-log-max nil))
      (ignore-errors (delay-mode-hooks (funcall mode)))
      (font-lock-ensure))
    (let ((pos (point-min))
          (pieces nil))
      (while (< pos (point-max))
        (let ((next (or (next-property-change pos) (point-max))))
          (when (overblock-md--shown-p pos)
            (push (buffer-substring pos next) pieces))
          (setq pos next)))
      ;; The dropped adornment leaves the newline that followed it, and
      ;; two newlines in a row read as a blank line the writer did not
      ;; put there.
      (replace-regexp-in-string
       "\n\n\n+" "\n\n" (apply #'concat (nreverse pieces))))))

(defun overblock-md-rendered (md &optional html)
  "Render the markdown MD to a propertized string.
`overblock-md-command' produces HTML, shr renders it, and LaTeX
fragments become preview images.  With HTML, that is rendered instead
and MD is not converted again: `overblock-md-html-batch' converts a
whole buffer of cells at once.

shr renders without its font arithmetic here: a cell's text hangs on
source lines at whatever indent the buffer wears, and only literal
columns survive a move.  The `:align-to' specs shr leaves behind are
flattened to real spaces for the same reason.

The answer is nil where no converter is installed and no HTML is given.
A caller leaves the markdown as it stands then, which is what a reader
without a converter has to see."
  ;; Only the converter's absence answers nil.  An empty cell converts to
  ;; empty HTML, which parses to no document at all, and shr renders that
  ;; as the empty string — a cell with a bar and nothing under it, which
  ;; is what an empty cell has to be.
  (when-let* ((page (or html (overblock-md--html
                              (overblock-md--verbatim-math md)))))
    (let ((dom (with-temp-buffer
                 (insert page)
                 (libxml-parse-html-region (point-min) (point-max))))
          (shr-use-fonts nil)
          ;; Emacs 31 slices an image taller than this into a row for
          ;; each line of the window it is drawn in.  There is no window
          ;; here — the rendering happens in a temporary buffer and is
          ;; laid out over source lines afterwards — and a slice is
          ;; measured against one, so the images come whole and
          ;; `overblock-image-limit' caps them as it always did.
          (shr-sliced-image-height nil)
          ;; shr has no function for a =th=, so a header cell reads
          ;; like any other row, and it draws code in a fixed pitch
          ;; face, which says nothing in a buffer that is fixed pitch
          ;; throughout: code came out as prose.
          (shr-external-rendering-functions
           `((th . ,(lambda (dom) (shr-fontize-dom dom 'bold)))
             (code . ,(lambda (dom)
                        (shr-fontize-dom dom 'overblock-md-code)))
             (dd . overblock-md--tag-dd)
             (ul . overblock-md--tag-list)
             (ol . overblock-md--tag-list)
             (img . overblock-md--tag-img)
             (table . overblock-md--tag-table)
             ,@shr-external-rendering-functions)))
      (with-temp-buffer
        (shr-insert-document dom)
        (overblock--flatten-alignment)
        ;; Trim whole blank lines, never a first line's indent: the
        ;; columns are literal now, and a table that starts the cell
        ;; must keep the indent its sister rows have.
        ;; Capped here as well as in a result: shr draws a `data:' or a
        ;; `cid:' image itself, and it knows nothing of
        ;; `overblock-image-height' — such a figure came back at its own
        ;; height, which is the block the wheel cannot get past.
        (overblock-image-cap
         (overblock-md--mathify
          (string-trim (buffer-string) "\\(?:[ \t]*\n\\)+")))))))

(provide 'overblock-md)
;;; overblock-md.el ends here
