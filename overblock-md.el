;;; overblock-md.el --- Markdown rendered for a block  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Marcel Arpogaus

;; Author: Marcel Arpogaus <znepry.necbtnhf@tznvy.pbz>
;; Assisted-by: Claude:claude-opus-5
;; Assisted-by: Claude:claude-fable-5
;; Version: 1.0
;; Package-Requires: ((emacs "29.1") (overblock "1.0") (latex-to-svg-backend "0.8"))
;; Keywords: convenience, tools
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

;; Markdown in, one propertized string out:
;;
;;     (overblock-md-rendered "# A heading\n\nwith $x^2$ in it.")
;;
;; An external program turns the markdown into HTML, shr renders the
;; HTML, and latex-to-svg-backend turns LaTeX fragments into preview
;; images.  The result is a string that a block can show; nothing here
;; shows anything itself.
;;
;; A rendered table is laid out in characters rather than pixels, so its
;; columns line up over the fixed-pitch lines of a buffer.  A local
;; image is drawn on the spot rather than fetched.  An image named by
;; URL is fetched once into a cache, and drawn from there.
;;
;; Rendering a whole buffer of cells calls the program once, with
;; `overblock-md-html-batch-async'.

;;; Code:

(require 'overblock)
(require 'shr)
(require 'dom)
;; `xdg-cache-home', not `getenv': it ignores a relative XDG_CACHE_HOME,
;; as the specification says.
(require 'xdg)
(require 'cl-lib)
(require 'seq)
(require 'subr-x)

;; Compiles a LaTeX fragment to an image in a process of its own, once
;; per equation, and sets colour and size when the image is drawn.
(require 'latex-to-svg-backend)

;; New in Emacs 31.
(defvar shr-sliced-image-height)

;; A build without libxml2 has no `libxml-parse-html-region', and
;; `overblock-md-program' returns nil there. A build without image
;; support has no `image-size'; the calls are behind `display-images-p'.
(declare-function eldoc-print-current-symbol-info "eldoc" (&optional interactive))
(defvar eldoc--last-request-state)
(declare-function image-size "image.c" (spec &optional pixels frame))
(declare-function libxml-parse-html-region "ext:xml.c"
                  (start end &optional base-url discard-comments))

(defgroup overblock-md nil
  "Markdown rendered for a block."
  :group 'overblock
  :prefix "overblock-md-")

(defface overblock-md-code '((t :inherit font-lock-constant-face))
  "Face for inline code in a rendered markdown cell.
shr draws code in a fixed pitch, which shows nothing in a fixed pitch
buffer, so this face uses a colour.")

(defcustom overblock-md-command
  ;; Best first, each told to leave the math alone. Without `--mathjax'
  ;; pandoc renders a simple formula as markup, and nothing reaches the
  ;; previews; with it, pandoc passes "\\(x_1 \\to x_2\\)" through. Without
  ;; its extensions `markdown_py' cannot do tables or fenced blocks, and
  ;; `cmark' and Perl `markdown' cannot do them at all.
  ;;
  ;; `--no-highlight': shr reads no CSS class, so the highlighting only
  ;; costs time and leaves line anchors as dead links.
  '("pandoc --mathjax --no-highlight --wrap=none -f markdown-implicit_figures"
    "markdown_py -x tables -x fenced_code"
    "cmark-gfm -e table" "markdown" "cmark")
  "How to turn Markdown into HTML.
Either one shell command as a string, or a list of candidates, of
which the first one found in the variable `exec-path' is used.  The
program reads Markdown on standard input and writes HTML on standard
output, so arguments are allowed: \"pandoc -f gfm -t html\".

When no candidate is installed, a caller shows the markdown as it is.

Choose arguments that leave the math alone: `overblock-md--stow-math'
gives the fragments to latex-to-svg-backend.  With pandoc, keep
`--mathjax' and `--wrap=none': a wrapped line splits a long formula."
  :type '(choice (string :tag "Shell command")
                 (repeat (string :tag "Candidate command")))
  :group 'overblock-md)

(defcustom overblock-md-code-modes
  '(("elisp" . emacs-lisp-mode) ("emacs-lisp" . emacs-lisp-mode)
    ("bash" . sh-mode) ("shell" . sh-mode) ("zsh" . sh-mode)
    ("cpp" . c++-mode) ("c++" . c++-mode) ("r" . ess-r-mode)
    ("R" . ess-r-mode) ("yml" . yaml-mode))
  "The major mode that paints a fenced block, by the language it names.
A fenced block that names a language, such as ```python, is drawn with
the font lock of that language.  A language not in this list is tried
as LANGUAGE-mode through `major-mode-remap-alist', so `python' finds
`python-mode' or the tree-sitter mode it is remapped to.  The list is
for names that do not spell their mode.  Nil turns the painting off,
and every block uses `overblock-md-code'."
  :type '(alist :key-type (string :tag "Language") :value-type function)
  :group 'overblock-md)

(defcustom overblock-md-remote-images t
  "Whether to fetch the images markdown names by URL.
A badge, such as the Colab badge of a notebook, is an image on the
web.  shr fetches it with `url-queue-retrieve', which answers after the
rendering is done, into a buffer that is gone.  With this on, the file
is fetched once, kept in the folder overblock-images/ of the XDG cache
folder, and drawn like a local one.  The link around it keeps its click
either way.

Nil renders such an image as its alt text and uses no network."
  :type 'boolean
  :group 'overblock-md)

(defvar overblock-md--remote-failed (make-hash-table :test #'equal)
  "The image URLs that could not be fetched in this session.
A URL that failed is not fetched again, because a caller renders the
same text many times.")

(defconst overblock-md--url-regexp "\\`https?://"
  "What an image source that names the network looks like.
A relative path, an absolute one and a `file://' URL are files.  Only
these are fetched, and only where `overblock-md-remote-images' allows
it.")

(defun overblock-md--fetchable-p (url)
  "Return non-nil where URL is an image this session may go and get."
  (and overblock-md-remote-images
       ;; A terminal shows the alt text anyway, and a batch session
       ;; (the tests) must not use the network.
       (display-images-p)
       (string-match-p overblock-md--url-regexp url)
       (not (gethash url overblock-md--remote-failed))))

(defun overblock-md--cache-name (url)
  "Return the name the image at URL is cached under.
The digest of the URL, with the extension of the URL when it is a
plain one, which tells Emacs the kind of image."
  (let ((extension (file-name-extension url)))
    (concat (md5 url)
            (if (and extension
                     (string-match-p "\\`[a-zA-Z0-9]+\\'" extension))
                (concat "." (downcase extension))
              ".img"))))

(defconst overblock-md--svg-start-regexp
  (concat "\\`\ufeff?\\(?:[ \t\r\n]\\|<\\?[^>]*>\\|<!--\\(?:[^-]\\|-[^-]\\)*-->"
          "\\|<!DOCTYPE[^[>]*\\(?:\\[[^]]*\\]\\)?[ \t\r\n]*>\\)*<svg")
  "What the start of an SVG file looks like: the svg tag comes first.
Before it only a byte-order mark, blanks, the XML declaration, comments
and a DOCTYPE stand.  An error page that holds an svg icon is no SVG.")

(defun overblock-md--image-p (file)
  "Return the type of the image FILE holds, whatever its name, or nil.
Not `image-supported-file-p', which reads the name: a URL with a query
caches as `<md5>.img'.  An SVG with a DOCTYPE subset or a byte-order
mark has no header that `image-type-from-file-header' knows, so a file
that begins with an svg tag counts too.  A directory holds no image."
  (and (file-regular-p file)
       (or (image-type-from-file-header file)
           (with-temp-buffer
             (insert-file-contents file nil 0 4096)
             (and (looking-at-p overblock-md--svg-start-regexp) 'svg)))))

(defun overblock-md--remote-file (url)
  "Return the local file the image URL was fetched into, or nil.
The file is kept in the folder overblock-images/ of the XDG cache
folder, named after the URL, so a badge is fetched once per machine.
See `overblock-md--fetchable-p' for what is fetched."
  (when (overblock-md--fetchable-p url)
    (let* ((dir (expand-file-name "overblock-images/" (xdg-cache-home)))
           (file (expand-file-name (overblock-md--cache-name url) dir)))
      (if (file-readable-p file)
          file
        (condition-case error
            (progn
              (make-directory dir t)
              ;; The reader waits for the fetch while the cell renders.
              (with-timeout (3 (error "Timed out"))
                (let ((inhibit-message t))
                  (url-copy-file url file t)))
              ;; A server that answers 404 sends a page, and no error.
              (unless (overblock-md--image-p file)
                (error "Not an image"))
              file)
          (error (puthash url t overblock-md--remote-failed)
                 (ignore-errors (delete-file file))
                 (message "overblock-md: no image from %s (%s)"
                          url (error-message-string error))
                 nil))))))

(defvar overblock-md--buffer nil
  "The buffer whose markdown `overblock-md-rendered' is rendering.
shr renders in a temporary buffer.  A preview that arrives later is
shown in this buffer.")

(defun overblock-md--drop-and-settle (prop)
  "Drop the renderings of this buffer whose text carries PROP, and settle.
Nothing is drawn here: the live cycle renders them again when the
reader stops, from the cache or from a new preview.
`overblock-md-pending' marks a formula shown as text while its image
is made; `overblock-md-math' marks one drawn in the colour of the
theme, and `overblock-md-painted' a block painted in its background."
  (overblock-live-drop-if
   (lambda (block)
     (let ((over (overblock-get block :over)))
       (and (stringp over)
            (text-property-not-all 0 (length over) prop nil over))))))

(defun overblock-md--theme-changed (&rest _)
  "Have every formula and painted block drawn again, in the new theme.
A preview is drawn in the foreground of the theme, and the cache is
keyed by that colour.  A code block or a table is painted in the
background colour of the theme, not with a face."
  (overblock-md--eldoc-forget)
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (overblock-md--drop-and-settle 'overblock-md-math)
      (overblock-md--drop-and-settle 'overblock-md-painted))))

(defun overblock-md--watch-themes ()
  "Have a theme change redraw formulas and painted blocks, from now on.
The first rendering installs the hooks: loading a package of this
repository installs no hook."
  (add-hook 'enable-theme-functions #'overblock-md--theme-changed)
  (add-hook 'disable-theme-functions #'overblock-md--theme-changed))

(defvar overblock-md--latex-arrivals nil
  "The buffers whose previews have arrived and are not drawn yet.
One redraw for all of them: the engine calls back once per equation.")

(defvar overblock-md--latex-arrival-timer nil
  "The timer that draws what `overblock-md--latex-arrivals' holds.")

(defun overblock-md--latex-arrived (buffer)
  "Note that a preview for BUFFER has arrived, and have it drawn.
This is the callback of the engine.  It runs once per equation, from
a process sentinel in the buffer of the engine, so BUFFER is carried
here and a timer does the work."
  (when (buffer-live-p buffer)
    (cl-pushnew buffer overblock-md--latex-arrivals)
    (unless (timerp overblock-md--latex-arrival-timer)
      (setq overblock-md--latex-arrival-timer
            (run-with-idle-timer
             0.1 nil #'overblock-md--latex-draw-arrivals))))
  nil)

(defvar overblock-md-math-face 'default
  "The face whose foreground a formula is drawn in.
A caller that paints its rendering in a face of its own binds this, so
the formulas have the colour of the prose.  pydoc binds it to
`font-lock-doc-face'.")

(defvar overblock-md--eldoc-asker nil
  "The buffer whose hover `overblock-md-eglot-renderer' renders, or nil.
eglot renders in a temporary buffer that is gone when a preview
arrives, so the hover is asked for again in this buffer.")

(defvar overblock-md--eldoc-timer nil
  "The timer that asks for the hover again once its previews arrived.")

(defun overblock-md--eldoc-arrived (asker)
  "Ask for the documentation of ASKER again, once the previews are in."
  (unless (timerp overblock-md--eldoc-timer)
    (setq overblock-md--eldoc-timer
          (run-with-idle-timer
           0.1 nil
           (lambda ()
             (setq overblock-md--eldoc-timer nil)
             (when (and (buffer-live-p asker) (eq (window-buffer) asker))
               (with-current-buffer asker
                 ;; Not interactive, which would pop up *eldoc*; eldoc
                 ;; has no public way to forget the request it answered.
                 (setq eldoc--last-request-state nil)
                 (eldoc-print-current-symbol-info)))))))
  nil)

(defun overblock-md--latex-draw-arrivals ()
  "Draw the buffers whose previews arrived while the reader waited."
  (setq overblock-md--latex-arrival-timer nil)
  (let ((buffers overblock-md--latex-arrivals))
    (setq overblock-md--latex-arrivals nil)
    (dolist (buffer buffers)
      (when (buffer-live-p buffer)
        (with-current-buffer buffer
          (overblock-md--drop-and-settle 'overblock-md-pending))))))

(defun overblock-md--font-height (buffer)
  "Return the pixel height of the font BUFFER is shown in, or nil.
The height is measured in the frame of a window that shows BUFFER, not
the selected frame, and in BUFFER, so `text-scale-adjust' sizes the
formulas too.

Return nil where no window shows the buffer.  The engine then compiles
the equation and returns nil, and the caller asks again when the buffer
shows."
  (when-let* (((buffer-live-p buffer))
              (window (get-buffer-window buffer 'visible)))
    (with-selected-frame (window-frame window)
      (with-current-buffer buffer (default-font-height)))))

(defun overblock-md--char-width ()
  "Return the pixel width of a column of the buffer the rendering is for.
Under `text-scale-adjust' a column there is wider than one of the frame."
  (if (buffer-live-p overblock-md--buffer)
      (with-current-buffer overblock-md--buffer (default-font-width))
    (frame-char-width)))

(defun overblock-md--unwrap-environment (frag)
  "Return FRAG without the display delimiters around a math environment.
pandoc writes a bare `\\begin{align}' block as display math, and LaTeX
refuses an `align' inside `\\=\\['.  Environments that live inside math,
such as `aligned', keep their delimiters."
  (if (string-match (rx bos "\\[" (* space)
                        (group "\\begin{"
                               (or "align" "alignat" "equation" "gather" "multline"
                                   "flalign" "eqnarray")
                               (? "*") "}"
                               (* anychar))
                        (* space) "\\]" eos)
                    frag)
      (match-string 1 frag)
    frag))

(defun overblock-md--latex-image (frag)
  "Return a preview image for the LaTeX fragment FRAG, or nil.
Return `pending' where the engine has none yet: it compiles the
equation in a process of its own and calls back, and the caller shows
the fragment as text meanwhile.  Return nil where equations cannot be
drawn (a terminal, or an Emacs without SVG).

The engine runs in the buffer the rendering is for, not the temporary
buffer of shr, so it reads the options there (a preamble in
`.dir-locals.el', for example) and warns once for that buffer.  The
colour and the font size come from there too.

The image is capped like every image of a block (see
`overblock-image-height'): a block taller than the window cannot be
scrolled past."
  (when (latex-to-svg-backend-available-p)
    (let* ((frag (overblock-md--unwrap-environment frag))
           (buffer overblock-md--buffer)
           (image (with-current-buffer (if (buffer-live-p buffer)
                                           buffer
                                         (current-buffer))
                    (latex-to-svg-backend
                     frag
                     :color (face-attribute overblock-md-math-face :foreground
                                            nil 'default)
                     :font-height (overblock-md--font-height buffer)
                     :callback (let ((asker overblock-md--eldoc-asker))
                                 (lambda ()
                                   (if asker
                                       (overblock-md--eldoc-arrived asker)
                                     (overblock-md--latex-arrived buffer)))))))
           (limit (overblock-image-limit)))
      (cond
       ((and image limit)
        ;; A copy: the engine shares its image with every caller.
        (cons 'image (append (list :max-height limit) (cdr image))))
       (image)
       (t 'pending)))))

;;;###autoload
(defun overblock-md-forget-failed-images ()
  "Fetch the images again whose URL could not be reached.
A URL that failed is not fetched again in the session.  This forgets
those, and the next render tries again.

A failed LaTeX preview belongs to the engine:
`latex-to-svg-backend-invalidate' forgets one equation."
  (interactive)
  (clrhash overblock-md--remote-failed))

(defconst overblock-md--math-regexp
  (rx (or (seq "$$" (+? anychar) "$$")
          ;; No space just inside either delimiter, as in CommonMark and
          ;; GitHub, so "costs $100 and $200" is no formula. The `opt'
          ;; keeps "$x$". `in', not `any': package-lint reads `any' in a
          ;; `not' as the Emacs 31 function, and `(not (or ...))' is a
          ;; character set only from Emacs 30.
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
A fragment that stays text is read as text, and the \\( \\) of MathJax
mean nothing to a reader.  Dollars stay: a document writes formulas
with them.

The text changes, not a display property: a piece hangs a whole row on
one display property, and display properties do not nest.

`overblock-md--fit' pads a table cell back to its width.  A fragment
with a line break is left alone, so display math that stays text keeps
its rows."
  (if (or (string-search "\n" frag)
          (not (or (string-prefix-p "\\(" frag)
                   (string-prefix-p "\\[" frag))))
      frag
    (substring frag 2 -2)))

(defconst overblock-md--math-mark ?\N{OBJECT REPLACEMENT CHARACTER}
  "The character that stands in the HTML where a LaTeX fragment was.
shr fills a paragraph, and could break a fragment between a backslash
and its delimiter.  A fragment becomes as many marks as its text is
wide, so the fill knows how much room it needs.  The fill can still
break the run, and `overblock-md--unstow-math' reads a broken run as
one fragment.")

(defun overblock-md--stow-in-string (text)
  "Return TEXT with its LaTeX fragments replaced by marks that carry them.
Each fragment becomes as many marks as its text is wide, so the fill
knows how much room to leave.  The fragment is on the marks under
`overblock-md--frag', and `overblock-md--unstow-math' reads it back
from the run, so a fragment that shr drops (in a comment or a link
title) shifts no other."
  (replace-regexp-in-string
   overblock-md--math-regexp
   (lambda (frag)
     ;; `replace-regexp-in-string' reads the match data after this
     ;; returns, and the engine searches too.
     (save-match-data
       (propertize (make-string (max 1 (overblock-md--math-columns frag))
                                overblock-md--math-mark)
                   'overblock-md--frag frag)))
   text t t))

(defun overblock-md--stow-math (node)
  "Take the LaTeX fragments of the parsed NODE out, in place.
Fragments are looked for in the text nodes, so the pattern cannot span
the tags between two dollars.

The text under <code> is left alone: code shows what the writer wrote.
A fenced block is <pre><code> in every converter here, while the <pre>
of `overblock-md--verbatim-math' has no <code> under it."
  (unless (eq (dom-tag node) 'code)
    (let ((tail (dom-children node)))
      (while tail
        (let ((child (car tail)))
          (cond ((stringp child)
                 (when (string-match-p "[$\\]" child)
                   (setcar tail (overblock-md--stow-in-string child))))
                ((consp child) (overblock-md--stow-math child))))
        (setq tail (cdr tail)))))
  node)

(defun overblock-md--math-columns (frag)
  "Return how many columns the LaTeX fragment FRAG will take once shown.
The columns of its preview image when the cache has one, else the width
of its text, as for a fragment still on its way or one that stays text.
The fill and the tables use this width."
  (let ((image (and (display-images-p)
                    (overblock-md--latex-image (overblock-md--one-line frag)))))
    (if-let* (((and image (not (eq image 'pending))))
              (width (car (ignore-errors (image-size image t)))))
        (ceiling width (overblock-md--char-width))
      (string-width frag))))

(defconst overblock-md--math-run
  (let ((mark (regexp-quote (string overblock-md--math-mark))))
    (concat mark "+\\(?:\n" mark "+\\)*"))
  "What one stowed fragment looks like after the text has been laid out.
A run of marks, which the fill may have broken over rows, is one
fragment.  A newline is part of the run only with marks on both sides,
so two formulas with a blank line between them are two runs.")

(defun overblock-md--unstow-math (text)
  "Return TEXT with each run of marks replaced by the fragment it carries.
A fragment comes back as its preview image where one can be made and
drawn, else as its own text without the MathJax delimiters.

`overblock-md--stow-math' puts the fragment on the marks under
`overblock-md--frag'.

In a table a fragment takes exactly the width of its marks, because a
table is laid out in columns of characters."
  (replace-regexp-in-string
   overblock-md--math-run
   (lambda (marks)
     (save-match-data
       (let* ((frag (get-text-property 0 'overblock-md--frag marks))
              (table (get-text-property 0 'overblock-md--table marks))
              (image (and frag (display-images-p)
                          (overblock-md--latex-image
                           (overblock-md--one-line frag)))))
         (cond
          ;; A mark in the source text, which carries no fragment.
          ((null frag) marks)
          ((eq image 'pending)
           ;; Stands in for a preview on its way, which redraws.
           (propertize (overblock-md--fit
                        (overblock-md--bare-math (overblock-md--as-text frag))
                        marks table)
                       'overblock-md-pending t))
          (image
           ;; Marked, so a theme change draws it again.
           (propertize
            (if table
                (overblock-md--place-in-cell frag image marks)
              ;; The text under the image is the formula when the run
              ;; is whole, so a copy gives the formula, not marks.
              ;; Inline math on one line: a line break of the
              ;; converter inside the fragment is no row.
              (overblock-md--place-image
               (if (string-search "\n" marks) marks (overblock-md--as-text frag))
               image))
            'overblock-md-math t))
          (t
           ;; Padded only in a table, which is laid out in columns.
           (overblock-md--fit
            (overblock-md--bare-math (overblock-md--as-text frag)) marks
            table))))))
   text t t))

(defun overblock-md--place-in-cell (frag image marks)
  "Return FRAG drawing IMAGE in a table cell, as wide as MARKS were.
A table is laid out in columns of characters, and an image is in
pixels.  A stretch after the image makes up the width of the marks, so
the row keeps its columns.  A stretch inside a display string is not
drawn, but a piece that holds an image is a before-string, where it is.
Where the image cannot be measured (no frame), the padded text stays."
  (let* ((room (* (string-width marks) (overblock-md--char-width)))
         (width (car (ignore-errors (image-size image t))))
         (gap (and width (- room width))))
    (if (not gap)
        (overblock-md--fit (overblock-md--bare-math (overblock-md--as-text frag))
                           marks t)
      (concat (propertize (overblock-md--one-line frag) 'display image)
              (if (> gap 0)
                  (propertize " " 'display `(space :width (,gap)))
                "")))))

(defun overblock-md--display-p (frag)
  "Return non-nil where FRAG is display math: a $$ block or a \\[ one."
  (string-match-p "\\`\\(?:\\$\\$\\|\\\\\\[\\)" frag))

(defun overblock-md--as-text (frag)
  "Return FRAG as the text a display without images shows.
Inline math is joined to one line: the converter wraps its output, so
a fragment can carry line breaks.  Display math keeps its rows, one
equation to a line (see `overblock-md--verbatim-math')."
  (if (overblock-md--display-p frag)
      frag
    (overblock-md--one-line frag)))

(defun overblock-md--fit (text marks &optional pad)
  "Return TEXT laid out where MARKS stood, in the same number of rows.
With PAD the text takes exactly the columns of the marks, as a table
needs."
  (let ((rows (1- (length (split-string marks "\n")))))
    (concat (if pad
                (truncate-string-to-width
                 text (string-width marks) 0 ?\s)
              text)
            (make-string rows ?\n))))

(defun overblock-md--one-line (frag)
  "Return FRAG with the line breaks the fill left in it turned to spaces.
shr fills a paragraph first, so a wide formula can be broken over two
rows.  LaTeX reads it the same on one line."
  (subst-char-in-string
   ?\N{NO-BREAK SPACE} ?\s
   (if (string-search "\n" frag)
       (replace-regexp-in-string "[ \t]*\n[ \t]*" " " frag)
     frag)))

(defun overblock-md--place-image (frag image)
  "Return FRAG drawing IMAGE once, whatever line break the fill left in it.
A display property is drawn once for every screen line its run
reaches, so an image on a broken fragment would show twice.

The image goes on the part before the first break, and the rest of the
fragment is dropped except its newlines, so the rows stay as they are.

Dropped, not hidden: a row of a block is a display property, and
display properties do not nest, so a `display' over the rest would not
hide it."
  (cond
   ((not (string-search "\n" frag))
    (propertize frag 'display image))
   ;; As an image, display math is one row, without empty rows under it.
   ((overblock-md--display-p frag)
    (propertize (overblock-md--one-line frag) 'display image))
   (t
    (let ((break (string-search "\n" frag)))
      (concat (propertize (substring frag 0 break) 'display image)
              (make-string (cl-count ?\n frag :start break) ?\n))))))

(defun overblock-md-program ()
  "Return the markdown converter as a list of program and arguments.
The first installed candidate of `overblock-md-command' wins.  Return
nil when none is installed, or when this Emacs has no
`libxml-parse-html-region' (a build without libxml2) for shr."
  (and (fboundp 'libxml-parse-html-region)
       (seq-some (lambda (command)
                   (let ((argv (split-string-shell-command command)))
                     (and (executable-find (car argv)) argv)))
                 (ensure-list overblock-md-command))))

(defconst overblock-md--marker "overblockcellbreak8f2b1c"
  "What stands between cells when they go to the converter together.
A plain word in a paragraph of its own, which every converter passes
through as a paragraph.")

(defun overblock-md--html (md)
  "Return the HTML `overblock-md-command' makes of MD, or nil.
Return nil where no converter is installed or it exits non-zero, so
the cell stays plain text.  This never signals: a caller renders from
the body of a minor mode, where an error would stop the hook that
turns the mode on."
  (when-let* ((program (overblock-md-program)))
    (let ((errors (make-temp-file "overblock-md-stderr")))
      (unwind-protect
          (with-temp-buffer
            (insert md)
            ;; Standard error to a file: pandoc warns there about math,
            ;; which must not land in the HTML, and a failure reports
            ;; its last line.
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

(defun overblock-md--batch-text (texts)
  "Return TEXTS joined for one call of the converter, or nil.
Return nil where a text holds the marker that separates them; the
caller then converts each text alone."
  (unless (seq-some (lambda (text) (string-search overblock-md--marker text))
                    texts)
    (string-join (mapcar #'overblock-md--verbatim-math texts)
                 (format "\n\n%s\n\n" overblock-md--marker))))

(defun overblock-md--batch-pieces (page texts)
  "Return the HTML of each of TEXTS out of PAGE, or nil.
Return nil where the marker did not come back once between every pair."
  (when-let* ((page)
              (pieces (split-string
                       page
                       (format "<p>[ \t\n]*%s[ \t\n]*</p>"
                               overblock-md--marker))))
    (and (= (length pieces) (length texts)) pieces)))

(defun overblock-md--batch-answer (page texts callback)
  "Hand CALLBACK the HTML of each of TEXTS out of PAGE.
Where the markers did not all come back, a text swallowed them: the
converter read it on to the end of all it was sent.  Each
half of TEXTS then goes again in a process of its own, and a text
alone gets nil, for the
caller to convert alone.  One such text costs a few processes in the
background, not one in the foreground for each text.  A PAGE of nil is
a converter that failed, as pandoc does where a marker lands in a YAML
block, and halves the same way."
  (if-let* ((pieces (or (overblock-md--batch-pieces page texts)
                         (null (cdr texts)))))
      (funcall callback (and (consp pieces) pieces))
    (let* ((head (seq-take texts (/ (length texts) 2)))
           (tail (nthcdr (length head) texts)))
      (overblock-md-html-batch-async
       head
       (lambda (first)
         (overblock-md-html-batch-async
          tail
          (lambda (second)
            (funcall callback
                     (append (or first (make-list (length head) nil))
                             (or second (make-list (length tail) nil)))))))))))

(defun overblock-md-html-batch-async (texts callback)
  "Convert TEXTS in one process and hand the HTML of each to CALLBACK.
CALLBACK gets the list in the order of TEXTS.  A batch that loses its
markers goes again in halves, and a text that still fails alone gets
nil in the list; see `overblock-md--batch-answer'.  CALLBACK gets nil
where the converter is missing or a text holds the marker.

Nothing waits for the process, so Emacs does not freeze.  When the
buffer that asked dies first, the answer is dropped."
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
               ;; Discard standard error. `:stderr nil' would mix the
               ;; warnings of pandoc into the HTML.
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
                         (overblock-md--batch-answer page texts
                                                     callback)))))))))
        (process-send-string process joined)
        (process-send-eof process)
        process)
    (funcall callback nil)
    nil))

(defvar-local overblock-md--in-flight nil
  "The kinds whose batch is still with the converter, each as (KIND . AGAIN).
AGAIN is non-nil where a cycle asked for KIND meanwhile: that cycle
renders again once the batch has landed.")

(defconst overblock-md--slice 50
  "How many renderings a batch shows before it lets the reader in.")

(defun overblock-md-render-regions (regions kind text show)
  "Render the REGIONS that want a rendering of KIND, in one process.
REGIONS are conses of buffer positions.  TEXT is called with the
bounds of one and returns its markdown.  SHOW is called with the
bounds and the HTML of one and draws it; the HTML is nil where the
converter failed for it, and SHOW then converts alone.
Nothing waits: the renderings arrive later, through
`overblock-md-html-batch-async'.

One batch of KIND at a time: a cycle that comes while one is with the
converter would send the same regions again, so it waits, and the live
cycle of KIND runs once more when the batch has landed.

`overblock-live-wanted-p' says which regions want rendering, and is
asked again when the answer arrives, because the reader can click,
type and move meanwhile.  A region whose markdown changed since it was
sent keeps its text.  The regions are markers, so text typed above
them does not move the renderings.  Nothing happens where no converter
is installed."
  (if-let* ((flight (assq kind overblock-md--in-flight)))
      (setcdr flight t)
    (when-let* (((overblock-md-program))
                (wanted (seq-filter (lambda (region)
                                      (overblock-live-wanted-p
                                       (car region) (cdr region) kind))
                                    regions))
                (marked (mapcar (lambda (region)
                                  (cons (copy-marker (car region))
                                        (copy-marker (cdr region) t)))
                                wanted)))
      (let ((sources (mapcar (lambda (region)
                               (funcall text (car region) (cdr region)))
                             marked)))
        (push (cons kind nil) overblock-md--in-flight)
        (condition-case err
            (overblock-md--send-batch kind marked sources text show)
          (error (overblock-md--land kind)
                 (dolist (region marked)
                   (set-marker (car region) nil)
                   (set-marker (cdr region) nil))
                 (signal (car err) (cdr err))))))))

(defun overblock-md--land (kind)
  "End the flight of the batch of KIND, and return its entry."
  (let ((flight (assq kind overblock-md--in-flight)))
    (setq overblock-md--in-flight (delq flight overblock-md--in-flight))
    flight))

(defun overblock-md--send-batch (kind marked sources text show)
  "Send the SOURCES of the MARKED regions of KIND, and show what comes back.
TEXT and SHOW are those of `overblock-md-render-regions'."
  (overblock-md-html-batch-async
   sources
   (lambda (htmls)
     (overblock-md--show-batch
      (current-buffer) kind text show
      (overblock-md--in-view-first
       ;; Not `cl-mapcar': HTMLS is nil where the converter failed,
       ;; and each region converts alone.
       (mapcar (lambda (region)
                 (list region (pop htmls) (pop sources)))
               marked))))))

(defun overblock-md--in-view-first (items)
  "Return ITEMS with those a window shows first, in order otherwise.
Each item starts with the (BEG . END) markers of its region.  A large
buffer then shows its window at once, and the rest follows in slices."
  (let ((windows (mapcar (lambda (window)
                           (cons (window-start window) (window-end window)))
                         (get-buffer-window-list nil nil 'visible))))
    (seq-sort-by (lambda (item)
                   (if (seq-some (lambda (shown)
                                   (and (< (caar item) (cdr shown))
                                        (> (cdar item) (car shown))))
                                 windows)
                       0 1))
                 #'< items)))

(defun overblock-md--show-item (item kind text show)
  "Show ITEM of a batch of KIND, and free its markers.
ITEM, TEXT and SHOW are those of `overblock-md--show-batch'."
  (pcase-let ((`((,beg . ,end) ,html ,source) item))
    (unwind-protect
        ;; The whole buffer: the reader can narrow while the batch is out.
        (without-restriction
          (when (and (overblock-live-wanted-p beg end kind)
                     ;; The text is still the text that was sent.
                     (equal source (funcall text beg end)))
            (funcall show beg end html)))
      (set-marker beg nil)
      (set-marker end nil))))

(defun overblock-md--show-batch (buffer kind text show items)
  "Show the ITEMS of a batch of KIND in BUFFER, a slice at a time.
Each item is (REGION HTML SOURCE).  `overblock-md--slice' of them are
shown, and a timer shows the rest, so the reader is never held for the
whole of a large buffer.  TEXT and SHOW are those of
`overblock-md-render-regions'.  The last slice ends the flight and runs
the live cycle of KIND again where one asked meanwhile."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (condition-case err
          (dotimes (_ (min overblock-md--slice (length items)))
            (overblock-md--show-item (pop items) kind text show))
        ;; A rendering that fails ends the flight, or the kind would
        ;; wait for it for good, and frees the markers of the rest.
        (error (overblock-md--land kind)
               (dolist (item items)
                 (set-marker (caar item) nil)
                 (set-marker (cdar item) nil))
               (signal (car err) (cdr err))))
      (if items
          (run-with-timer 0 nil #'overblock-md--show-batch
                          buffer kind text show items)
        (let ((flight (overblock-md--land kind)))
          (when-let* (((cdr flight))
                      (spec (assq kind overblock-live--specs)))
            (funcall (nth 1 spec))))))))

(defun overblock-md--verbatim-math (md)
  "Return MD with its display-math blocks wrapped in <pre>.
A $$ block has one equation to a line, and shr fills a paragraph.  <pre>
passes through every converter as raw HTML, and shr keeps its lines.

This happens on every display, because a fragment can stay text on any
display (no LaTeX, or a compile error).  A preview is not affected:
the block is matched across its lines and replaced whole.

Not inside a fenced block, which shows what was written."
  ;; The common case, without a copy of the text.
  (if (not (string-search "$$" md))
      md
    (with-temp-buffer
      (insert md)
      (goto-char (point-min))
      (let ((fence "^[ \t]*\\(?:```\\|~~~\\)"))
        (while (re-search-forward "^\\$\\$\n\\(\\(?:.*\n\\)*?\\)\\$\\$$" nil t)
          (let ((beg (match-beginning 0))
                (end (match-end 0))
                (body (match-string 1)))
            ;; An odd number of fence lines above: inside a fence.
            (unless (cl-oddp (save-excursion
                               (goto-char beg)
                               (count-matches fence (point-min) (point))))
              (delete-region beg end)
              (goto-char beg)
              (insert "<pre>$$\n" body "$$</pre>")))))
      (buffer-string))))

(defun overblock-md--tag-table (dom)
  "Render the table DOM and mark the text it covers.
`overblock-md--unstow-math' keeps the width of a formula in marked
text, because a table is padded to the width of its text."
  (let ((start (point)))
    ;; shr draws every image of a table again under it, from
    ;; `shr-collect-extra-strings-in-table', which is no public API; an
    ;; image in a cell is drawn there once.
    (cl-letf* ((collect (symbol-function 'shr-collect-extra-strings-in-table))
               ((symbol-function 'shr-collect-extra-strings-in-table)
                (lambda (&rest args)
                  (let ((shr-external-rendering-functions
                         (cons '(img . ignore) shr-external-rendering-functions)))
                    (apply collect args)))))
      (shr-tag-table dom))
    (put-text-property start (point) 'overblock-md--table t)))

(defun overblock-md--directory ()
  "Return the directory of the file whose markdown is rendering.
Not `default-directory', which a caller can have bound elsewhere when
the answer of the converter arrives, as eglot binds it to the root of
the project."
  (if-let* ((buffer (if (buffer-live-p overblock-md--buffer)
                        overblock-md--buffer
                      (current-buffer)))
            (file (buffer-file-name buffer)))
      (file-name-directory file)
    default-directory))

(defun overblock-md--image-file (src)
  "Return the readable local image file that SRC names, or nil.
A relative path, as in `![a figure](figure.png)', is relative to the
directory of the file of the buffer.  An absolute path and a `file://'
URL name the file directly.  Another scheme returns nil."
  (when-let* ((path (cond ((string-prefix-p "file://" src)
                           (url-unhex-string (substring src 7)))
                          ((not (string-match-p "\\`[a-zA-Z][a-zA-Z0-9+.-]*:"
                                                src))
                           src)))
              ((not (string-empty-p path)))
              (file (expand-file-name path (overblock-md--directory)))
              ((file-readable-p file))
              ;; The content, not the name: a git-lfs pointer is text.
              ((overblock-md--image-p file)))
    file))

(defun overblock-md--tag-list (dom)
  "Render the list DOM, and open a paragraph only where no list is open.
shr puts a blank line before and after every list, which makes a
nested list taller than its source.  `shr-list-mode' says that a list
is open already.  Outside a list, the handler of shr keeps its blank
lines."
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
Pandoc wraps a description in a paragraph, and shr opens a paragraph
with a blank line, so a numpydoc entry would be taller than its source.
Only the first paragraph is unwrapped, so two paragraphs stay two.

Otherwise this is `shr-tag-dd': the description is four columns in
from its term, as numpydoc writes it."
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

(defun overblock-md--image-label (alt where)
  "Return the label of an image with ALT text, at the file or URL WHERE.
An empty alt text becomes the file name in brackets, and nothing for
a URL."
  (cond ((and alt (not (string-empty-p alt))) alt)
        ((string-match-p overblock-md--url-regexp where) "")
        (t (format "[%s]" (file-name-nondirectory where)))))

(defun overblock-md--image (file dom)
  "Return the image of FILE, at the size that the tag DOM asks for.
A README sizes a logo with width and height, as <img width=\"80\">
or pandoc's {width=80}; in pixels, they are the size it is drawn at.
As in eww, the baseline is at the foot, so a link underline runs under
the image."
  (apply #'create-image file (overblock-md--image-p file) nil :ascent 100
         (mapcan (lambda (attribute)
                   (when-let* ((value (dom-attr dom attribute))
                               ((string-match "\\`\\([0-9]+\\)\\(?:px\\)?\\'"
                                              value)))
                     (list (intern (format ":%s" attribute))
                           (string-to-number (match-string 1 value)))))
                 '(width height))))

(defun overblock-md--tag-img (dom)
  "Draw the image DOM names when it is a file, or else its label.
shr would fetch an image with `url-queue-retrieve', which answers after
the cell is rendered, so the rendering keeps the placeholder.  Only a
data URI goes to shr, which draws it with no fetch.

The alt text carries the image; `overblock-md-rendered' caps it.  See
`overblock-md--image-label' for an image with no alt text."
  (let* ((src (or (dom-attr dom 'src) ""))
         (alt (dom-attr dom 'alt))
         (file (or (overblock-md--image-file src)
                   (overblock-md--remote-file src)))
         ;; A drawn image needs a label to sit on: a fetched one with
         ;; no alt text gets the name of its file in the cache.
         (label (overblock-md--image-label alt (or file src))))
    (if (string-prefix-p "data:" src)
        ;; shr draws the image of a data URI itself.
        (shr-tag-img dom)
      ;; The parser drops a blank between two images, which would run
      ;; their labels together.
      (when (and (> (point) (point-min))
                 (get-text-property (1- (point)) 'overblock-md-label))
        (insert " "))
      ;; A remote image that was not fetched, or a file that is not
      ;; there, stays its label. Not given to shr, which would fetch
      ;; it whatever the option says. Without images there is no
      ;; display property either: the placeholder of shr is an image,
      ;; which would hide the label.
      (insert (propertize (if (and file (display-images-p))
                              (propertize label 'display
                                          (overblock-md--image file dom))
                            label)
                          'overblock-md-label t)))))

(defun overblock-md--code-mode (dom)
  "Return the major mode that paints the fenced block DOM, or nil.
The language is in the class of the <pre> or of its <code>: pandoc
writes `python', markdown_py and cmark `language-python'.
`overblock-md-code-modes' maps it to a mode, else LANGUAGE-mode through
`major-mode-remap-alist'.  A mode that this Emacs does not have gives
nil, and the block wears `overblock-md-code'."
  (when-let* ((overblock-md-code-modes)
              (class (or (dom-attr dom 'class)
                         (dom-attr (dom-child-by-tag dom 'code) 'class)))
              (lang (seq-some (lambda (word)
                                (and (not (member word '("sourceCode" "numberSource")))
                                     (string-remove-prefix "language-" word)))
                              (split-string class)))
              (mode (or (cdr (assoc lang overblock-md-code-modes))
                        (intern-soft (concat lang "-mode")))))
    (setq mode (alist-get mode major-mode-remap-alist mode))
    (and (fboundp mode) mode)))

(defun overblock-md--text (dom)
  "Return the text of DOM, its children's joined with nothing between.
`dom-texts' puts a space between them and is obsolete in Emacs 31,
and `dom-inner-text' is not in Emacs 29.1."
  (if (stringp dom)
      dom
    (mapconcat #'overblock-md--text (dom-children dom) "")))

(defun overblock-md--tag-pre (dom)
  "Render the preformatted block DOM: a fenced block, or raw HTML.
A <pre> that holds markup other than <code> is raw HTML, and its tags
and line breaks are shr's to draw.  The rest is a fenced block; see
`overblock-md--code-block'."
  (if (seq-some (lambda (child)
                  (and (consp child) (not (eq (dom-tag child) 'code))))
                (dom-children dom))
      (shr-tag-pre dom)
    (overblock-md--code-block dom)))

(defun overblock-md--code-block (dom)
  "Render the fenced block DOM with the font lock of its language.
The code goes through a buffer in that mode and comes back with its
faces.  The indentation of shr comes before each line, so a block in a
list item keeps its place.  A block whose language this Emacs does not
have, or that names none, wears `overblock-md-code'.

Under the faces of the language goes only the background of
`overblock-md-code', so `overblock-md--squared' makes a rectangle of
the block, and plain identifiers keep the default colour."
  (let* ((mode (overblock-md--code-mode dom))
         (shr-folding-mode 'none)
         (code (with-temp-buffer
                 (insert (string-trim-right (overblock-md--text dom)))
                 (if mode
                     (let ((inhibit-message t)
                           (message-log-max nil))
                       (ignore-errors (delay-mode-hooks (funcall mode)))
                       (font-lock-ensure))
                   (add-face-text-property (point-min) (point-max)
                                           'overblock-md-code))
                 (let ((code (buffer-string)))
                   (when-let* ((background (overblock-md--background
                                            'overblock-md-code)))
                     (overblock-faced code (list :background background
                                                 :extend t)))
                   code))))
    (shr-ensure-newline)
    (dolist (line (split-string code "\n"))
      (shr-indent)
      (insert line "\n"))
    (shr-ensure-newline)))

(defvar overblock-md-width nil
  "The number of columns a rendering is filled to, or nil for shr's own.
A rendering happens in a temporary buffer, where shr fills to the width
of the frame.  A block is often narrower (a split window, an indented
doc string), and a longer row is truncated at the window edge.

The mode that renders binds this, because it knows where its block
is.  `overblock-md-columns' measures the room.")

(defun overblock-md--background (face)
  "Return the background FACE paints with, or nil where it paints none.
FACE is what a text property holds: a named face, a plist, or a list of
either."
  (cond
   ((null face) nil)
   ((facep face)
    (let ((background (face-attribute face :background nil t)))
      (unless (memq background '(nil unspecified)) background)))
   ((keywordp (car-safe face))
    (or (let ((background (plist-get face :background)))
          (unless (memq background '(nil unspecified)) background))
        (overblock-md--background (plist-get face :inherit))))
   ((consp face) (seq-some #'overblock-md--background face))))

(defun overblock-md--rectangle (lines background)
  "Return LINES painted BACKGROUND over their whole width.
shr paints the background of a code block or a table row only as far
as the text of each row.  The rows are squared off to the longest, and
the indentation before the text is painted too."
  (let ((width (apply #'max (mapcar #'string-width lines)))
        (paint (list :background background)))
    ;; The colour is that of the theme now; see
    ;; `overblock-md--theme-changed'.
    (mapcar (lambda (line)
              ;; Pads to a display width and keeps the properties;
              ;; `string-pad' measures with `length'.
              (let ((padded (truncate-string-to-width line width 0 ?\s)))
                ;; Appended, so a bold header cell stays bold.
                (add-face-text-property 0 (length padded) paint t padded)
                (put-text-property 0 (length padded) 'overblock-md-painted t
                                   padded)
                padded))
            lines)))

(defun overblock-md--squared (text)
  "Return TEXT with every run of painted rows squared off.
A row is painted when its last character is: shr ends a table row and
a line of code with the face of the block.

A blank line has no face, but belongs to the run when painted rows are
on both sides of it.  It is held back until a painted row follows,
because a blank line at the end of a block belongs to nothing."
  ;; The lists grow by `push' and are reversed once at the end. Order
  ;; does not matter to the padding, so a reversed run squares off the
  ;; same.
  (let (rows run background held)
    (cl-flet ((flush ()
                (when run
                  (setq rows (nconc (overblock-md--rectangle run background)
                                    rows)
                        run nil))
                (setq rows (nconc held rows) held nil)))
      (dolist (line (split-string text "\n"))
        (let ((paint (and (> (length line) 0)
                          (overblock-md--background
                           (get-text-property (1- (length line))
                                              'face line)))))
          (cond
           ((and paint (equal paint background))
            (setq run (cons line (nconc held run)) held nil))
           (paint (flush) (setq background paint run (list line)))
           ((and run (string-blank-p line))
            (push line held))
           (t (flush)
              (setq background nil)
              (push line rows)))))
      (flush))
    (string-join (nreverse rows) "\n")))

(defun overblock-md-columns (&optional indent)
  "Return the columns a rendering has, INDENT of them spent on indenting.
Return nil where no window shows this buffer, which leaves the filling
to shr.  One column is kept back, because a row that fills the last
column wraps.  `overblock-window-columns' counts in the font of the
window, so `text-scale-adjust' is respected."
  (when-let* ((columns (overblock-window-columns)))
    (max 20 (- columns (or indent 0) 1))))

(defun overblock-md-follow-link (event)
  "Follow the rendered link clicked in EVENT; see `overblock-md-browse'.
A rendering is a display string, and `shr-browse-url' reads the URL
from buffer text at point.  This reads it from the clicked string."
  (interactive "e")
  (let* ((posn (event-start event))
         (url (pcase (posn-string posn)
                (`(,string . ,index) (get-text-property index 'shr-url string))
                (_ (with-current-buffer (window-buffer (posn-window posn))
                     (get-char-property (posn-point posn) 'shr-url))))))
    (if url
        (progn (select-window (posn-window posn))
               (overblock-md-browse url))
      (message "No link here"))))

(defun overblock-md-browse (url)
  "Open URL, the target of a link in the markdown of this buffer.
A URL with a scheme goes to `browse-url'.  A link of a README is often
relative: #SLUG goes to the heading of this buffer whose id is SLUG,
and a path opens its file, relative to this buffer, at its #SLUG if it
names one, or at its line N for #LN.  See `overblock-md--slugs' for
the ids of a heading."
  (if (string-match-p "\\`[a-zA-Z][a-zA-Z0-9+.-]*:" url)
      (browse-url url)
    (pcase-let* ((`(,path ,anchor)
                  (mapcar (lambda (part)
                            (decode-coding-string (url-unhex-string part) 'utf-8))
                          (split-string url "#")))
                 (file (and (not (string-empty-p path))
                            (overblock-md--link-file path))))
      (cond ((and file (not (file-exists-p file)))
             (message "No file %s" (abbreviate-file-name file)))
            (t (when file (find-file file))
               (when (and anchor (not (string-empty-p anchor)))
                 (overblock-md--goto-anchor anchor)))))))

(defun overblock-md--goto-anchor (anchor)
  "Move to the ANCHOR of a link in this buffer.
LN is line N, as GitHub links code, and LN-LM begins there too; #l2,
in lower case, is the id of a heading.  Anything else is a heading."
  (if (let ((case-fold-search nil))
        (string-match "\\`L\\([0-9]+\\)\\(?:-L[0-9]+\\)?\\'" anchor))
      (progn (push-mark nil t)
             (goto-char (point-min))
             (forward-line (1- (string-to-number (match-string 1 anchor)))))
    (overblock-md--goto-heading anchor)))

(defun overblock-md--link-file (path)
  "Return the file that the link PATH names, from this buffer.
A PATH that starts with a slash is from the root of the repository, as
on GitHub, where there is one."
  (if-let* (((string-prefix-p "/" path))
            (root (vc-root-dir)))
      (expand-file-name (substring path 1) root)
    (expand-file-name path)))

(defvar-local overblock-md-heading-regexp "^#+[ \t]+\\(.*?\\)[ \t#]*$"
  "What a heading of the markdown of this buffer looks like.
Group 1 is its text.  A mode whose markdown is in comments, as the
cells of a notebook are, sets its own.")

(defun overblock-md--slugs (heading)
  "Return the ids that pandoc and GitHub give the HEADING text.
Both are in lower case, without punctuation, with dashes for spaces.
Pandoc drops what comes before the first letter and makes one dash of
a run of spaces; GitHub keeps digits, and a dash for every space."
  (let ((text (downcase (string-trim
                         ;; A link counts by its text, as rendered, and a
                         ;; tag not at all; in code, < and > are text.
                         (replace-regexp-in-string
                          "<[^>]*>" ""
                          (replace-regexp-in-string
                           "`[^`]*`"
                           (lambda (code)
                             (replace-regexp-in-string "[<>`]" "" code))
                           (replace-regexp-in-string
                            "\\[\\([^]]*\\)\\]([^)]*)" "\\1" heading)
                           t t))))))
    (list (replace-regexp-in-string
           " +" "-" (replace-regexp-in-string
                     "\\`[^[:alpha:]]+\\|[^[:alnum:] _.-]" "" text))
          (replace-regexp-in-string
           " " "-" (replace-regexp-in-string "[^[:alnum:] _-]" "" text)))))

(defun overblock-md--goto-heading (slug)
  "Move to the heading of this buffer whose id is SLUG."
  (if-let* ((pos (save-excursion
                   (goto-char (point-min))
                   (overblock-md--find-heading slug))))
      (progn (push-mark nil t) (goto-char pos))
    (message "No heading #%s here" slug)))

(defun overblock-md--find-heading (slug)
  "Return the start of the first heading from point whose id is SLUG.
A line in a fenced block is code, as a # comment there is, and no
heading; a fence line of either kind, at the margin, opens or closes
one.  A line with an HTML anchor, id=\"SLUG\" or name=\"SLUG\", counts
too.  Return nil where there is none."
  (let ((seen (make-hash-table :test #'equal))
        (anchor (format "<[[:alpha:]][^>]*[ \t]\\(?:id\\|name\\)=[\"']%s[\"']"
                        (regexp-quote slug)))
        fence found)
    (while (and (not found) (not (eobp)))
      (cond ((looking-at-p " \\{0,3\\}\\(?:```\\|~~~\\)") (setq fence (not fence)))
            (fence)
            ((or (save-excursion (re-search-forward anchor (pos-eol) t))
                 (member slug (overblock-md--ids (overblock-md--slugs-here)
                                                 seen)))
             (setq found (point))))
      (forward-line 1))
    found))

(defun overblock-md--ids (slugs seen)
  "Return the ids of a heading whose SLUGS are those of its text.
The second heading of one text gets the id SLUG-1, the third SLUG-2,
as pandoc and GitHub give them.  SEEN counts the headings so far."
  (mapcar (lambda (slug)
            (let ((n (gethash slug seen 0)))
              (puthash slug (1+ n) seen)
              (if (zerop n) slug (format "%s-%d" slug n))))
          (delete-dups (copy-sequence slugs))))

(defun overblock-md--slugs-here ()
  "Return the ids of the heading on this line, or nil.
A heading is a line that `overblock-md-heading-regexp' matches, or a
line of text over a line of = or of -."
  (when-let* ((text (cond ((looking-at overblock-md-heading-regexp)
                           (match-string 1))
                          ((and (looking-at "[ \t]*\\([^ \t\n].*?\\)[ \t]*$")
                                (save-excursion
                                  (forward-line 1)
                                  (looking-at-p "\\(?:=+\\|-+\\)[ \t]*$")))
                           (match-string 1)))))
    (overblock-md--slugs text)))

(defvar-keymap overblock-md-link-map
  :doc "Keymap on a rendered link: `shr-map' with a click that works.
See `overblock-md-follow-link'."
  :parent shr-map
  "<mouse-2>" #'overblock-md-follow-link)

(defun overblock-md--own-links ()
  "Give every link of this buffer `overblock-md-link-map' for `shr-map'."
  (let ((pos (point-min)))
    (while (setq pos (text-property-any pos (point-max) 'keymap shr-map))
      (let ((end (next-single-property-change pos 'keymap nil (point-max))))
        (put-text-property pos end 'keymap overblock-md-link-map)
        (setq pos end)))))

(defun overblock-md-rendered (md &optional html)
  "Render the markdown MD to a propertized string.
`overblock-md-command' produces HTML, shr renders it, and LaTeX
fragments become preview images.  With HTML, that is rendered instead
and MD is not converted again: `overblock-md-html-batch-async' converts
a whole buffer of cells at once.

shr renders without fonts here: the text hangs on source lines at any
indentation, and only literal columns survive a move.  The `:align-to'
specs of shr become real spaces for the same reason.

Return nil where no converter is installed and no HTML is given; the
caller then leaves the markdown as it is."
  ;; An empty cell renders as the empty string, not nil.
  (overblock-md--watch-themes)
  (when-let* ((page (or html (overblock-md--html
                              (overblock-md--verbatim-math md)))))
    (let* ((overblock-md--buffer (current-buffer))
           (dom (overblock-md--stow-math
                 (with-temp-buffer
                   (insert page)
                   (libxml-parse-html-region (point-min) (point-max)))))
           (shr-use-fonts nil)
           ;; In columns, because `shr-use-fonts' is off above.
           (shr-width overblock-md-width)
           ;; The asterisk of shr is what the source says already.
           (shr-bullet "• ")
           ;; Emacs 31 slices a tall image per window line, but there is
           ;; no window here; `overblock-image-limit' caps the whole image.
           (shr-sliced-image-height nil)
           ;; shr has no function for a th, and its fixed pitch code
           ;; looks like prose in a fixed pitch buffer.
           (shr-external-rendering-functions
            `((th . ,(lambda (dom) (shr-fontize-dom dom 'bold)))
              (code . ,(lambda (dom)
                         (shr-fontize-dom dom 'overblock-md-code)))
              (dd . overblock-md--tag-dd)
              (ul . overblock-md--tag-list)
              (ol . overblock-md--tag-list)
              (img . overblock-md--tag-img)
              (pre . overblock-md--tag-pre)
              (table . overblock-md--tag-table)
              ;; shr draws h3 in italic and h4 to h6 plain, which a
              ;; font with no italic shows as body text. Bold, they
              ;; look as h2 does, which takes a step up.
              (h2 . ,(lambda (dom) (shr-heading dom 'shr-h2 '(:height 1.1))))
              ,@(mapcar (lambda (tag)
                          (cons tag (lambda (dom)
                                      (shr-heading dom (intern (format "shr-%s" tag))
                                                   'bold))))
                        '(h3 h4 h5 h6))
              ,@shr-external-rendering-functions)))
      (with-temp-buffer
        (shr-insert-document dom)
        (overblock-md--own-links)
        (overblock-flatten-alignment)
        ;; Trim whole blank lines, never the indentation of the first
        ;; line: a table at the start keeps its columns. Every image of
        ;; the rendering is capped here.
        (overblock-image-cap
         (overblock-md--squared
          (overblock-md--unstow-math
           (string-trim (buffer-string) "\\(?:[ \t]*\n\\)+"))))))))

;;;; A renderer for what a language server says

(defvar overblock-md--eldoc-cache (make-hash-table :test #'equal)
  "The rendering of each hover text seen, by the text.
eldoc asks again on every idle after a move, often for the same text,
and a converter process is slow.")

(defconst overblock-md--eldoc-cache-size 200
  "How many renderings are kept before the table is emptied.")

(defun overblock-md--eldoc-forget ()
  "Forget the renderings kept for eldoc.
A theme change calls this, because a formula has the colour of the
theme, and so does the `:set' of `overblock-md-eldoc-width'."
  (clrhash overblock-md--eldoc-cache))

(defcustom overblock-md-eldoc-width 72
  "Columns a language server's documentation is filled to.
`overblock-md-eglot-renderer' renders in a buffer that no window
shows, so this sets the width for the echo area, the *eldoc* buffer
and an eldoc-box child frame alike."
  :type 'natnum
  :set (lambda (symbol value)
         (set-default symbol value)
         (overblock-md--eldoc-forget))
  :group 'overblock-md)

(defun overblock-md--eldoc-markdown (md)
  "Return MD as the converter reads it the way the server meant it.
A server writes its signature in a fence, a rule of three dashes, and
the first paragraph directly under the rule.  pandoc reads a rule, a
paragraph and the setext underline of the next section as one simple
table.  A blank line after the rule makes it a rule."
  (replace-regexp-in-string "^---\n" "---\n\n" md t t))

(defun overblock-md--eldoc-rendering (md)
  "Return the rendering of the hover text MD, from the table where it can.
Return nil where no converter is installed.  A rendering that still
waits for a formula is not kept, or its stand-in would stay."
  (or (gethash md overblock-md--eldoc-cache)
      (let ((rendered (let ((overblock-md-width overblock-md-eldoc-width)
                            (overblock-md--eldoc-asker (window-buffer)))
                        (overblock-md-rendered (overblock-md--eldoc-markdown md)))))
        (when (and rendered
                   (not (text-property-not-all 0 (length rendered)
                                               'overblock-md-pending nil
                                               rendered)))
          (when (>= (hash-table-count overblock-md--eldoc-cache)
                    overblock-md--eldoc-cache-size)
            (clrhash overblock-md--eldoc-cache))
          (puthash md rendered overblock-md--eldoc-cache))
        rendered)))

;;;###autoload
(defun overblock-md-eglot-renderer ()
  "Render the markdown of this buffer with `overblock-md-rendered', in place.
A value for `eglot-documentation-renderer':

  (setopt eglot-documentation-renderer #\\='overblock-md-eglot-renderer)

eglot calls it in a temporary buffer that holds the markdown of the
server, and runs `font-lock-ensure' afterwards.  Unlike the font lock
of a markdown mode, this lays out tables, draws formulas and paints
fenced blocks in their language, with `overblock-md-command'.  The
result shows wherever eldoc shows documentation, filled to
`overblock-md-eldoc-width'.  eglot renders plain text with `text-mode'
whatever the variable says.  Without a converter the markdown stays as
it is."
  (when-let* ((rendered (overblock-md--eldoc-rendering (buffer-string))))
    (erase-buffer)
    (insert rendered)
    ;; Else the `font-lock-ensure' of eglot removes every face.
    (setq-local font-lock-fontified t)))

(provide 'overblock-md)
;;; overblock-md.el ends here
