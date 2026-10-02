;;; overblock-pydoc.el --- Python documentation, read as documentation  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Marcel Arpogaus

;; Author: Marcel Arpogaus <znepry.necbtnhf@tznvy.pbz>
;; Assisted-by: Claude:claude-opus-5
;; Version: 1.0
;; Package-Requires: ((emacs "29.1") (overblock "1.0") (overblock-md "1.0"))
;; Keywords: languages, docs, convenience
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

;; `overblock-pydoc-mode' shows the doc strings of a Python buffer as
;; the documentation they are.  The triple quotes and the indentation go,
;; the markup is rendered, and the code around them is untouched.  A
;; click on one shows its source in place, through `overblock-live-edit'.
;; The edit button (`overblock-pydoc-edit') opens the doc string in a
;; buffer of its own.  Point moving through one changes nothing.
;;
;; Font lock decides which strings are documentation.  python.el paints
;; the doc string of a module, a definition or an assignment with
;; `font-lock-doc-face', and every other string with
;; `font-lock-string-face'.  So the mode needs no parser and no grammar
;; of its own, and works in `python-mode' and `python-ts-mode' alike.
;;
;; It runs the same live cycle as `overblock-md-preview-mode', through
;; `overblock-live-start'.  This file says which regions to render, and
;; with what.

;;; Code:

(require 'overblock)
(require 'overblock-md)

(defgroup overblock-pydoc nil
  "Python doc strings rendered where they are written."
  :group 'languages
  :group 'overblock
  :prefix "overblock-pydoc-")

;;;###autoload (put 'overblock-pydoc-markup 'safe-local-variable #'symbolp)
(defcustom overblock-pydoc-markup 'markdown
  "The markup the doc strings of this buffer are written in.
It says which command of `overblock-pydoc-command' renders them and
which major mode of `overblock-pydoc-modes' reads them, so that the
rendering and the buffer `overblock-pydoc-edit' opens agree.

Markdown is the default: a numpy style doc string, as mkdocstrings
reads it, holds Markdown under its section titles (fenced blocks, pipe
tables, bold).  A project whose doc strings are reStructuredText for
Sphinx sets this to `rst', best per project:

  ;;; .dir-locals.el
  ((python-base-mode . ((overblock-pydoc-markup . rst))))

A reader given the wrong markup lays out what it does not know as
prose."
  :type '(choice (const :tag "reStructuredText" rst)
                 (const :tag "Markdown" markdown))
  :safe #'symbolp)

(defcustom overblock-pydoc-command
  '((rst . "pandoc --mathjax --no-highlight --wrap=none -f rst")
    (markdown
     . "pandoc --mathjax --no-highlight --wrap=none -f markdown+hard_line_breaks-implicit_figures"))
  "How to turn a doc string into HTML, per markup.
An alist of (MARKUP . COMMAND), where MARKUP is a value of
`overblock-pydoc-markup' and COMMAND has the form of
`overblock-md-command': one shell command, or a list of candidates of
which the first one installed is used.  It replaces that variable
while a doc string renders.

Markdown with hard line breaks, because a numpy style parameter list
is lines: `name : type' and its indented description under it.
CommonMark has no definition list and joins an indented line to the
paragraph above, so all entries of a section become one paragraph.
With the extension each source line stays a line, which also suits a
rendering laid over its own lines.  The indent of the description is
lost, because CommonMark strips it from a continuation line.

No highlighting: shr reads no CSS class (see `overblock-md-command')."
  :type '(alist :key-type symbol
                :value-type (choice string (repeat string))))

(defcustom overblock-pydoc-modes
  '((rst . rst-mode)
    (markdown . markdown-mode))
  "The major mode that reads a doc string, per markup.
An alist of (MARKUP . MODE), where MARKUP is a value of
`overblock-pydoc-markup'.  `overblock-pydoc-edit' opens the source of a
doc string in it.

`rst-mode' is built in.  Name another mode here to use it instead."
  :type '(alist :key-type symbol :value-type function))

(defun overblock-pydoc--mode-for-markup ()
  "Return the major mode that reads a doc string of this buffer.
`overblock-pydoc-modes' says which.  A markup the option does not
name gets `rst-mode', and `text-mode' replaces a mode that is not
installed."
  (let ((mode (or (alist-get overblock-pydoc-markup overblock-pydoc-modes)
                  #'rst-mode)))
    (if (fboundp mode) mode #'text-mode)))

(defun overblock-pydoc--command-for-markup ()
  "Return the command that renders a doc string of this buffer.
`overblock-pydoc-command' says which.  A markup that it does not name
uses `overblock-md-command'."
  (or (alist-get overblock-pydoc-markup overblock-pydoc-command)
      overblock-md-command))

(defface overblock-pydoc-footer '((t :inherit shadow :underline t))
  "Face of the rule below a rendered doc string.
The underline closes what the overline of `overblock-bar' opens above.
A rule that a face draws runs from the start of the text of the bar,
the indentation of the doc string, to the edge of the window, so
nothing has to measure it.")

(defun overblock-pydoc--redraw ()
  "Draw the bars of every rendered doc string again, in every buffer.
A change to the buttons or a label shows on the bars at once."
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (when (bound-and-true-p overblock-pydoc-mode)
        (dolist (block (overblock-in (point-min) (point-max) 'pydoc))
          (overblock-delete block))
        (overblock-pydoc-render-buffer)))))

(defcustom overblock-pydoc-buttons
  '((edit ("" "✎" "edit") "Edit this doc string in its own buffer"
          overblock-pydoc-edit t))
  "The buttons on the bar of a rendered doc string, left to right.
An entry has the shape `overblock-buttons' reads.  A click on the
rendering already shows the source in place, so there is no button
for that."
  :type overblock-button-type
  :set (lambda (symbol value)
         (set-default symbol value)
         (overblock-pydoc--redraw)))

(defvar-keymap overblock-pydoc-map
  :doc "Keymap on a rendered doc string.
A click shows the source of the doc string, to edit it."
  "<mouse-1>" #'overblock-live-edit)

;;;; Which regions

(defun overblock-pydoc--doc-face-p (pos)
  "Return non-nil where font lock painted POS as a doc string.
python.el decides this in `python-info-docstring-p': a string that
opens a definition, a module or an assignment is documentation and has
`font-lock-doc-face', every other string has `font-lock-string-face'.
The tree-sitter fontifier paints the same face, so one path serves
`python-mode' and `python-ts-mode'.

The face property is read as a list, because a theme may add its own
face beside it."
  (memq 'font-lock-doc-face (ensure-list (get-text-property pos 'face))))

(defun overblock-pydoc--opens-a-line-p (start)
  "Return non-nil where START is where the code of its line begins.
Blanks and a string prefix (the `r' of a raw doc string and the
others) may come before it, because font lock paints the string and
not the letters that open it.

This rejects what a mispaired quote run leaves: a quote sequence in
the prose of one doc string ends it early, every later string pairs
the wrong way, and font lock follows that parse.  Such a region starts
in the middle of a line, and a rendering over it would cover code."
  (string-match-p "\\`[[:blank:]]*[rRbBuUfF]\\{0,2\\}\\'"
                  (buffer-substring-no-properties
                   (save-excursion (goto-char start) (pos-bol))
                   start)))

(defun overblock-pydoc--with-prefix (start)
  "Return START moved back over the letters that prefix a string.
Font lock paints the quotes of a doc string and not the letters that
open it.  The block must also cover the `r' of a raw doc string."
  (save-excursion
    (goto-char start)
    (skip-chars-backward "rRbBuUfF" (pos-bol))
    (point)))

(defun overblock-pydoc--string-end (start limit)
  "Return where the string that opens at START ends, at most LIMIT.
Return nil when it never ends: an unterminated doc string is not
rendered.  `parse-partial-sexp', told to stop at the end of a string,
walks from inside this one to just past its closing quotes.  The face
run of font lock is not used, because an escape sequence in the prose
has a face of its own and breaks the run.  `scan-sexps' is not used,
because it reads the first two of three quotes as an empty string."
  (save-excursion
    (goto-char start)
    ;; Start past the opening fence: `python-mode' gives the first of
    ;; three quotes the syntax of a plain string delimiter, so a scan
    ;; from between the quotes reads an empty string.
    (let* ((fence (if (looking-at-p "\"\"\"\\|'''") 3 1))
           (inside (min limit (+ start fence)))
           (state (syntax-ppss inside)))
      (when (nth 3 state)
        (let ((done (parse-partial-sexp inside limit nil nil state
                                        'syntax-table)))
          ;; Still in the string: there are no closing quotes, and a
          ;; block would cover the rest of the file.
          (unless (nth 3 done)
            ;; Two quotes more for a fence of three: the scan ends the
            ;; string at the first of the three closing quotes.
            (min limit (+ (point) (1- fence)))))))))

(defvar-local overblock-pydoc--strings-cache nil
  "The doc strings of this buffer and the tick they were found at.
A cons of (TICK . STRINGS).  The live cycle re-arms from
`post-command-hook', and without the cache each motion of point walks
the whole buffer again for the same answer.")

(defun overblock-pydoc--strings ()
  "Return the bounds of every doc string of the accessible buffer.
Each is a cons of the position of the opening quote and the one after
the closing quote.

Font lock says which strings are documentation (see
`overblock-pydoc--doc-face-p'), and the syntax scan says where each of
them ends."
  ;; A narrowed buffer is not cached: widening does not change
  ;; `buffer-chars-modified-tick'.
  (if (buffer-narrowed-p)
      (overblock-pydoc--walk (point-min) (point-max))
    (let ((tick (buffer-chars-modified-tick)))
      (unless (eql (car overblock-pydoc--strings-cache) tick)
        (setq overblock-pydoc--strings-cache
              (cons tick (overblock-pydoc--walk (point-min) (point-max)))))
      (cdr overblock-pydoc--strings-cache))))

(defun overblock-pydoc--walk (beg end)
  "Return the bounds of every doc string between BEG and END.
`overblock-pydoc--strings' is this behind a cache.  It calls
`font-lock-ensure' first, because jit lock paints only what has been
on the screen."
  (font-lock-ensure beg end)
  (save-excursion
    (let ((pos beg) found)
      (while (< pos end)
        (if (and (overblock-pydoc--doc-face-p pos)
                 (overblock-pydoc--opens-a-line-p pos))
            (let ((finish (overblock-pydoc--string-end pos end)))
              (when finish
                (push (cons (overblock-pydoc--with-prefix pos) finish) found))
              (setq pos (max (or finish 0) (1+ pos))))
          (setq pos (or (next-single-property-change pos 'face nil end)
                        end))))
      (nreverse found))))

;;;; What to render them with

(defconst overblock-pydoc--opening
  "\\`\\([rRbBuUfF]*\\)\\(\"\"\"\\|'''\\|\"\\|'\\)"
  "What opens a doc string: the letters that prefix it and its quotes.
The quotes are three double quotes, three single quotes, or one of
either.  The prefix is any of `r', `b', `u' or `f' in either case.")

(defun overblock-pydoc--opened-with (text)
  "Return (PREFIX QUOTES) of the doc string TEXT, or nil for neither.
What TEXT opens with is what it closes with and what a commit writes
back.  The prefix matters: in a raw doc string a backslash means
something else."
  (when (string-match overblock-pydoc--opening text)
    (list (match-string 1 text) (match-string 2 text))))

(defun overblock-pydoc--converter-text (beg end)
  "Return the prose of the doc string BEG..END as the converter reads it.
That is `overblock-pydoc--source', with every doctest of a Markdown doc
string in a fence: Markdown reads `>>>' as three nested quotes."
  (let ((source (overblock-pydoc--source beg end)))
    (if (eq overblock-pydoc-markup 'markdown)
        (overblock-pydoc--fence-doctests source)
      source)))

(defun overblock-pydoc--fence-doctests (text)
  "Return TEXT with each doctest outside a fence put in a pycon fence.
A doctest is a run of lines from one that begins with `>>>' after at
most three spaces, to the next blank line.  One four or more spaces in
is an indented code block already, and stays as it is.  So does one
that a fence holds; a fence ends at a line that begins with marks as
long as its own."
  (let (out fence doctest)
    ;; DOCTEST is the indent of the doctest in hand, FENCE the marks of
    ;; the fence in hand.
    (dolist (line (split-string text "\n"))
      (cond (doctest
             (when (string-blank-p line)
               (push (concat doctest "```") out)
               (setq doctest nil)))
            ;; A closing fence carries no info string.
            (fence
             (when (string-match-p
                    (concat "\\` \\{0,3\\}" (regexp-quote fence) "[`~]*[ \t]*\\'")
                    line)
               (setq fence nil)))
            ;; Four spaces in, a fence line is code text, and a backtick
            ;; after the marks makes inline code of it.
            ((string-match "\\` \\{0,3\\}\\(```+[^`]*\\'\\|~~~+\\)" line)
             (setq fence (replace-regexp-in-string "[^`~].*" ""
                                                   (match-string 1 line))))
            ;; Four spaces in, a doctest is in an indented code block
            ;; already.
            ((string-match "\\`\\( \\{0,3\\}\\)>>>" line)
             ;; The fence stands at the indent of the doctest, so a
             ;; doctest under a list item stays in the item.
             (setq doctest (match-string 1 line))
             (push (concat doctest "```pycon") out)))
      (push line out))
    (when doctest (push (concat doctest "```") out))
    (string-join (nreverse out) "\n")))

(defun overblock-pydoc--source (beg end)
  "Return the prose of the doc string BEG..END.
The quotes go, and so does the indentation every line shares with the
definition it belongs to: a doc string is written where the code stands
and reads as prose one column from the left."
  (let* ((text (buffer-substring-no-properties beg end))
         (quotes (nth 1 (overblock-pydoc--opened-with text)))
         (bare (if quotes
                   (string-remove-suffix
                    quotes
                    (substring text (+ (string-match (regexp-quote quotes) text)
                                       (length quotes))))
                 text))
         (lines (split-string bare "\n"))
         ;; The first line follows the quotes, so the common
         ;; indentation is measured on the lines after it. One test
         ;; filters and measures: `string-blank-p' and `[:blank:]'
         ;; disagree on a non-breaking space.
         (indents (seq-keep (lambda (line)
                              (string-match-p "[^[:blank:]]" line))
                            (cdr lines)))
         (indent (if indents (apply #'min indents) 0)))
    (string-trim-right
     (string-join (cons (string-trim (car lines))
                        (mapcar (lambda (line)
                                  (if (> (length line) indent)
                                      (substring line indent)
                                    (string-trim line)))
                                (cdr lines)))
                  "\n"))))

(defun overblock-pydoc--glyph ()
  "Return the glyph that marks a doc string, as this frame draws it.
The plain fallback is a page, not the diamond of a markdown cell,
because both can show in one notebook."
  (overblock-glyph "" "▯" "doc"))

(defun overblock-pydoc--rule (indent)
  "Return the row that closes a rendered doc string, INDENT columns in.
Only a rule: the label and the buttons are on the bar above.

It starts with a zero-width space, because `overblock--pieces' trims
blank lines off the ends, and a row of spaces is a blank line."
  (concat (propertize "\N{ZERO WIDTH SPACE}"
                      'face 'overblock-pydoc-footer)
          (overblock-bar "" "" "" 'overblock-pydoc-footer indent)))

(defun overblock-pydoc--bar (summary indent)
  "Return the bar of a rendered doc string, INDENT columns in.
The bar holds the glyph, the SUMMARY as its label, and the buttons at
the edge of the window, under the rule its face draws.

The summary is on the bar so the rendering has as many rows as the
doc string, and no row carries two lines.  A summary too long for the
room is cut with an ellipsis, as any label of a bar is."
  (overblock-bar (overblock-pydoc--glyph) (concat summary " ")
                 (overblock-buttons overblock-pydoc-buttons)
                 'overblock-bar indent))

(defun overblock-pydoc--bar-room ()
  "Return the columns the bar spends beside the summary.
The prose is filled that much narrower, so the first line of a long
summary fits the bar and is not cut there."
  (+ (string-width (overblock-pydoc--glyph))
     (string-width (overblock-buttons overblock-pydoc-buttons))
     3))

(defun overblock-pydoc--dressed (prose indent)
  "Return PROSE with its bar and its rule, for a doc string INDENT columns in.
The first line of PROSE is on the bar, the rest is under it, and a
rule closes it.  Prose of one line is only a bar, so it is not boxed
in among plain lines of code.

The bar starts where the block does, at the opening quote INDENT
columns in, and every later row starts at that column too.  The block
leaves the indentation of the source in view (the `:indent' of
`overblock-show')."
  (pcase-let ((`(,summary . ,body) (split-string prose "\n")))
    (if body
        (string-join `(,(overblock-pydoc--bar summary indent)
                       ,@body
                       ,(overblock-pydoc--rule indent))
                     "\n")
      (overblock-pydoc--bar summary indent))))

(defun overblock-pydoc--show (beg end &optional html)
  "Render the doc string BEG..END over its own source, and return it.
HTML is the answer of the converter for this doc string, when a caller
sent the whole buffer through one process.

Every row starts at the column of BEG, not at the indentation of its
line.  The block leaves that many columns of every source line in
view, so the indentation stays buffer text (with any indentation guide
on it).  The first row starts where the block does, so both must use
the column of BEG, which for a raw doc string includes its prefix."
  (and-let* ((source (overblock-pydoc--converter-text beg end))
             ((not (string-empty-p source)))
             (indent (save-excursion (goto-char beg) (current-column)))
             (rendered
              ;; The width is nil where no window shows the buffer,
              ;; which leaves the filling to shr. The command is bound
              ;; here, not as a clause, so a nil command does not abort
              ;; the render.
              (let ((overblock-md-width
                     (overblock-md-columns (+ indent (overblock-pydoc--bar-room))))
                    (overblock-md-command (overblock-pydoc--command-for-markup))
                    (overblock-md-math-face 'font-lock-doc-face))
                ;; A conversion that fails gives an empty block, so the
                ;; doc string does not go to the converter on every pass.
                (if-let* ((prose (overblock-md-rendered source html)))
                    (overblock-pydoc--dressed (string-trim-right prose "\n+")
                                              indent)
                  "")))
             ((overblock-show-rendering
               beg end rendered 'font-lock-doc-face
               :kind 'pydoc
               :indent indent
               :keymap overblock-pydoc-map
               :help-echo "mouse-1: edit this doc string")))))

;;;; When

;;;###autoload
(defun overblock-pydoc-render-buffer ()
  "Render every doc string of the buffer that wants it.
One asynchronous converter process does all of them, so the reader
does not wait.  `overblock-md-render-regions' is the batch, and says
what happens to a doc string the reader reaches while the process
runs.  `overblock-live-start' calls this again whenever the reader
stops."
  (interactive)
  (let ((overblock-md-command (overblock-pydoc--command-for-markup)))
    (overblock-md-render-regions (overblock-pydoc--strings)
                                 'pydoc #'overblock-pydoc--converter-text
                                 #'overblock-pydoc--show)))

(defun overblock-pydoc--put (beg end prose)
  "Write the edited PROSE back into the doc string BEG..END and render it.
The quotes go back on, and every line but the first is indented to
the column of the doc string, which undoes `overblock-pydoc--source'."
  (let* ((text (buffer-substring-no-properties beg end))
         (opened (or (overblock-pydoc--opened-with text) '("" "\"\"\"")))
         (prefix (nth 0 opened))
         (lines (split-string (string-trim-right prose) "\n"))
         ;; A one-quote string cannot hold a newline: prose of several
         ;; lines goes back between three of that quote.
         (quotes (if (and (cdr lines) (= (length (nth 1 opened)) 1))
                     (make-string 3 (aref (nth 1 opened) 0))
                   (nth 1 opened)))
         (indent (save-excursion (goto-char beg) (current-indentation)))
         (pad (make-string indent ?\s))
         (body (string-join (cons (car lines)
                                  (mapcar (lambda (line)
                                            (if (string-blank-p line)
                                                ""
                                              (concat pad line)))
                                          (cdr lines)))
                            "\n")))
    (goto-char beg)
    (delete-region beg end)
    ;; The prefix goes back: without the `r' of a raw string every
    ;; backslash in the prose means something else. The closing quotes
    ;; of a doc string of several lines go on a line of their own, as
    ;; in PEP 257.
    (insert prefix quotes body
            (if (cdr lines) (concat "\n" pad quotes) quotes))
    (overblock-pydoc--show beg (point))))

;;;###autoload
(defun overblock-pydoc-edit (&optional event)
  "Edit the doc string at point, or the one clicked in EVENT.
The prose opens in its own buffer, without the quotes and the
indentation, in the mode `overblock-pydoc-modes' names for the markup
of this buffer.  `overblock-edit-commit' puts it back and renders it,
and `overblock-edit-abort' discards the edit."
  (interactive (list last-input-event))
  (overblock-goto-event event)
  (if-let* ((block (overblock-at 'pydoc)))
      (overblock-edit-in-buffer
       (overlay-start block) (overlay-end block)
       (list :name (format "*overblock-pydoc: %s:%d*" (buffer-name)
                           (line-number-at-pos (overlay-start block)))
             :label "doc string"
             :mode (overblock-pydoc--mode-for-markup)
             :text #'overblock-pydoc--source
             :put #'overblock-pydoc--put))
    (user-error "No rendered doc string here")))

;;;; The mode

;;;###autoload
(define-minor-mode overblock-pydoc-mode
  "Render the doc strings of this buffer as documentation.
A click on a rendered doc string shows its source, which renders again
when point leaves it.  Point moving into one changes nothing, so the
code around a doc string is edited with the prose in view.

A converter and shr render the prose.  `overblock-pydoc-command' names
the converter for the markup in `overblock-pydoc-markup'."
  :lighter " PyDoc"
  (when overblock-pydoc-mode
    (overblock-only-in 'overblock-pydoc-mode 'python-base-mode))
  (if overblock-pydoc-mode
      (progn
        (setq-local overblock-live-source-at-point nil)
        (overblock-live-start 'pydoc #'overblock-pydoc-render-buffer))
    (overblock-live-stop 'pydoc)
    (kill-local-variable 'overblock-live-source-at-point)))

(provide 'overblock-pydoc)
;;; overblock-pydoc.el ends here
