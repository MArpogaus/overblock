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
;; the documentation they are: the triple quotes and the indentation go,
;; the markup is rendered, and the code around them is untouched.  Move
;; point into one and its source comes back to be edited; move away and
;; it reads as documentation again.  A click puts point in it, which
;; comes to the same thing.
;;
;; Which strings are documentation is what font lock has already
;; decided: python.el paints the doc string of a module, a definition
;; or an assignment with `font-lock-doc-face' and every other string
;; with `font-lock-string-face'.  A string that is data is left alone,
;; and the mode needs no parser and no grammar of its own — it works in
;; `python-mode' and in `python-ts-mode' alike.
;;
;; It is the second of the two modes the layer ships, and the shorter
;; one: the cycle it runs is `overblock-live-start', the same call
;; `overblock-md-preview-mode' makes, and what is left here is which
;; regions and rendered with what.

;;; Code:

(require 'overblock)
(require 'overblock-md)

(defgroup overblock-pydoc nil
  "Python doc strings rendered where they are written."
  :group 'languages
  :group 'overblock
  :prefix "overblock-pydoc-")

(define-obsolete-variable-alias 'overblock-pydoc-idle 'overblock-live-idle "1.0")

;;;###autoload (put 'overblock-pydoc-markup 'safe-local-variable #'symbolp)
(defcustom overblock-pydoc-markup 'markdown
  "The markup the doc strings of this buffer are written in.
It says which command of `overblock-pydoc-command' renders them and
which major mode of `overblock-pydoc-modes' reads them, so that the
rendering and the buffer `overblock-pydoc-edit' opens agree.

Markdown is the default: it is what a numpy style doc string carries
today under its section titles — a fenced block, a pipe table, bold —
as mkdocstrings reads it, and the Markdown reader keeps the parameter
entries on their lines (see `overblock-pydoc-command').  A project
whose doc strings are reStructuredText for Sphinx sets this to `rst',
which is worth doing per project rather than globally:

  ;;; .dir-locals.el
  ((python-base-mode . ((overblock-pydoc-markup . rst))))

One doc string, one markup: a reader that is given the other one lays
out what it does not know as prose.  Measured on a numpy doc string
holding a pipe table, the reStructuredText reader ran the table
together into a paragraph of pipes, and the Markdown reader laid it
out in columns and left the Sphinx roles standing in the prose."
  :type '(choice (const :tag "reStructuredText" rst)
                 (const :tag "Markdown" markdown))
  :safe #'symbolp)

(defcustom overblock-pydoc-command
  '((rst . "pandoc --mathjax --no-highlight -f rst")
    (markdown . "pandoc --mathjax --no-highlight -f markdown+hard_line_breaks"))
  "How to turn a doc string into HTML, per markup.
An alist of (MARKUP . COMMAND), where MARKUP is a value of
`overblock-pydoc-markup' and COMMAND is read as `overblock-md-command'
is read: one shell command, or a list of candidates of which the first
one installed is used.  It stands in that variable's place while a doc
string is rendered.

Markdown with hard line breaks, because a numpy style parameter list
is lines: `name : type' and its indented description under it.
CommonMark has no definition list and reads an indented line after a
paragraph line as more of the paragraph, so every entry of a section
ran together into one — measured, six parameters as a single
paragraph.  With the extension each source line stays a line, which is
also the shape a rendering laid over its own lines wants; what is lost
is the indent of the description, which CommonMark strips from a
continuation line.

No highlighting, for the reason `overblock-md-command' gives: shr
reads no CSS class, so what pandoc spends on painting a code block is
spent for nothing."
  :type '(alist :key-type symbol
                :value-type (choice string (repeat string))))

(defcustom overblock-pydoc-modes
  '((rst . rst-mode)
    (markdown . markdown-mode))
  "The major mode that reads a doc string, per markup.
An alist of (MARKUP . MODE), where MARKUP is a value of
`overblock-pydoc-markup'.  `overblock-pydoc-edit' opens the source of a
doc string in it.

`rst-mode' is built in and knows what Python's own tools read;
`markdown-mode' reads what a Markdown project writes.  Name another
mode here where you prefer one."
  :type '(alist :key-type symbol :value-type function))

(defun overblock-pydoc--mode-for-markup ()
  "Return the major mode that reads a doc string of this buffer.
`overblock-pydoc-modes' says which, and `rst-mode' answers for a
markup the option says nothing about."
  (or (alist-get overblock-pydoc-markup overblock-pydoc-modes) #'rst-mode))

(defun overblock-pydoc--command-for-markup ()
  "Return the command that renders a doc string of this buffer.
`overblock-pydoc-command' says which, read as `overblock-md-command'
is read, and a markup the option says nothing about renders with
whatever `overblock-md-command' holds."
  (or (alist-get overblock-pydoc-markup overblock-pydoc-command)
      overblock-md-command))

(defface overblock-pydoc-footer '((t :inherit shadow :underline t))
  "Face of the rule below a rendered doc string.
The underline closes what the overline of `overblock-bar' opened above
it.  A rule a face draws runs the width of the row it is on: it begins
where the bar's own text begins, which is the column the doc string is
indented to, and it ends where the row ends, which is the window's
edge.  Nothing has to measure either.")

(defun overblock-pydoc--redraw ()
  "Draw the bars of every rendered doc string again, in every buffer.
A button list or a label the reader changes reaches the bars at once;
without this it waited for something else to render the doc string
again — a window changing width, or the file opened afresh."
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (when (bound-and-true-p overblock-pydoc-mode)
        (dolist (block (overblock-in (point-min) (point-max) 'pydoc))
          (overblock-delete block))
        (overblock-pydoc-render-buffer)))))

(defcustom overblock-pydoc-buttons
  '((edit ("\uea73" "✎" "edit") "Edit this doc string in its own buffer"
          overblock-pydoc-edit t))
  "The buttons on the bar of a rendered doc string, left to right.
An entry is the shape `overblock-buttons' reads.

One button, and a codicon like every other glyph of the layer: a
click on the rendering already gives the source back where it stands,
which is what its tooltip says, so a button for it said the same thing
twice."
  :type overblock-button-type
  :set (lambda (symbol value)
         (set-default symbol value)
         (overblock-pydoc--redraw)))

(defvar-keymap overblock-pydoc-map
  :doc "Keymap on a rendered doc string.
A click shows the source of the doc string, which is what a reader
wants of a rendering they mean to edit."
  "<mouse-1>" #'overblock-live-edit)

;;;; Which regions

(defun overblock-pydoc--doc-face-p (pos)
  "Return non-nil where font lock painted POS as a doc string.
python.el decides this for its own fontification, in
`python-info-docstring-p\': a string that opens a definition, a module
or an assignment is documentation and wears `font-lock-doc-face\',
every other string wears `font-lock-string-face\'.  Its tree-sitter
fontifier paints the same face, so `python-mode\' and `python-ts-mode\'
are served by the one path and no grammar is needed.

The face is asked for as a list: font lock paints one face on a doc
string and a theme may add its own beside it."
  (memq 'font-lock-doc-face (ensure-list (get-text-property pos 'face))))

(defun overblock-pydoc--opens-a-line-p (start)
  "Return non-nil where START is where the code of its line begins.
Blanks may stand before it, and a string prefix — the `r\' of a raw
doc string and the rest — because font lock paints the string and not
the letters that open it.

What this rejects is what a mispaired quote run leaves behind: a
quote sequence inside the prose of one doc string ends it early, every
string after it pairs the wrong way round, and font lock inherits the
parse.  Such a region begins in the middle of a line — measured, at
column 71 of a line indented to four — and a rendering laid over it is
prose drawn over code.  Left as source it is merely unrendered."
  (string-match-p "\\`[[:blank:]]*[rRbBuUfF]\\{0,2\\}\\'"
                  (buffer-substring-no-properties
                   (save-excursion (goto-char start) (pos-bol))
                   start)))

(defun overblock-pydoc--with-prefix (start)
  "Return START moved back over the letters that prefix a string.
Font lock paints the quotes of a doc string and not the letters that
open it, so the `r\' of a raw doc string stood to the left of the bar
that covers the rest of the line — a letter of code beside a rendering,
which is exactly what a block is supposed not to leave behind."
  (save-excursion
    (goto-char start)
    (skip-chars-backward "rRbBuUfF" (pos-bol))
    (point)))

(defun overblock-pydoc--string-end (start limit)
  "Return where the string that opens at START ends, at most LIMIT.
Nil when it never ends: an unterminated doc string is not rendered.
The syntax scan answers it: `parse-partial-sexp\' told to stop at the
end of a string walks from inside this one to just past its closing
quotes.  Not the end of what font lock painted, which is shorter — an
escape sequence in the prose wears a face of its own and breaks the
run in two, measured in `python-mode\' and in `python-ts-mode\' alike;
and not `scan-sexps\', which reads the first two of three quotes as an
empty string."
  (save-excursion
    (goto-char start)
    ;; From past the opening fence, and not from between its first two
    ;; quotes: `python-mode' gives the first of three quotes the syntax
    ;; of a plain string delimiter, so a scan begun there reads those
    ;; two as a string of nothing and every doc string came out two
    ;; characters long.
    (let* ((fence (if (looking-at-p "\"\"\"\\|'''") 3 1))
           (inside (min limit (+ start fence)))
           (state (syntax-ppss inside)))
      (when (nth 3 state)
        (let ((done (parse-partial-sexp inside limit nil nil state
                                        'syntax-table)))
          ;; Still in the string where the walk stopped: the closing
          ;; quotes are not there, and a doc string that ends nowhere
          ;; is not one to render.  Answered `limit' before, and the
          ;; block was drawn over the rest of the file — the two lines
          ;; under a half-typed """ among them.
          (unless (nth 3 done)
            ;; And two quotes more for a fence of three: the scan ends
            ;; the string at the first of the three closing quotes,
            ;; which is the same syntax the opening fence is given.
            (min limit (+ (point) (1- fence)))))))))

(defvar-local overblock-pydoc--strings-cache nil
  "The doc strings of this buffer and the tick they were found at.
A cons of (TICK . STRINGS).  The live cycle re-arms from
`post-command-hook', so a reader who only moves point pays the walk
again for an answer that cannot have changed: measured in his
configuration, 1.05 milliseconds a pause on 60 doc strings, and the
walk is over the whole buffer whatever the reader touched.")

(defun overblock-pydoc--strings (beg end)
  "Return the bounds of every doc string between BEG and END.
Each is a cons of the position of the opening quote and the one after
the closing quote.

Font lock says which strings are documentation — see
`overblock-pydoc--doc-face-p\' — and the syntax scan says where each of
them ends.  `font-lock-ensure\' first: jit lock has painted only what
has been on the screen, and a doc string below the window would
otherwise be no doc string at all."
  ;; The whole buffer and nothing else is asked for by every caller
  ;; here, so that is what is kept.  A narrower question walks as it
  ;; always did — and a narrowing makes every question a narrow one,
  ;; whatever the bounds say: `buffer-chars-modified-tick' does not
  ;; change when the buffer is widened again, so the answer for one
  ;; defun would have stood for the whole file.
  (if (and (= beg (point-min)) (= end (point-max)) (not (buffer-narrowed-p)))
      (let ((tick (buffer-chars-modified-tick)))
        (unless (eql (car overblock-pydoc--strings-cache) tick)
          (setq overblock-pydoc--strings-cache
                (cons tick (overblock-pydoc--walk beg end))))
        (cdr overblock-pydoc--strings-cache))
    (overblock-pydoc--walk beg end)))

(defun overblock-pydoc--walk (beg end)
  "Return the bounds of every doc string between BEG and END.
`overblock-pydoc--strings' is this behind a cache."
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
A doc string is written four ways — three double quotes, three single
ones, or one of either — and any of them may carry `r', `b', `u' or
`f' in either case.")

(defun overblock-pydoc--opened-with (text)
  "Return (PREFIX QUOTES) of the doc string TEXT, or nil for neither.
What TEXT opens with is what it closes with and what a commit has to
write back.  A commit that wrote three double quotes over an r-string
turned a raw doc string into an ordinary one, where a backslash means
something else: measured, an unchanged commit of a doc string reading
r\"\"\"Match \\d+ digits.\"\"\" gave back a string with an invalid escape
in it, and \\n, \\t and \\b in such a doc string become control
characters."
  (when (string-match overblock-pydoc--opening text)
    (list (match-string 1 text) (match-string 2 text))))

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
         ;; The first line stands after the quotes and shares no
         ;; indentation with the rest, so the common indentation is
         ;; measured on the lines that follow it.
         ;; One question, asked once: `string-blank-p' reads
         ;; [ \t\n\r] and `[:blank:]' reads every space Unicode
         ;; has, so a line of one non-breaking space passed the
         ;; filter and then answered nil to the match — and `min'
         ;; over a nil signalled, from mode-on, from the idle timer
         ;; and from the converter's sentinel.  A line pasted out of
         ;; a browser is how one gets there.
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
The plain candidate is a page and not the diamond a markdown cell
wears: both modes are on in one notebook, and a frame without the icon
font drew the same mark for a cell and for a doc string."
  (overblock-glyph "" "▯" "doc"))

(defun overblock-pydoc--rule (indent)
  "Return the row that closes a rendered doc string, INDENT columns in.
A rule and nothing else: the label and the buttons stand on the bar
above, and saying both twice said nothing the second time.

It opens with a zero-width space, because `overblock--pieces\' trims
the blank lines off the ends of what it is given and a row of spaces is
a blank line: the rule was trimmed away and the doc string had no
footer at all."
  (concat (propertize "\N{ZERO WIDTH SPACE}"
                      'face 'overblock-pydoc-footer)
          (overblock-bar "" "" "" 'overblock-pydoc-footer indent)))

(defun overblock-pydoc--bar (summary indent)
  "Return the bar of a rendered doc string, INDENT columns in.
The glyph, the SUMMARY where the label of a bar stands, and the buttons
at the window\'s edge, all under the rule the bar\'s face draws.

The summary rides the bar because the bar is the one row a rendering
adds to its doc string.  Under the bar, on a row of its own, it made
the rendering one line longer than the source, and the first row then
carried two lines: the summary shared the quote\'s line number with
the bar or stood on the next, depending on nothing but how the lines
of the rest happened to fill — measured, two doc strings of the same
shape rendered two ways.  On the bar the rendering has as many rows as
the doc string, nothing shares a row, and a doc string of one line
reads as the first row of one of twenty.  A summary too long for the
room is cut with an ellipsis, as any label of a bar is."
  (overblock-bar (overblock-pydoc--glyph) (concat summary " ")
                 (overblock-buttons overblock-pydoc-buttons)
                 'overblock-bar indent))

(defun overblock-pydoc--dressed (prose indent)
  "Return PROSE with its bar and its rule, for a doc string INDENT columns in.
The first line of PROSE rides the bar; the rest stands under it, and a
rule closes it.  Prose of one line is all bar: a rule under one row
would box it in, and a boxed line of prose among plain lines of code
is a loud way to say very little.

The bar is the first line of what the block shows, so it begins where
the block does — the opening quote, already INDENT columns in — and
every row after it begins at that column too: the block leaves the
indentation of the source in view under the rendering, which is
`overblock-show\''s `:indent\'."
  (pcase-let ((`(,summary . ,body) (split-string prose "\n")))
    (if body
        (string-join `(,(overblock-pydoc--bar summary indent)
                       ,@body
                       ,(overblock-pydoc--rule indent))
                     "\n")
      (overblock-pydoc--bar summary indent))))

(defun overblock-pydoc--show (beg end &optional html)
  "Render the doc string BEG..END over its own source, and return it.
HTML is what the converter answered for this doc string, where a caller
sent the whole buffer through one process.

Every row begins at the column BEG itself begins at, measured, and not
at the indentation of its line: the block leaves that many columns of
every source line in view, so the indentation stays the buffer's own
text — with whatever an indentation guide painted on it — and a
description line indented deeper is covered from that column on.  The
first row hangs where the block does, so the two have to be the same
column: a raw doc string begins one column in from its code, past the
letter that prefixes its quotes, and the rendering of one stood a
column out of line."
  (when-let* ((source (overblock-pydoc--source beg end))
              ((not (string-empty-p source)))
              (indent (save-excursion (goto-char beg) (current-column)))
              (rendered
               ;; The prose has the window less the columns it is
               ;; indented by; nil where no window shows the buffer,
               ;; which leaves the filling to shr.
               ;; The command is bound here and not in the clause list
               ;; above: as a clause, a nil `overblock-pydoc-command'
               ;; would abort the render instead of leaving the
               ;; rendering to shr with whatever `overblock-md-command'
               ;; holds.
               (let ((overblock-md-width (overblock-md-columns indent))
                     (overblock-md-command (overblock-pydoc--command-for-markup)))
                 (when-let* ((prose (overblock-md-rendered source html)))
                   (overblock-pydoc--dressed (string-trim-right prose "\n+")
                                             indent))))
              (block (overblock-show-rendering
                      beg end rendered 'font-lock-doc-face
                      :kind 'pydoc
                      :indent indent
                      :keymap overblock-pydoc-map
                      :help-echo "mouse-1: edit this doc string")))
    block))

;;;; When

;;;###autoload
(defun overblock-pydoc-render-buffer ()
  "Render every doc string of the buffer that wants it.
In one converter process, asked and not waited for: measured, eight doc
strings cost 145 milliseconds one process apiece and the reader felt
every one; this way they cost 7 and the renderings arrive together a
moment later.  `overblock-md-render-regions' is the batch, and says
what happens to a doc string the reader has reached while the process
ran.  This is what `overblock-live-start' is given, and it is called
again whenever the reader stops."
  (interactive)
  (when-let* ((regions (seq-filter
                        (lambda (region)
                          (overblock-live-wanted-p (car region) (cdr region)
                                                   'pydoc))
                        (overblock-pydoc--strings (point-min) (point-max)))))
    (let ((overblock-md-command (overblock-pydoc--command-for-markup)))
      (overblock-md-render-regions regions 'pydoc #'overblock-pydoc--source
                                   #'overblock-pydoc--show))))

(defun overblock-pydoc--put (beg end prose)
  "Write the edited PROSE back into the doc string BEG..END and render it.
The quotes go back on and every line but the first is indented to where
the doc string stood, which is what Python\'s own tools expect of a doc
string and what `overblock-pydoc--source\' took off."
  (let* ((text (buffer-substring-no-properties beg end))
         (opened (or (overblock-pydoc--opened-with text) '("" "\"\"\"")))
         (prefix (nth 0 opened))
         (quotes (nth 1 opened))
         (indent (save-excursion (goto-char beg) (current-indentation)))
         (pad (make-string indent ?\s))
         (lines (split-string (string-trim-right prose) "\n"))
         (body (string-join (cons (car lines)
                                  (mapcar (lambda (line)
                                            (if (string-blank-p line)
                                                ""
                                              (concat pad line)))
                                          (cdr lines)))
                            "\n")))
    (goto-char beg)
    (delete-region beg end)
    ;; What the doc string opened with is what it gets back: the `r' of
    ;; a raw string is part of the string and not decoration, and a
    ;; commit that dropped it changed what every backslash in the prose
    ;; means.
    ;;
    ;; The closing quotes go on a line of their own where the doc
    ;; string has more than one, which is how PEP 257 writes one — and
    ;; only for the triple quotes, because a one-quote string cannot
    ;; hold a newline at all.
    (insert prefix quotes body
            (if (and (cdr lines) (= (length quotes) 3))
                (concat "\n" pad quotes)
              quotes))
    (overblock-pydoc--show beg (point))))

;;;###autoload
(defun overblock-pydoc-edit (&optional event)
  "Edit the doc string at point, or the one clicked in EVENT.
The prose opens in its own buffer, without the quotes and without the
indentation, in the mode `overblock-pydoc-modes\' names for the markup
of this buffer.
`overblock-edit-commit\' puts it back and renders it;
`overblock-edit-abort\' discards the edit."
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
A click on a rendered doc string gives its source back, and it is
rendered again once point has left it; point moving into one changes
nothing, so the code around a doc string is edited with the prose in
view.

A converter and shr render the prose: `overblock-pydoc-command' names
the converter for the markup `overblock-pydoc-markup' says the doc
strings are written in."
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
