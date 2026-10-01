;;; overblock-md-preview.el --- Markdown rendered where you write it  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Marcel Arpogaus

;; Author: Marcel Arpogaus <znepry.necbtnhf@tznvy.pbz>
;; Assisted-by: Claude:claude-opus-5
;; Keywords: text, markdown, convenience
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

;; `overblock-md-preview-mode' shows a markdown buffer as it will read,
;; and keeps it editable.  The text is rendered over its own source, and
;; a click on a rendering shows the source it stands on.  After an edit,
;; the block renders again when point has left it.
;;
;; The unit is the markdown block: the front matter, the run of lines
;; between two blank lines or fences, a whole fenced block of code, or
;; an HTML comment, see `overblock-md-preview--comment'.
;; A fence indented under a list item is part of that item.  A line of
;; markdown is often not markdown by itself.  A row of a table needs the
;; rows around it, a line of a fenced block is code, and an item needs
;; its list.  The block goes to the converter in one piece.  The
;; rendering is dealt back over its lines, a piece to a line, so a tall
;; rendering scrolls like text.
;;
;; `overblock-pydoc-mode' and `overblock-rmd-mode' use the same live
;; cycle, each in a package of its own.
;; `overblock-md-preview-regions-function' is where another mode says
;; which regions it renders.
;;
;; This file says which regions to render, what to render them with,
;; and when.  The showing, the hiding of the source under the rendering,
;; and the edit that makes a rendering stale belong to the layer.

;;; Code:

(require 'overblock)
(require 'overblock-md)

(defgroup overblock-md-preview nil
  "Markdown rendered over the lines it is written on."
  :group 'text
  :group 'overblock
  :prefix "overblock-md-preview-")

(defvar-keymap overblock-md-preview-map
  :doc "Keymap on a rendered block.
A click shows the source of the block, to edit it."
  "<mouse-1>" #'overblock-live-edit)

;;;; Which regions

(defconst overblock-md-preview-closing-fence-regexp
  "^[[:blank:]]*\\(?:```+\\|~~~+\\)[[:blank:]]*$"
  "What a line that closes a fenced block looks like.
The fence alone: in CommonMark an opening fence can name its language
and a closing fence cannot.  A caller that hides the fence of a block
asks this before it hides a line.")

(defconst overblock-md-preview--fence-regexp
  "^\\( *\\)\\(\\(?:[-+*]\\|[0-9]+[.)]\\) +\\)?\\(```+\\|~~~+\\)"
  "What a fence line looks like: indentation, a list marker, the marks.")

(defconst overblock-md-preview--fence-or-comment
  (concat overblock-md-preview--fence-regexp "\\|^<!--")
  "A fence line, or the start of an HTML comment at the left margin.")

(defun overblock-md-preview-fences (end &optional comments)
  "Return the bounds of the front matter and every fenced block up to END.
Each is a cons of the start of the opening fence line and the end of
the closing one.  Public because `overblock-rmd' takes the R chunks of
an Rmd file from it.

A fence opens a block and the next fence closes it, whatever blank
lines stand between them.  As in CommonMark, the closing fence is of
the same kind and at least as long, so three backquotes inside a ~~~
block, or inside a longer run of backquotes, stay one block.

A fence that names a language opens a block and closes none.  When it
comes while a block of the same kind is open, it ends that block where
it is, so an unclosed Rmd chunk does not take the header of the next
chunk as its closing fence.  A fence that is never closed runs to the
end of the buffer.

With COMMENTS, an HTML comment at the left margin is one region up to
its end, and a fence in it opens nothing; see
`overblock-md-preview--comment' for where one begins.  Split at a
blank line, its closing half would show as text, and a fence in it
would open a block.  In an Rmd file a chunk in a comment still runs,
so `overblock-rmd' asks for none."
  (save-excursion
    (goto-char (point-min))
    (let ((regions (overblock-md-preview--front-matter end))
          block)
      (while (re-search-forward (if comments
                                    overblock-md-preview--fence-or-comment
                                  overblock-md-preview--fence-regexp)
                                end t)
        (if (match-beginning 3)
            (pcase-let ((`(,next . ,done) (overblock-md-preview--fence block)))
              (when done (push done regions))
              (setq block next))
          (when-let* ((comment (overblock-md-preview--comment end block)))
            (push comment regions))))
      (when block (push (cons (car block) (point-max)) regions))
      (nreverse regions))))

(defun overblock-md-preview--front-matter (end)
  "Return a list of the bounds of the front matter, and move past it.
Front matter is a YAML block at the top of the buffer, from a line of
three dashes, with a line of text after it, to one of three dashes or
three dots, before END.  A
blank line in it would split it, and the converter would read its
halves as a rule and a heading.
Return nil, and leave point, where there is none."
  (when (looking-at-p "---[ \t]*\n[ \t]*[^ \t\n]")
    (let ((from (point)))
      (forward-line 1)
      (if (re-search-forward "^\\(?:---\\|\\.\\.\\.\\)[ \t]*$" end t)
          (list (cons from (pos-eol)))
        (goto-char from)
        nil))))

(defconst overblock-md-preview--before-html
  "[[:blank:]]*$\\|#\\| \\{0,3\\}\\(?:```\\|~~~\\)\\|\\(?:---\\|\\.\\.\\.\\)[ \t]*$"
  "What a line above an HTML block of one line looks like.
A blank line, a heading, a fence or the end of front matter: after a
line of a paragraph, a comment of one line is part of the paragraph.")

(defun overblock-md-preview--front-matter-p (beg end)
  "Return non-nil where BEG..END is the front matter of the buffer."
  (and (= beg (point-min))
       (when-let* ((matter (save-excursion
                             (goto-char beg)
                             (car (overblock-md-preview--front-matter
                                   (point-max))))))
         (= (cdr matter) end))))

(defun overblock-md-preview--comment (end block)
  "Return the bounds of the HTML comment that begins a block on this line.
It begins one where BLOCK, the open fenced block, is nil, and either it
runs over more lines, or the line above ends no paragraph, see
`overblock-md-preview--before-html'.  A comment of one line inside a
paragraph is part of the paragraph; one of more lines hides what it
holds, whatever stands above it.  It ends on the line that holds its end,
before END, whatever blank lines stand in it; one that does not end
is no region."
  (let ((from (pos-bol)))
    (when (and (not block)
               (or (not (save-excursion (search-forward "-->" (pos-eol) t)))
                   (save-excursion
                     (goto-char from)
                     (or (bobp)
                         (progn (forward-line -1)
                                (looking-at-p
                                 overblock-md-preview--before-html)))))
               (search-forward "-->" end t))
      (cons from (pos-eol)))))

(defun overblock-md-preview--fence (block)
  "Read the fence line just matched, with BLOCK open, and return (NEXT . DONE).
BLOCK is (OPEN FENCE LIMIT) for the block that the fence line at OPEN
opened, its marks FENCE, its LIMIT that of
`overblock-md-preview--limit'; or nil where no block is open.  NEXT is
the block open after this line, and DONE the bounds of a block this
line ended, or nil."
  (pcase-let* ((`(,open ,fence ,limit) block)
               (this (match-string-no-properties 3))
               (item (match-beginning 2))
               (from (- (match-beginning 3) (pos-bol)))
               (opened (lambda ()
                         (list (pos-bol) this
                               (overblock-md-preview--limit from item)))))
    (cond ((overblock-md-preview--too-deep-p (length (match-string 1))
                                             item limit)
           (list block))
          ;; A backtick after the marks makes inline code of it.
          ((and (eq (aref this 0) ?`) (looking-at-p "[^\n]*`")) (list block))
          ((null block) (list (funcall opened)))
          ;; Of another kind, or shorter than the opening fence: content
          ;; of the block.
          ((or (not (eq (aref this 0) (aref fence 0)))
               (< (length this) (length fence)))
           (list block))
          ((looking-at-p "[[:blank:]]*$") (cons nil (cons open (pos-eol))))
          (t (cons (funcall opened) (cons open (1- (pos-bol))))))))

(defun overblock-md-preview--limit (from item)
  "Return the deepest column a fence can close the block at.
A fence FROM columns in opens the block.  ITEM is non-nil where it
stands on the line of a list item.  The limit is three columns in, or
three deeper than an opening fence at or right of the content column
of a list item, or on the line of one."
  (if (or item
          (>= from (or (overblock-md-preview--in-item-p (pos-bol))
                       most-positive-fixnum)))
      (+ from 3)
    3))

(defun overblock-md-preview--too-deep-p (indent item limit)
  "Return non-nil where a fence INDENT columns in is code text.
ITEM is non-nil where the fence stands on the line of a list item.
LIMIT is that of `overblock-md-preview--limit' for the open block, or
nil where no block is open.

In an open block, a fence on an item line is content, and so is one
deeper than LIMIT.  Outside, a fence four columns in opens a block only
under a list item.  The columns are counted in the text: a rendering
hides the indentation from `current-indentation'."
  (if limit
      (or item (> indent limit))
    (and (> indent 3)
         (not (overblock-md-preview--in-item-p (pos-bol))))))

(defun overblock-md-preview--margin-p (pos)
  "Return non-nil where the line at POS begins at the left margin."
  (save-excursion (goto-char pos) (not (looking-at-p "[ \t]"))))

(defun overblock-md-preview--ends-p (fence from every)
  "Return non-nil where the line at point ends the paragraph from FROM.
A blank line does, and so does FENCE, the start of a fence reached
here, unless it is indented under a list item: the nearest line above
it that begins at the left margin begins an item.  With EVERY, each
fence ends its paragraph.

An item ends a later paragraph of an item, which is indented and no
item itself: in one block with it, the converter reads the item as
more text of that paragraph."
  (cond (fence
         (or every
             (overblock-md-preview--margin-p fence)
             (not (overblock-md-preview--in-item-p fence))))
        ((looking-at-p "[[:blank:]]*$"))
        (from (and (overblock-md-preview--item-p (point))
                   (not (overblock-md-preview--margin-p from))
                   (not (overblock-md-preview--item-p from))
                   (overblock-md-preview--in-item-p from)))))

(defun overblock-md-preview--in-item-p (pos)
  "Return the content column of the list item the line at POS is under.
That is the column of the text after the marker of the nearest line
above POS that begins at the left margin, where that line begins an
item.  Return nil elsewhere."
  (save-excursion
    (goto-char pos)
    (while (and (zerop (forward-line -1))
                (looking-at-p "[ \t]\\|[ \t]*$")))
    (when (looking-at "[ \t]*\\(?:[-+*]\\|[0-9]+[.)]\\)[ \t]+")
      (- (match-end 0) (point)))))

(defun overblock-md-preview--item-p (pos)
  "Return non-nil where the line at POS begins a list item."
  (save-excursion
    (goto-char pos)
    (looking-at-p "[ \t]*\\(?:[-+*]\\|[0-9]+[.)]\\)[ \t]")))

(defun overblock-md-preview-paragraphs (end fences &optional every)
  "Return the bounds of every paragraph up to END, FENCES aside.
A paragraph is the run of lines between two blank ones, or between a
blank line and a fence: a fence ends the paragraph that touches it,
unless it is indented under a list item, to which it belongs.  A list
item also ends a later paragraph of the item before it, which is
indented and no item itself.  With EVERY, each fence ends one: the
fences of an Rmd file are chunks.  The lines a fence holds are not
read here: `overblock-md-preview-fences' has them already, and a
blank line inside one ends no paragraph."
  (save-excursion
    (goto-char (point-min))
    (let (regions from last)
      (while (< (point) end)
        ;; FENCES and this walk are both in order, so each fence is
        ;; reached once, not tested on every line.
        (let ((fence (and fences (>= (point) (caar fences)))))
          (when (overblock-md-preview--ends-p (and fence (caar fences))
                                               from every)
            (when from (push (cons from last) regions))
            (setq from nil))
          ;; A line of text goes on with the paragraph or begins one, as
          ;; an item does that ends a paragraph.
          (unless (or fence (looking-at-p "[[:blank:]]*$"))
            (setq last (pos-eol)
                  from (or from (pos-bol))))
          ;; A fence indented under a list item belongs to it: the walk
          ;; jumps over it and the item goes on, to its end at least.
          (when fence
            (goto-char (cdar fences))
            ;; Read only by a paragraph that goes on over the fence.
            (setq last (cdar fences)
                  fences (cdr fences))))
        (forward-line 1))
      (when from (push (cons from last) regions))
      (nreverse regions))))

(defvar-local overblock-md-preview-regions-function
  #'overblock-md-preview-regions
  "Function that returns the regions of this buffer to render.
It is called with the bounds to look at, and returns a list of conses
in order.  The default returns every block of markdown.

A mode that reads part of the buffer as something else sets this.  In
an Rmd file the fenced chunks are R code that runs, so `overblock-rmd'
returns the prose alone.")

(defun overblock-md-preview--outside (fences paragraphs)
  "Return the FENCES that no paragraph of PARAGRAPHS holds.
A fence under a list item is part of the item's paragraph, and two
blocks over the same lines would let neither render.  Both lists are
in order, so one walk does it."
  (let (out)
    (dolist (fence fences)
      (while (and paragraphs (<= (cdar paragraphs) (car fence)))
        (pop paragraphs))
      (unless (and paragraphs (<= (caar paragraphs) (car fence)))
        (push fence out)))
    (nreverse out)))

(defun overblock-md-preview-regions (beg end &optional prose-only)
  "Return every block of markdown between BEG and END, in order.
Each is a cons of the start and the end of the block.  A block is a
whole fenced block of code, the front matter at the top, an HTML
comment at the left margin (see `overblock-md-preview--comment'), or
else the run of lines between two blank lines or fences.  A fence
indented under a list item is part of that item.  PROSE-ONLY leaves the fenced
blocks and the front matter out, and reads no comments, for a caller
whose fences hold code, such as the chunks of an Rmd file.

The unit is the block, not the line: a converter renders each line of
a table, a fenced block or a list wrongly by itself.  The whole block
goes to the converter and `overblock-show' deals the rendering back
over its lines.

The walk starts at the top of the buffer whatever BEG is, because only
that tells whether BEG is inside a fence."
  (let* ((all (overblock-md-preview-fences end (not prose-only)))
         (paragraphs (overblock-md-preview-paragraphs end all prose-only))
         (fences (overblock-md-preview--outside all paragraphs)))
    (seq-filter (lambda (region)
                  (and (< (car region) (cdr region))
                       (<= beg (car region) end)))
                (if prose-only
                    paragraphs
                  ;; Not `:key': the keyword form of `sort' is Emacs 30,
                  ;; and this package supports 29.1.
                  (sort (append fences paragraphs)
                        (lambda (a b) (< (car a) (car b))))))))

;;;; What to render them with

(defun overblock-md-preview--source (beg end)
  "Return the markdown BEG..END, less the indentation of its first line.
Only under a list item: a later paragraph of an item is indented under
it, and the converter reads a fence indented deeper than the line
before it as text.  Elsewhere the indentation makes a code block.
Only spaces go, so a tab stays the converter's to read.  A fenced
block goes out closed under its opening marks; see
`overblock-md-preview--closed'."
  (let* ((text (if (overblock-md-preview--front-matter-p beg end)
                   ;; It renders to nothing, and a YAML error in it would
                   ;; fail the batch.
                   ""
                 (buffer-substring-no-properties beg end)))
         (indent (if (overblock-md-preview--in-item-p beg)
                     (or (string-match-p "[^ ]" text) 0)
                   0)))
    (overblock-md-preview--closed
     (replace-regexp-in-string (format "^ \\{0,%d\\}" indent) "" text))))

(defun overblock-md-preview--closed (text)
  "Return TEXT with its closing fence under its opening one.
Where TEXT opens with a fence, its own closing fence, if any, gives way
to one at the column of the opening marks.  The converter then reads
the block as the preview pairs it, alone and in a batch: a closing
fence at the margin under a list item would end the list, and pair
with a fence of the next block.  Other TEXT goes to
`overblock-md-preview--comment-closed'."
  (if (and (string-match (concat "\\` \\{0,3\\}\\(?:\\(?:[-+*]\\|[0-9]+[.)]\\) +\\)?"
                                 "\\(```+\\|~~~+\\)\\([^\n]*\\)")
                         text)
           (not (and (eq (aref (match-string 1 text) 0) ?`)
                     (string-search "`" (match-string 2 text)))))
      (let ((marks (match-string 1 text))
            (column (match-beginning 1)))
        (concat (replace-regexp-in-string
                 (format "\n[ \t]*%s\\{%d,\\}[ \t]*\n?\\'"
                         (regexp-quote (substring marks 0 1)) (length marks))
                 "" text)
                "\n" (make-string column ?\s) marks))
    (overblock-md-preview--comment-closed text)))

(defun overblock-md-preview--comment-closed (text)
  "Return TEXT with the half of an HTML comment it lacks.
A comment cut at a blank line, as in an Rmd file where a comment is no
block, sends a half that opens it and one that closes it.  Each gets
the other mark, so the converter shows neither as text, and the half
that opens it does not take the markers of a batch."
  (cond ((and (string-prefix-p "<!--" text)
              (not (string-search "-->" text)))
         (concat text "\n-->"))
        ((and (string-suffix-p "-->" (string-trim-right text))
              (not (string-search "<!--" text)))
         (concat "<!--\n" text))
        (t text)))

(defun overblock-md-preview--show (beg end &optional html)
  "Render the markdown BEG..END over its own source, and return the block.
HTML is the answer of `overblock-md-html-batch-async' for this block,
when a caller sent the whole buffer through one process.
`overblock-show' deals the rendering over the lines of the region, a
piece to a line, so a tall block scrolls like text."
  (when-let* ((source (overblock-md-preview--source beg end))
              ;; A conversion that fails gives an empty block, which
              ;; keeps the source in view and the region from going to
              ;; the converter again on every idle pass.
              (rendered (or (let ((overblock-md-width (overblock-md-columns)))
                              (overblock-md-rendered source html))
                            "")))
    (overblock-show-rendering beg end rendered 'default
                              :kind 'md-preview
                              :keymap overblock-md-preview-map
                              :help-echo "mouse-1: edit this text")))

;;;; When to render them

;;;###autoload
(defun overblock-md-preview-render-buffer ()
  "Render every block of the buffer that is not rendered yet.
One asynchronous converter process does the whole buffer, so the
reader does not wait for it.

`overblock-live-start' calls this again whenever the reader stops.
`overblock-md-render-regions' is the batch."
  (interactive)
  (overblock-md-render-regions
   (funcall overblock-md-preview-regions-function (point-min) (point-max))
   'md-preview #'overblock-md-preview--source #'overblock-md-preview--show))

;;;###autoload
(define-minor-mode overblock-md-preview-mode
  "Render every block of this buffer over its own markdown source.
A block without a rendering stays source while point is in it, so it
can be written in place, and renders when point leaves it.  A rendered
block stays rendered when point moves through it.  A click on a
rendering shows its source to edit it.

`overblock-md-command' converts the markdown.  The mode does nothing
when none of its candidates is installed."
  :lighter " MdPrev"
  ;; `overblock-rmd-mode' renders its prose through this same live
  ;; cycle, and this mode would take the cycle over. A message, not an
  ;; error: a configuration can hook both modes onto
  ;; `markdown-mode-hook'.
  (when overblock-md-preview-mode
    (overblock-only-in 'overblock-md-preview-mode 'markdown-mode))
  (cond
   ;; Refused, and nothing else: the two modes share the kind, so the
   ;; last branch would stop the cycle of `overblock-rmd-mode'.
   ((and overblock-md-preview-mode (bound-and-true-p overblock-rmd-mode))
    (setq overblock-md-preview-mode nil)
    (message "overblock-md-preview: off, overblock-rmd-mode renders this prose"))
   (overblock-md-preview-mode
    (overblock-live-start 'md-preview #'overblock-md-preview-render-buffer))
   (t (overblock-live-stop 'md-preview))))

(provide 'overblock-md-preview)
;;; overblock-md-preview.el ends here
