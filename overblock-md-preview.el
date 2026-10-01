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
;; The unit is the markdown block: the run of lines between two blank
;; lines or fences, or a whole fenced block of code.  A line of markdown is often
;; not markdown by itself.  A row of a table needs the rows around it, a
;; line of a fenced block is code, and an item needs its list.  The
;; block goes to the converter in one piece.  The rendering is dealt
;; back over its lines, a piece to a line, so a tall rendering scrolls
;; like text.
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

(defun overblock-md-preview-fences (end)
  "Return the bounds of every fenced code block up to END.
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
end of the buffer."
  (save-excursion
    (goto-char (point-min))
    (let (regions open fence)
      (while (re-search-forward "^[[:blank:]]*\\(```+\\|~~~+\\)" end t)
        (let ((this (match-string-no-properties 1))
              (bare (looking-at-p "[[:blank:]]*$")))
          (cond ((null open) (setq open (pos-bol) fence this))
                ;; Of another kind, or shorter than the opening fence:
                ;; content of the block.
                ((or (not (eq (aref this 0) (aref fence 0)))
                     (< (length this) (length fence))))
                (bare (push (cons open (pos-eol)) regions)
                      (setq open nil fence nil))
                (t (push (cons open (1- (pos-bol))) regions)
                   (setq open (pos-bol) fence this)))))
      (when open (push (cons open (point-max)) regions))
      (nreverse regions))))

(defun overblock-md-preview--margin-p (pos)
  "Return non-nil where the line at POS begins at the left margin."
  (save-excursion (goto-char pos) (not (looking-at-p "[ \t]"))))

(defun overblock-md-preview--interrupts-p (fence from)
  "Return non-nil where the FENCE ends the paragraph that began at FROM.
It does unless it is indented under a list item: the nearest line
above it that begins at the left margin begins an item.  FROM nil is
no paragraph."
  (or (not from)
      (overblock-md-preview--margin-p fence)
      (not (overblock-md-preview--in-item-p fence))))

(defun overblock-md-preview--in-item-p (pos)
  "Return non-nil where the indented line at POS belongs to a list item."
  (save-excursion
    (goto-char pos)
    (while (and (zerop (forward-line -1))
                (looking-at-p "[ \t]\\|[ \t]*$")))
    (overblock-md-preview--item-p (point))))

(defun overblock-md-preview--item-p (pos)
  "Return non-nil where the line at POS begins a list item."
  (save-excursion
    (goto-char pos)
    (looking-at-p "[ \t]*\\(?:[-+*]\\|[0-9]+[.)]\\)[ \t]")))

(defun overblock-md-preview-paragraphs (end fences)
  "Return the bounds of every paragraph up to END, FENCES aside.
A paragraph is the run of lines between two blank ones, or between a
blank line and a fence: a fence ends the paragraph that touches it,
unless it is indented under a list item, to which it belongs.
The lines a fence holds are not read here:
`overblock-md-preview-fences' has them already, and a blank line inside
one ends no paragraph."
  (save-excursion
    (goto-char (point-min))
    (let (regions from last)
      (while (< (point) end)
        ;; FENCES and this walk are both in order, so each fence is
        ;; reached once, not tested on every line.
        (let ((fence (and fences (>= (point) (caar fences)))))
          (cond
           ((or (and fence (overblock-md-preview--interrupts-p (caar fences) from))
                (and (not fence) (looking-at-p "[[:blank:]]*$")))
            ;; A blank line ends a paragraph, and so does a fence that
            ;; touches it, unless the fence is indented under an item.
            (when from (push (cons from last) regions))
            (setq from nil))
           ((not fence)
            (setq last (pos-eol)
                  from (or from (pos-bol)))))
          ;; A fence indented under a list item belongs to it: the walk
          ;; jumps over it and the item goes on.
          (when fence
            (goto-char (cdar fences))
            (setq fences (cdr fences))))
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

(defun overblock-md-preview-regions (beg end &optional prose-only)
  "Return every block of markdown between BEG and END, in order.
Each is a cons of the start and the end of the block.  A block is a
whole fenced block of code, or else the run of lines between two blank
lines or fences.  PROSE-ONLY leaves the fenced blocks out, for a caller whose
fences hold code, such as the chunks of an Rmd file.

The unit is the block, not the line: a converter renders each line of
a table, a fenced block or a list wrongly by itself.  The whole block
goes to the converter and `overblock-show' deals the rendering back
over its lines.

The walk starts at the top of the buffer whatever BEG is, because only
that tells whether BEG is inside a fence."
  (let* ((fences (overblock-md-preview-fences end))
         (paragraphs (overblock-md-preview-paragraphs end fences)))
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

(defun overblock-md-preview--show (beg end &optional html)
  "Render the markdown BEG..END over its own source, and return the block.
HTML is the answer of `overblock-md-html-batch-async' for this block,
when a caller sent the whole buffer through one process.
`overblock-show' deals the rendering over the lines of the region, a
piece to a line, so a tall block scrolls like text."
  (when-let* ((source (string-trim (buffer-substring-no-properties beg end)))
              ((not (string-empty-p source)))
              (rendered (let ((overblock-md-width (overblock-md-columns)))
                          (overblock-md-rendered source html))))
    (overblock-show-rendering beg end rendered 'default
                              :kind 'md-preview
                              :keymap overblock-md-preview-map
                              :help-echo "mouse-1: edit this text")))

;;;; When to render them

;;;###autoload
(defun overblock-md-preview-render-buffer ()
  "Render every block of the buffer that is not rendered yet.
One asynchronous converter process does the whole buffer, so the
reader does not wait for it.  A block falls back to its own conversion
when the answer comes back without the marker between every pair.

`overblock-live-start' calls this again whenever the reader stops.
`overblock-md-render-regions' is the batch."
  (interactive)
  (overblock-md-render-regions
   (funcall overblock-md-preview-regions-function (point-min) (point-max))
   'md-preview #'buffer-substring-no-properties #'overblock-md-preview--show))

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
