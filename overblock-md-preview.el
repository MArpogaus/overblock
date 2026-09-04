;;; overblock-md-preview.el --- Markdown rendered where you write it  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Marcel Arpogaus

;; Author: Marcel Arpogaus <znepry.necbtnhf@tznvy.pbz>
;; Assisted-by: Claude:claude-opus-5
;; Keywords: text, markdown, convenience
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

;; `overblock-md-preview-mode' shows a markdown buffer as it will read
;; and keeps it editable: the text is rendered over its own source, and
;; a click on a rendering shows the source it stands on.  Edit it, move
;; on, and it is rendered again once you have stopped.
;;
;; The unit is the markdown block — the run of lines between two blank
;; ones, and a fenced piece of code whole.  A line of markdown is often
;; not markdown by itself: a row of a table needs the rows around it, a
;; line of a fenced block is code, and an item needs its list.  The
;; block goes to the converter in one piece and the rendering is dealt
;; back over its lines, a piece to a line, which is what lets a tall
;; rendering scroll like text.
;;
;; It is one of the two demonstrations that `overblock' is a layer and
;; not a part of the notebook — see `overblock-pydoc-mode' for the
;; other.  What this file holds is the answer to three questions:
;; which regions to render, what to render them with, and when to
;; render them.  The showing, the hiding of the source under the
;; rendering, and the edit that makes a rendering stale are the layer's.

;;; Code:

(require 'overblock)
(require 'overblock-md)

(defgroup overblock-md-preview nil
  "Markdown rendered over the lines it is written on."
  :group 'text
  :prefix "overblock-md-preview-")

(defcustom overblock-md-preview-idle 0.2
  "Seconds of quiet before a line is rendered again.
The line point leaves is rendered when the reader stops moving, not on
every command: a held down `C-n' would otherwise render a line for
every keypress it repeats."
  :type 'number)

(defvar-keymap overblock-md-preview-map
  :doc "Keymap on a rendered line.
A click shows the source of the line, which is what a reader wants of
a rendering they mean to edit."
  "<mouse-1>" #'overblock-live-edit)


;;;; Which regions

(defun overblock-md-preview--regions (beg end)
  "Return every block of markdown between BEG and END.
Each is a cons of where the block starts and where it ends.  A block is
what markdown calls one: the run of lines between two blank ones, and a
fenced piece of code with whatever blank lines it holds.

The block and not the line, because a line of markdown is often not
markdown at all.  Measured against pandoc: the three lines of a table
render to a row apiece with the rule between them turned into empty
cells, a line of a fenced block renders as a paragraph of code, and the
lines of a list each render as a list of one.  The whole block reaches
the converter and the rendering is dealt back over its lines by
`overblock-show', which hangs a piece on each of them.

Walked from the top of the buffer whatever BEG says, because that is the
only way to know whether BEG stands inside a fence."
  (save-excursion
    (goto-char (point-min))
    (let ((fenced nil)
          (from nil)
          ;; Where the last line of text ended: a block that runs to
          ;; the end of the buffer ends there and not at `point-max',
          ;; which is past the newline of that line.
          (last nil)
          regions)
      (while (not (eobp))
        (let ((fence (looking-at-p "[[:blank:]]*\\(```\\|~~~\\)"))
              (blank (looking-at-p "[[:blank:]]*$")))
          (cond
           ;; A fence opens a block of its own and closes it, so the
           ;; code between two of them is never cut at a blank line.
           (fence
            (setq last (pos-eol))
            (if fenced
                (progn (setq fenced nil)
                       (when from
                         (push (cons from (pos-eol)) regions)
                         (setq from nil)))
              (unless from (setq from (pos-bol)))
              (setq fenced t)))
           ((and blank (not fenced))
            (when from
              (push (cons from last) regions)
              (setq from nil)))
           (t (setq last (pos-eol))
              (unless from (setq from (pos-bol))))))
        (forward-line 1))
      (when from (push (cons from last) regions))
      ;; Only what the caller asked for, and nothing of no length.
      (seq-filter (lambda (region)
                    (and (< (car region) (cdr region))
                         (>= (car region) beg)
                         (<= (car region) end)))
                  (nreverse regions)))))

;;;; What to render them with

(defun overblock-md-preview--show (beg end &optional html)
  "Render the markdown BEG..END over its own source, and return the block.
HTML is what `overblock-md-html-batch' answered for this block, where a
caller sent the whole buffer through one process.  The rendering is
dealt over the lines of the region by `overblock-show', a piece to a
line, which is what lets a tall block scroll like text."
  (when-let* ((source (string-trim (buffer-substring-no-properties beg end)))
              ((not (string-empty-p source)))
              (rendered (overblock-md-rendered source html))
              ;; A line that renders to nothing of its own — a lone
              ;; HTML comment — is left as it is rather than blanked.
              ((not (string-empty-p (string-trim rendered)))))
    (when-let* ((block
                 (overblock-show
                  beg end
                  :kind 'md-preview
                  :over (overblock-fill-props
                         (overblock-faced rendered 'default)
                         'keymap overblock-md-preview-map
                         'help-echo "mouse-1: edit this text")
                  :keymap overblock-md-preview-map
                  :help-echo "mouse-1: edit this text")))
      ;; An edit the mode did not see coming — a replacement over the
      ;; buffer, a macro, an undo — leaves a rendering of text that has
      ;; changed.  The reader's own typing never reaches this: point
      ;; landing on the line takes the rendering off first.
      (overblock-stale-when-edited block)
      block)))


;;;; When to render them

(defun overblock-md-preview-render-buffer ()
  "Render every block of the buffer that is not already rendered.
One converter process for the whole buffer: the blocks go through
`overblock-md-html-batch' together, and each falls back to its own
conversion where that answers nothing.  `overblock-live-render-buffer'
would render them one process at a time, which is what this is for."
  (interactive)
  (when (overblock-md-program)
    (let* ((blocks (seq-remove (lambda (block)
                                 (overblock-in (car block) (cdr block)
                                               'md-preview))
                               (overblock-md-preview--regions (point-min)
                                                              (point-max))))
           (htmls (and (cdr blocks)
                       (overblock-md-html-batch
                        (mapcar (lambda (block)
                                  (buffer-substring-no-properties
                                   (car block) (cdr block)))
                                blocks)))))
      (dolist (block blocks)
        (overblock-md-preview--show (car block) (cdr block) (pop htmls))))))

;;;###autoload
(define-minor-mode overblock-md-preview-mode
  "Render every line of this buffer over its own markdown source.
The line point is on shows its source, so it can be edited where it
stands; the rest of the buffer reads as it will look.  A click on a
rendered line puts point there.

`overblock-md-command' is what converts the markdown, and the mode does
nothing where none of its candidates is installed."
  :lighter " MdPrev"
  (if overblock-md-preview-mode
      (progn
        ;; The whole buffer first, in one converter process, and then
        ;; the cycle: the layer would start it a process at a time.
        (overblock-md-preview-render-buffer)
        (overblock-live-start 'md-preview
                              #'overblock-md-preview--regions
                              #'overblock-md-preview--show
                              overblock-md-preview-idle))
    (overblock-live-stop)))

(provide 'overblock-md-preview)
;;; overblock-md-preview.el ends here
