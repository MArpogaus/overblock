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
;; and keeps it editable: every line is rendered over its own source,
;; and the line point is on shows that source.  Move onto a line to
;; edit it, move away and it is rendered again.  A click puts point on
;; the line, which comes to the same thing.
;;
;; The unit is the line, because the line is what a reader clicks and
;; what they edit.  A fenced code block is left as it is: its lines are
;; code, and each of them alone is not markdown.
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

(defun overblock-md-preview--lines (beg end)
  "Return every line between BEG and END that is worth rendering.
Each is a cons of the start and the end of the line.  A blank line
renders to nothing, and the lines of a fenced code block are code
rather than markdown — the fences with them, since a fence alone
renders as itself.

Walked from the top of the buffer whatever BEG says, because that is
the only way to know whether BEG is inside a fence, and in one pass:
asking the question line by line walked the buffer once a line, which
is a second of work in a file of two thousand."
  (save-excursion
    (goto-char (point-min))
    (let ((fenced nil)
          lines)
      (while (< (point) end)
        (if (looking-at-p "[[:blank:]]*\\(```\\|~~~\\)")
            (setq fenced (not fenced))
          (when (and (not fenced)
                     (>= (point) beg)
                     (not (looking-at-p "[[:blank:]]*$")))
            (push (cons (pos-bol) (pos-eol)) lines)))
        (forward-line 1))
      (nreverse lines))))

;;;; What to render them with

(defun overblock-md-preview--show (beg end &optional html)
  "Render the line BEG..END over its own source, and return the block.
HTML is what `overblock-md-html-batch' answered for this line, where a
caller rendered the whole buffer in one process."
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
                         'help-echo "mouse-1: edit this line")
                  :keymap overblock-md-preview-map
                  :help-echo "mouse-1: edit this line")))
      ;; An edit the mode did not see coming — a replacement over the
      ;; buffer, a macro, an undo — leaves a rendering of text that has
      ;; changed.  The reader's own typing never reaches this: point
      ;; landing on the line takes the rendering off first.
      (overblock-stale-when-edited block)
      block)))


;;;; When to render them

(defun overblock-md-preview-render-buffer ()
  "Render every line of the buffer that is not already rendered.
One converter process for the whole buffer: the lines go through
`overblock-md-html-batch' together, and each falls back to its own
conversion where that answers nothing.  `overblock-live-render-buffer'
would render them one process at a time, which is what this is for."
  (interactive)
  (when (overblock-md-program)
    (let* ((lines (seq-remove (lambda (line)
                                (overblock-in (car line) (cdr line)
                                              'md-preview))
                              (overblock-md-preview--lines (point-min)
                                                           (point-max))))
           (htmls (and (cdr lines)
                       (overblock-md-html-batch
                        (mapcar (lambda (line)
                                  (buffer-substring-no-properties
                                   (car line) (cdr line)))
                                lines)))))
      (dolist (line lines)
        (overblock-md-preview--show (car line) (cdr line) (pop htmls))))))

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
                              #'overblock-md-preview--lines
                              #'overblock-md-preview--show
                              overblock-md-preview-idle))
    (overblock-live-stop)))

(provide 'overblock-md-preview)
;;; overblock-md-preview.el ends here
