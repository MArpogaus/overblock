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
;; an HTML comment.  `overblock-md-regions' finds them.  The block goes
;; to the converter in one piece, and the rendering is dealt back over
;; its lines, a piece to a line, so a tall rendering scrolls like text.
;;
;; `overblock-pydoc-mode' and `overblock-rmd-mode' use the same live
;; cycle, each in a package of its own.
;; `overblock-md-preview-regions-function' is where another mode says
;; which regions it renders.
;;
;; This file says which regions to render, what to render them with,
;; and when.  `overblock-md' knows what a block of markdown is.  The
;; showing, the hiding of the source under the rendering, and the edit
;; that makes a rendering stale belong to the layer.

;;; Code:

(require 'overblock)
(require 'overblock-md)

(defgroup overblock-md-preview nil
  "Markdown rendered over the lines it is written on."
  :group 'text
  :group 'overblock
  :prefix "overblock-md-preview-")

(defvar-local overblock-md-preview-regions-function
  #'overblock-md-regions
  "Function that returns the regions of this buffer to render.
It is called with no arguments, and returns a list of conses in
order.  The default returns every block of markdown.

A mode that reads part of the buffer as something else sets this.  In
an Rmd file the fenced chunks are R code that runs, so `overblock-rmd'
returns the prose alone.")


;;;; What to render them with

(defun overblock-md-preview--show (beg end &optional html)
  "Render the markdown BEG..END over its own source, and return the block.
HTML is the answer of `overblock-md-html-batch-async' for this block,
when a caller sent the whole buffer through one process.
`overblock-show' deals the rendering over the lines of the region, a
piece to a line, so a tall block scrolls like text."
  (overblock-md-show beg end (overblock-md-source beg end) html 'default
                     :kind 'md-preview
                     :keymap overblock-live-map
                     :help-echo "mouse-1: edit this text"))

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
   (funcall overblock-md-preview-regions-function)
   'md-preview #'overblock-md-source #'overblock-md-preview--show))

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
