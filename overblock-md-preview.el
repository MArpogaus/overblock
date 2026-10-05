;;; overblock-md-preview.el --- Markdown rendered where you write it  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Marcel Arpogaus

;; Author: Marcel Arpogaus <znepry.necbtnhf@tznvy.pbz>
;; Assisted-by: Claude:claude-opus-5
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

;; `overblock-md-preview-mode' shows a markdown buffer as it will read,
;; and keeps it editable.  The mode renders the text over its own
;; source, and a click on a rendering shows the source it stands on.
;;
;; Turn the mode on with M-x overblock-md-preview-mode, or add it to
;; `markdown-mode-hook'.
;;
;; docs/overblock-md.org has the details.

;;; Code:

(require 'overblock)
(require 'overblock-md)

;;;; Rendering

(defun overblock-md-preview--show (beg end &optional html)
  "Render the markdown BEG..END over its own source, and return the block.
HTML is the answer of the converter for it, when a batch converted the
whole buffer."
  (overblock-md-show beg end (overblock-md-source beg end) html 'default
                     :kind 'md-preview
                     :keymap overblock-live-map
                     :help-echo "mouse-1: edit this text"))

;;;###autoload
(defun overblock-md-preview-render-buffer ()
  "Render every block of the buffer that wants it.
One asynchronous converter process does all of them, so the reader
does not wait.  `overblock-live-start' calls this again whenever the
reader stops."
  (interactive)
  (overblock-md-render-regions
   (overblock-md-regions)
   'md-preview #'overblock-md-source #'overblock-md-preview--show))

;;;; Mode

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
  (overblock-only-in 'overblock-md-preview-mode 'markdown-mode)
  (if overblock-md-preview-mode
      (overblock-live-start 'md-preview #'overblock-md-preview-render-buffer)
    (overblock-live-stop 'md-preview)))

(provide 'overblock-md-preview)
;;; overblock-md-preview.el ends here
