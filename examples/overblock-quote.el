;;; overblock-quote.el --- Quoted lines read as a quote  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Marcel Arpogaus

;; Author: Marcel Arpogaus <znepry.necbtnhf@tznvy.pbz>
;; Assisted-by: Claude:claude-opus-5
;; Keywords: convenience
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

;; An example, and no package: the smallest render mode on overblock.
;; Every run of lines that start with `>' in a text buffer shows as one
;; block in the face of a comment, without the markers.  A click on a block shows its
;; source, and the block renders again when point leaves it.
;;
;; To try it, evaluate this file with M-x load-file, open a text file
;; or a mail with quoted lines, and turn on M-x overblock-quote-mode.
;;
;; It has the sections of a render mode: Options, Regions, Rendering,
;; Mode.  A mode whose regions come from the layer, such as
;; overblock-md-preview, has no Regions.  The live cycle of the layer
;; does the rest: `overblock-live-start' calls
;; `overblock-quote-render-buffer' when the reader stops, and
;; `overblock-live-map' answers the click.
;; docs/custom-mode.org walks through this file.

;;; Code:

(require 'overblock)

;;;; Options

(defgroup overblock-quote nil
  "Quoted lines rendered as a quote."
  :group 'overblock
  :prefix "overblock-quote-")

(defface overblock-quote '((t :inherit (italic font-lock-comment-face)))
  "Face of a rendered quote.")

;;;; Regions

(defun overblock-quote--regions ()
  "Return every run of quoted lines of the buffer, in order.
Each is a cons of the start of its first line and the start of the
line after it, so the region holds whole lines."
  (save-excursion
    (goto-char (point-min))
    (let (regions)
      (while (re-search-forward "^>" nil t)
        (let ((beg (pos-bol)))
          (while (and (zerop (forward-line 1)) (looking-at-p ">")))
          (push (cons beg (point)) regions)))
      (nreverse regions))))

;;;; Rendering

(defun overblock-quote--show (beg end)
  "Render the quote BEG..END over its own source, and return the block."
  (overblock-show-rendering
   beg end
   (replace-regexp-in-string "^> ?" "" (buffer-substring-no-properties beg end))
   'overblock-quote
   :kind 'quote
   :keymap overblock-live-map
   :help-echo "mouse-1: edit this quote"))

(defun overblock-quote-render-buffer ()
  "Render every quote of the buffer that wants it.
`overblock-live-start' calls this again whenever the reader stops."
  (interactive)
  (pcase-dolist (`(,beg . ,end) (overblock-quote--regions))
    (when (overblock-live-wanted-p beg end 'quote)
      (overblock-quote--show beg end))))

;;;; Mode

(define-minor-mode overblock-quote-mode
  "Show every run of quoted lines as a quote, without the markers.
A click on a quote shows its source.  The quote renders again when
point leaves it."
  :lighter " Quote"
  (overblock-only-in 'overblock-quote-mode 'text-mode)
  (if overblock-quote-mode
      (overblock-live-start 'quote #'overblock-quote-render-buffer)
    (overblock-live-stop 'quote)))

(provide 'overblock-quote)
;;; overblock-quote.el ends here
