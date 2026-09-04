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

(defun overblock-md-preview--fences (end)
  "Return the bounds of every fenced code block up to END.
A fence opens a block and the next one closes it, whatever blank lines
stand between them, so the code inside is never cut in two.  A fence
that is never closed runs to the end of the buffer, which is what a
reader sees while they are still typing it."
  (save-excursion
    (goto-char (point-min))
    (let (regions open)
      (while (re-search-forward "^[[:blank:]]*\\(```\\|~~~\\)" end t)
        (if open
            (progn (push (cons open (pos-eol)) regions)
                   (setq open nil))
          (setq open (pos-bol))))
      (when open (push (cons open (point-max)) regions))
      (nreverse regions))))

(defun overblock-md-preview--paragraphs (end fences)
  "Return the bounds of every paragraph up to END, FENCES aside.
A paragraph is the run of lines between two blank ones.  The lines a
fence holds are not read here: `overblock-md-preview--fences' has them
already, and a blank line inside one ends no paragraph."
  (save-excursion
    (goto-char (point-min))
    (let (regions from last)
      (while (< (point) end)
        (cond
         ;; A fence in one jump, and the fence with it.  FENCES arrive
         ;; in order and this walk is in order too, so each is reached
         ;; once and then done with — asked of every line instead, the
         ;; question cost lines times fences, which was 64 milliseconds
         ;; of every pass in a document of 900 lines with a dozen
         ;; fences, and every pass is one the reader waits through.
         ((and fences (>= (point) (caar fences)))
          (goto-char (cdar fences))
          (setq fences (cdr fences)))
         ((looking-at-p "[[:blank:]]*$")
          (when from (push (cons from last) regions))
          (setq from nil))
         (t (setq last (pos-eol))
            (unless from (setq from (pos-bol)))))
        (forward-line 1))
      (when from (push (cons from last) regions))
      (nreverse regions))))

(defun overblock-md-preview--regions (beg end)
  "Return every block of markdown between BEG and END, in order.
Each is a cons of where the block starts and where it ends.  A block is
what markdown calls one: a fenced piece of code whole, and otherwise
the run of lines between two blank ones.

The block and not the line, because a line of markdown is often not
markdown at all.  Measured against pandoc: the three lines of a table
render to a row apiece with the rule between them turned into empty
cells, a line of a fenced block renders as a paragraph of code, and the
lines of a list each render as a list of one.  The whole block reaches
the converter and the rendering is dealt back over its lines by
`overblock-show', which hangs a piece on each of them.

Read from the top of the buffer whatever BEG says, because that is the
only way to know whether BEG stands inside a fence."
  (let ((fences (overblock-md-preview--fences end)))
    (seq-filter (lambda (region)
                  (and (< (car region) (cdr region))
                       (<= beg (car region) end)))
                (sort (append fences
                              (overblock-md-preview--paragraphs end fences))
                      :key #'car))))

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
  "Render every block of the buffer that is not rendered yet.
One converter process for the whole buffer rather than one for each
block, and nothing waits for it: measured on a document of 216 lines,
turning the mode on cost the reader 454 milliseconds of which the
converter was 440, and asked for like this it costs 8 and the
renderings arrive a moment later.  A block falls back to its own
conversion where the answer comes back without the marker between
every pair.

`overblock-live-wanted-p' says which blocks want rendering, and says
it again when the answer arrives: the reader has clicked, typed and
moved on while the process ran.  This is what `overblock-live-start' is
given, and it is called again whenever the reader stops."
  (interactive)
  (when-let* (((overblock-md-program))
              (blocks (seq-filter
                       (lambda (block)
                         (overblock-live-wanted-p (car block) (cdr block)
                                                  'md-preview))
                       (overblock-md-preview--regions (point-min)
                                                      (point-max)))))
    (overblock-md-html-batch-async
     (mapcar (lambda (block)
               (buffer-substring-no-properties (car block) (cdr block)))
             blocks)
     (lambda (htmls)
       (dolist (block blocks)
         (let ((html (pop htmls)))
           (when (overblock-live-wanted-p (car block) (cdr block)
                                          'md-preview)
             (overblock-md-preview--show (car block) (cdr block) html))))))))

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
      (overblock-live-start 'md-preview
                            #'overblock-md-preview-render-buffer
                            overblock-md-preview-idle)
    (overblock-live-stop)))

(provide 'overblock-md-preview)
;;; overblock-md-preview.el ends here
