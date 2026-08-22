;;; pycell-block.el --- Text blocks over a buffer  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Marcel Arpogaus

;; Author: Marcel Arpogaus <znepry.necbtnhf@tznvy.pbz>
;; Assisted-by: Claude:claude-opus-5
;; Assisted-by: Claude:claude-fable-5
;; Version: 0.1.3
;; Package-Requires: ((emacs "29.1"))
;; Keywords: convenience, tools
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

;; A block shows text over a region of a buffer, or after it, with
;; decorations around it.  Nothing here knows about Python, about cells
;; or about markdown: a caller renders text and a block puts it on the
;; screen.
;;
;;     (pycell-block-show BEG END :kind \='result :body TEXT :header TEXT)
;;
;; An anchor overlay covers the region and holds the state.  A second
;; overlay covers the newline that ends the region and carries what
;; shows after it: the header, the body and the footer, each on a row of
;; its own, in the slot that suits it.  Measured in a graphical frame: a
;; bar puts its icons at the window edge with `(space :align-to (- right
;; ...))\=', which a display property ignores and a string does not, and
;; an image in a display string is swallowed while one in a string
;; draws.  So the header and the footer are strings, and the body rides
;; the display property unless it holds an image.
;;
;; Text shown over the region hangs on its lines, a piece to a line.
;; Emacs lays a display string out whole on every redisplay, so one
;; string for a tall region costs its full height on every scroll event,
;; while a piece to a line costs only what the window shows.  Lines with
;; no piece left for them go under a cloak.
;;
;; See `pycell-block-show' for what a caller may pass.

;;; Code:

(require 'seq)
(require 'subr-x)

(defgroup pycell-block nil
  "Blocks of text shown over a buffer."
  :group 'convenience
  :prefix "pycell-block-")

(defcustom pycell-block-fringe nil
  "Whether a block draws a bracket in the fringe beside it.
The bracket marks how far the block reaches, as `org-modern' marks a
source block, and it costs what a bracket costs: a body that wears one
rides a string rather than a display property, because a `line-prefix'
inside a display string draws no fringe."
  :type 'boolean
  :group 'pycell-block)

(defface pycell-block-fringe-face '((t :inherit shadow))
  "Face of the bracket a block draws in the fringe."
  :group 'pycell-block)

;; The bracket of a block is a line down the left fringe, two pixel
;; columns wide as `org-modern' draws one, and `(top t)' repeats it for
;; the whole height of every row.
;;
;; A foot at each end, as `org-modern' draws one, would take a bitmap of
;; its own for the first and the last row.  Measured: such a bitmap lands
;; beside the right row where that row is a line of the buffer, and one
;; row off where the row belongs to a string, which is what a block shows
;; after its region.  A line without feet says the same thing and needs
;; no overlay of its own.
(define-fringe-bitmap 'pycell-block--line
  (vector (logior (expt 2 15) (expt 2 14))) nil 16 '(top t))

(defconst pycell-block--fringe-prefix
  (propertize " " 'display '(left-fringe pycell-block--line
                                         pycell-block-fringe-face))
  "The line prefix that draws the bracket.
One string for every block: nothing may write into its properties.")

(defun pycell-block-get (block prop)
  "Return the PROP of BLOCK."
  (plist-get (overlay-get block 'pycell-block) prop))

(defun pycell-block-set (block prop value)
  "Set the PROP of BLOCK to VALUE and return VALUE.
The screen follows on the next `pycell-block-refresh'."
  (overlay-put block 'pycell-block
               (plist-put (overlay-get block 'pycell-block) prop value))
  value)

(defun pycell-block-in (beg end &optional kind)
  "Return the blocks that overlap BEG..END, those of KIND when it is given.
The order is whatever `overlays-in\=' gives, so a caller that takes the
first is relying on one block of a kind to a region.  Without KIND every
kind answers."
  (seq-filter (lambda (ov)
                (and (overlay-get ov 'pycell-block)
                     (or (null kind) (eq kind (pycell-block-get ov :kind)))))
              (overlays-in beg end)))

(defun pycell-block-at (&optional kind)
  "Return the block of KIND at point, or nil.
Point counts as inside a block that begins or ends on it, so a block
whose region starts where point sits answers.  A caller that works from
a click moves point there first."
  (car (pycell-block-in (max (1- (point)) (point-min))
                        (min (1+ (point)) (point-max))
                        kind)))

(defun pycell-block-delete (block)
  "Delete BLOCK and the overlays that carry what it shows."
  (mapc #'delete-overlay
        (delq nil (append (list (pycell-block-get block :newline))
                          (pycell-block-get block :parts)
                          (pycell-block-get block :attached))))
  (delete-overlay block))

(defun pycell-block-clear (&optional beg end kind)
  "Delete the blocks of KIND that overlap BEG..END.
BEG and END default to the whole buffer, KIND to every kind.  A
narrowing hides nothing from this: the range is searched whole, so no
block is left behind outside it."
  (without-restriction
    (mapc #'pycell-block-delete
          (pycell-block-in (or beg (point-min)) (or end (point-max)) kind))))

(defun pycell-block-show (beg end &rest props)
  "Show a block over the region BEG..END and return it.
It replaces the blocks of its own kind in that region — every kind,
where no `:kind\=' is given.  PROPS is a plist, and every entry is
optional:

  :kind      a symbol that tells the blocks of one caller from another.
             Without it the block is anonymous, and an anonymous block
             answers to every kind in `pycell-block-in\=', `-at\=' and
             `-clear\='.
  :data      anything the caller keeps with the block.  The layer stores
             it and never reads it, so its shape is the caller\='s own.
  :over      text shown instead of the lines of the region, a piece to
             a line; without it the region stays as it is.
  :body      text shown after the region, on the newline that ends it,
             or on the anchor where the region ends without one.
  :header    text above the body, :footer text below it.
  :hidden    non-nil shows nothing at all, decorations included.
  :attached  overlays of the caller\='s own, deleted with the block.
  :keymap and :help-echo go on every overlay the block draws; an
             overlay of the caller\='s own under `:attached\=' keeps
             whatever the caller put on it.

The caller renders the text and hands it over; a block never calls a
renderer itself.  Change a property with `pycell-block-set' and call
`pycell-block-refresh' to show the change.

The anchor ends before the newline of the region, so a window that
starts at the next line keeps the block out of view.  Both overlays
advance with text typed at their end.

A block also keeps the overlays that carry what it shows.  `:newline\='
is readable, and a caller needs it to keep an outline fold off the
newline the block hangs on; `:parts\=' is the layer\='s own, made anew by
every `pycell-block-refresh\='."
  (pycell-block-clear beg end (plist-get props :kind))
  (let* ((anchor-end (if (and (eq (char-before end) ?\n)
                              ;; a region that is only a newline keeps a
                              ;; non-empty anchor
                              (> (1- end) beg))
                         (1- end)
                       end))
         (block (make-overlay beg anchor-end nil t t)))
    (overlay-put block 'evaporate t)
    (overlay-put block 'pycell-block props)
    (when (eq (char-after anchor-end) ?\n)
      (let ((ov (make-overlay anchor-end (1+ anchor-end) nil t)))
        (overlay-put ov 'evaporate t)
        (pycell-block-set block :newline ov)))
    (pycell-block-refresh block)
    block))

(defun pycell-block--dress (block ov)
  "Give OV the keymap and the help echo of BLOCK, and return OV.
Everything a block shows answers to the same click and says the same
thing under the mouse, whichever overlay carries it."
  (when-let* ((map (pycell-block-get block :keymap)))
    (overlay-put ov 'keymap map))
  (when-let* ((help (pycell-block-get block :help-echo)))
    (overlay-put ov 'help-echo help))
  ov)

(defun pycell-block--cloak (block beg end)
  "Return an overlay of BLOCK that hides BEG..END and stays hidden.
A cloak covers the lines that no piece was left for.  It has to start
at the end of a visible line: `scroll-down' answers a run that begins
a line with a beginning-of-buffer error, in the middle of the region."
  (let ((ov (make-overlay beg end nil t)))
    (overlay-put ov 'evaporate t)
    (overlay-put ov 'invisible t)
    (overlay-put ov 'pycell-cloak t)
    (pycell-block--dress block ov)))

(defun pycell-block--lines (text)
  "Split TEXT into the lines that can stand on a row of their own.
A newline inside an image run stays where it is.  Such a run draws one
image however many lines it covers, and display math covers three:
the two dollar rows and the formula.  A piece for each of those lines
would carry the same run and draw the same image again."
  (let ((pos 0) (from 0) lines)
    ;; The newlines are what matter, so they are searched for rather than
    ;; walked to: measured over a rendered cell of three hundred lines,
    ;; 2.14 milliseconds character by character against 0.10 this way.
    (while (setq pos (string-search "\n" text pos))
      (if (eq (car-safe (get-text-property pos 'display text)) 'image)
          (setq pos (1+ pos))
        (push (substring text from pos) lines)
        (setq pos (1+ pos)
              from pos)))
    (push (substring text from) lines)
    (nreverse lines)))

(defun pycell-block--pieces (block text)
  "Hang TEXT over the lines of BLOCK, a piece to a line.
Return the overlays that carry the pieces and the cloaks.

A rendering rarely has as many lines as the region.  Where it has more,
one line carries several of them, dealt out as evenly as the two counts
allow; where it has fewer, the lines left over go under a cloak.

A piece covers the text of its line and leaves the newline alone, so
the buffer keeps its line structure and every line keeps its height.
A piece with an image in it cannot ride a `display' property, because
display properties do not nest and the image would be swallowed.  Such
a piece hides its line with a display string of nothing and rides the
after-string instead, which draws images.  The line keeps its own row
either way, which is what makes the region scroll a line at a time.

A line without text cannot carry a piece — there is nothing to put the
display property on — and a rendering rarely fills as many lines as the
region has anyway.  Those lines go under a cloak."
  (let* ((beg (overlay-start block))
         ;; The last newline of the region belongs to it: the anchor
         ;; stops before that newline, and a cloak that stopped there
         ;; too would leave the last line of the region on the screen.
         (end (if-let* ((nl (pycell-block-get block :newline)))
                  (overlay-end nl)
                (overlay-end block)))
         (lines (pycell-block--lines (string-trim text "\n" "\n")))
         (count (length lines))
         (rows (save-excursion
                 (goto-char beg)
                 (let (rows)
                   (while (< (point) end)
                     (push (cons (point) (min end (pos-eol))) rows)
                     (forward-line 1))
                   (nreverse rows))))
         (slots (max 1 (seq-count (lambda (row) (> (cdr row) (car row))) rows)))
         (filled 0)
         parts cloak-from)
    (dolist (row rows)
      (let ((from (car row))
            (to (cdr row))
            chunk)
        (when (> to from)                    ; a line with text to cover
          ;; Line FILLED of SLOTS takes the rendered lines from
          ;; COUNT*FILLED/SLOTS to COUNT*(FILLED+1)/SLOTS, so a remainder
          ;; is spread over the lines rather than heaped on the last.
          (setq chunk (seq-subseq lines
                                  (/ (* filled count) slots)
                                  (/ (* (1+ filled) count) slots))
                filled (1+ filled)))
        (if (null chunk)
            ;; No text on this line, or no rendered lines left for it.
            ;; Open a cloak at the newline above, or leave the open one
            ;; to grow.
            (unless cloak-from (setq cloak-from (1- from)))
          (when cloak-from
            (push (pycell-block--cloak block cloak-from (1- from)) parts)
            (setq cloak-from nil))
          (let ((ov (make-overlay from to nil t))
                (piece (string-join chunk "\n")))
            (overlay-put ov 'evaporate t)
            (if (pycell-block-image-in piece)
                (progn (overlay-put ov 'display "")
                       (overlay-put ov 'after-string piece))
              (overlay-put ov 'display piece))
            (push (pycell-block--dress block ov) parts)))))
    (when cloak-from
      (push (pycell-block--cloak block cloak-from (1- end)) parts))
    (nreverse parts)))

(defun pycell-block--attach (block shown)
  "Show the header, the body and the footer of SHOWN after BLOCK.
SHOWN is the property list of what the block shows, or nil for a block
that shows nothing.
Each stands on a row of its own, in the slot that suits it.

A row that must be a string rides the after-string of the anchor: a bar
draws its icons at the window edge with `(space :align-to (- right
...))\=', which a display property ignores, and an image in a display
string is swallowed while one in a string draws.

A plain body rides the display property of the newline that ends the
region, which is the cheapest place for text.

The newline is never hidden.  Measured: with the rows of a figure on the
after-string of that newline and the newline itself replaced by an empty
display string, `pixel-scroll-precision-scroll-up\=' refused to pass the
block — 280 refusals with a beginning-of-buffer error over six figures,
where the same buffer with the newline left alone scrolls to the top
without one.

Each string carries the line breaks that its own rows need."
  (let* ((header (plist-get shown :header))
         (body (plist-get shown :body))
         (footer (plist-get shown :footer))
         (fringe pycell-block-fringe)
         (on-display (and body
                          (not (pycell-block-image-in body))
                          (not fringe)
                          ;; without a newline there is nothing to hang a
                          ;; display property on, so the body joins the
                          ;; rows on the anchor
                          (pycell-block-get block :newline)))
         ;; the rows that ride the anchor, in the order they show
         (strings (delq nil (list header
                                  (unless on-display body)
                                  (unless on-display footer))))
         ;; A row needs a break before it unless the newline it hangs on
         ;; already begins a line: a cell that ends in a blank line gives
         ;; that line to the header.
         (lead (if (eq (char-before (overlay-end block)) ?\n) "" "\n"))
         (nl (pycell-block-get block :newline)))
    (overlay-put block 'after-string
                 (when strings
                   (let ((text (concat lead (string-join strings "\n"))))
                     ;; A string carries the prefixes itself: measured in
                     ;; a graphical frame, the fringe beside the rows of
                     ;; a string follows the properties of that string,
                     ;; and the overlay that shows it says nothing about
                     ;; them.
                     (when fringe
                       (add-text-properties
                        0 (length text)
                        (list 'line-prefix pycell-block--fringe-prefix
                              'wrap-prefix pycell-block--fringe-prefix)
                        text))
                     text)))
    (when (and nl (overlay-buffer nl))
      (overlay-put nl 'display
                   (when on-display
                     (concat (if header "\n" lead) body "\n")))
      (overlay-put nl 'after-string
                   (when (and on-display footer) (concat footer "\n")))
      (pycell-block--dress block nl))))

(defun pycell-block-refresh (block)
  "Show BLOCK again from its properties.
Call it after `pycell-block-set'.  Everything the block shows is made
anew, so nothing has to be saved and given back."
  (mapc #'delete-overlay (pycell-block-get block :parts))
  (pycell-block-set block :parts nil)
  ;; A hidden block shows nothing, so it reads nothing.
  ;; `pycell-block-set' can hand back a new plist when it adds a key, so
  ;; SHOWN is read before any part is written.
  (let ((shown (unless (pycell-block-get block :hidden)
                 (overlay-get block 'pycell-block))))
    (pycell-block--dress block block)
    (when-let* ((over (plist-get shown :over)))
      (pycell-block-set block :parts (pycell-block--pieces block over)))
    (pycell-block--attach block shown)
    ;; The bracket runs beside the region; `pycell-block--attach' has put
    ;; it beside the rows it wrote.
    (when pycell-block-fringe
      (overlay-put block 'line-prefix pycell-block--fringe-prefix)
      (overlay-put block 'wrap-prefix pycell-block--fringe-prefix))
    block))

(defun pycell-block-image-in (text)
  "Return the `display\=' spec of the first image in TEXT, or nil.
The value is (image . PLIST), so a caller can read `:data\=' or `:type\='
from it.  A `raise\=' spec, which shr uses for a superscript, is not an
image and does not answer here."
  (let ((len (length text))
        (pos 0)
        img)
    ;; Run to run, not character to character: a display property that is
    ;; not an image can cover a hundred thousand characters — a raised
    ;; superscript, an alignment stretch — and the step of one measured
    ;; 38.5 milliseconds over such a run against 0.001 for the jump.
    (while (and (not img)
                (setq pos (text-property-not-all pos len 'display nil text)))
      (let ((disp (get-text-property pos 'display text)))
        (if (eq (car-safe disp) 'image)
            (setq img disp)
          (setq pos (or (next-single-property-change pos 'display text) len)))))
    img))

(provide 'pycell-block)
;;; pycell-block.el ends here
