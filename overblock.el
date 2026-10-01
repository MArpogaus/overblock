;;; overblock.el --- Text blocks over a buffer  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Marcel Arpogaus

;; Author: Marcel Arpogaus <znepry.necbtnhf@tznvy.pbz>
;; Assisted-by: Claude:claude-opus-5
;; Assisted-by: Claude:claude-fable-5
;; Version: 1.0
;; Package-Requires: ((emacs "29.1"))
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

;; A block shows text over a region of a buffer, or after it, with
;; decorations around it.  Nothing here knows about Python, about cells
;; or about markdown: a caller renders text and a block puts it on the
;; screen.
;;
;;     (overblock-show BEG END :kind 'result :body TEXT :header TEXT)
;;
;; An anchor overlay covers the region and holds the state.  A second
;; overlay covers the newline that ends the region and carries what
;; shows after it.  That is the header and the body, each on a row of
;; its own, in the slot that suits it.  A bar puts its icons at the
;; window edge with `(space :align-to (- right ...))'.  A display string
;; ignores such a space, and an overlay string does not.  An image in a
;; display string is swallowed, and one in an overlay string draws.  So
;; the header is an overlay string, and the body is the display property
;; unless it holds an image.
;;
;; Text shown over the region hangs on its lines, a piece to a line.
;; Emacs lays a display string out whole on every redisplay.  Thus one
;; string for a tall region costs its full height on every scroll event,
;; and a piece to a line costs only what the window shows.  Lines of
;; text with no piece left for them go under a cloak; a blank line
;; stays, as the gap it is.
;;
;; See `overblock-show' for what a caller may pass.

;;; Code:

(require 'seq)
(require 'subr-x)
;; For `prop-match-value', which is not autoloaded.
(require 'text-property-search)

(defgroup overblock nil
  "Blocks of text shown over a buffer."
  :group 'convenience
  :prefix "overblock-")

(defcustom overblock-image-height 0.8
  "How tall an image may be drawn inline, as a share of the window.
Zero draws it at whatever size it came in.  The cap is on the drawing
only: a caller that pops an image out, or writes it to a file, works
from the original.

A block taller than the window cannot be scrolled past: the wheel
bounces back off it.  The default leaves room for the two lines of
text a block carries besides the figure.

The share is taken when the block is drawn, from the window that shows
the buffer, else the selected window.  A window resized later keeps
the size the figure had."
  :type 'number
  :group 'overblock)

(defface overblock-bar '((t :inherit (shadow default) :overline t :extend t))
  "Face of a bar over a block: the rule above it and the text on it.
One face for every bar (over a cell, a chunk, a result and a doc
string), so all notebooks look the same.  The overline is the rule:
a face draws it over the whole row, with no measuring.

`default' after `shadow' gives a background: a display string also has
the face of the text under it, such as the background of
`markdown-code-face' on the fence line of an Rmd chunk.")

(defface overblock-body '((t :inherit shadow :extend t))
  "Face of the body of a result, under its bar.")

(defun overblock-get (block prop)
  "Return the PROP of BLOCK."
  (plist-get (overlay-get block 'overblock) prop))

(defun overblock-set (block prop value)
  "Set the PROP of BLOCK to VALUE and return VALUE.
The screen follows on the next `overblock-refresh'."
  (overlay-put block 'overblock
               (plist-put (overlay-get block 'overblock) prop value))
  value)

(defun overblock-in (beg end &optional kind)
  "Return the blocks that overlap BEG..END, those of KIND when it is given.
The order is that of `overlays-in', so a caller that takes the first
relies on one block of a kind in a region.  Without KIND, every kind
counts."
  (seq-filter (lambda (ov)
                (and (overlay-get ov 'overblock)
                     (or (null kind) (eq kind (overblock-get ov :kind)))))
              (overlays-in beg end)))

(defun overblock-at (&optional kind)
  "Return the block of KIND at point, or nil.
Point counts as inside a block that starts or ends at point.  A caller
that works from a click moves point there first."
  (car (overblock-in (max (1- (point)) (point-min))
                     (min (1+ (point)) (point-max))
                     kind)))

(defun overblock--carriers (block)
  "Return the overlays that carry what BLOCK shows, the anchor apart.
One list for `overblock-delete', which removes them, and
`overblock-sweep-orphans', which keeps them, so the two always agree."
  (delq nil (append (list (overblock-get block :newline))
                    (overblock-get block :parts)
                    (overblock-get block :attached))))

(defun overblock-delete (block)
  "Delete BLOCK and the overlays that carry what it shows."
  (mapc #'delete-overlay (overblock--carriers block))
  (delete-overlay block))

(defun overblock-clear (&optional beg end kind)
  "Delete the blocks of KIND that overlap BEG..END.
BEG and END default to the whole buffer, KIND to every kind.  The
range is searched without the narrowing.

Clearing the whole buffer also sweeps orphans (see
`overblock-sweep-orphans').  Only the whole buffer: a range does not
say which block an orphan belonged to."
  ;; Tested before the widening: under a narrowing, the bounds of the
  ;; caller are the narrowed ones.
  (let ((whole (and (null kind)
                    (<= (or beg (point-min)) (point-min))
                    (>= (or end (point-max)) (point-max)))))
    (without-restriction
      (mapc #'overblock-delete
            (overblock-in (or beg (point-min)) (or end (point-max)) kind))
      (when whole (overblock-sweep-orphans)))))

(defun overblock-sweep-orphans ()
  "Remove the overlays of the layer that no live block owns.
A block is removed through its anchor, which knows what it drew.  When
the anchor is gone but its overlays are not (a package deleted the
overlays of a region, or the anchor evaporated with its line), one of
them can be a cloak that keeps lines invisible.

Every overlay of the layer has the `overblock-part' property, so the
orphans can be found.  A caller that cleared one kind of block calls
this, because an orphan has no kind."
  (without-restriction
    (let ((owned (make-hash-table :test #'eq)))
      (dolist (block (overblock-in (point-min) (point-max)))
        (puthash block t owned)
        (dolist (ov (overblock--carriers block))
          (puthash ov t owned)))
      (dolist (ov (overlays-in (point-min) (point-max)))
        (when (and (overlay-get ov 'overblock-part)
                   (not (gethash ov owned)))
          (delete-overlay ov))))))

(defconst overblock--plain '(:inherit default :extend t)
  "The face that paints the source under a rendering plain.
Extended, because past the end of a line only a face with `:extend'
paints, and the face of the source's newline showed there instead.")

(defvar-local overblock--columns nil
  "The columns this buffer was last drawn at, or nil before the first look.
`overblock--width-changed' compares with it, so its hooks, which run
for every kind of change, redraw only when the width changed.")

(defun overblock-show (beg end &rest props)
  "Show a block over the region BEG..END and return it.
Return nil where BEG..END holds nothing to hang a block on: an anchor
of no length is deleted at once.  The block replaces the blocks of its
own kind in that region, or of every kind without `:kind'.  PROPS is a
plist, and every entry is optional:

  :kind      a symbol that tells the blocks of one caller from another.
             Without it the block is anonymous: `overblock-in',
             `-at' and `-clear' reach it when they are asked for no
             kind, and no named kind answers for it.
  :data      anything the caller keeps with the block.  The layer stores
             it and never reads it.
  :over      text shown instead of the lines of the region, a piece to
             a line; without it the region stays as it is.
  :indent    columns at the start of every line after the first that
             the pieces leave in view, so the indentation of the source
             stays buffer text, with any indentation guide on it.  A
             line indented deeper is covered from that column on, so
             every row starts at one column; the first row starts
             where the block does.  Nil covers whole lines.
  :body      text shown after the region, on the newline that ends it,
             or on the anchor where the region ends without one.
  :header    text shown above the body.
  :hidden    non-nil shows nothing at all, decorations included.
  :attached  overlays of the caller's own, deleted with the block.
  :keymap and :help-echo go on every overlay the block draws; an
             overlay of the caller under `:attached' keeps its own.  A
             click is answered by the string it lands on, with what
             `overblock-fill-props' left there (such as the keymap of
             shr on a link): the string answers the mouse, the
             overlays answer point.

The caller renders the text; a block never calls a renderer.  Change a
property with `overblock-set' and call `overblock-refresh' to show it.

The anchor ends before the newline of the region, so a window that
starts at the next line does not show the block.  Both overlays grow
with text typed at their end.

A block also keeps the overlays that carry what it shows.  `:newline'
is readable: a caller needs it to keep an outline fold off the newline
the block hangs on.  `:parts' belongs to the layer and is made again
by every `overblock-refresh'."
  (overblock-clear beg end (plist-get props :kind))
  (let* ((anchor-end (if (and (eq (without-restriction (char-before end))
                                  ?\n)
                              ;; A region of only a newline keeps a
                              ;; non-empty anchor.
                              (> (1- end) beg))
                         (1- end)
                       end))
         ;; `evaporate' deletes a zero-length overlay at once, so return
         ;; nil instead of a dead anchor.
         (block (and (> anchor-end beg)
                     (make-overlay beg anchor-end nil t t))))
    (when block
      (overlay-put block 'evaporate t)
      (overlay-put block 'overblock-part t)
      ;; The source under a rendering is painted plain: the face of a
      ;; newline extends to the window edge. Only under a rendering of
      ;; whole lines: a result leaves its code in view, and `:indent'
      ;; leaves the indentation guide. Below `hl-line' (-50).
      (when (and (plist-get props :over) (not (plist-get props :indent)))
        (overlay-put block 'face overblock--plain))
      (overlay-put block 'priority -60)
      ;; The width the rendering was built for, for
      ;; `overblock--width-changed'. Built with no window to measure,
      ;; the buffer forgets its width, so the next window that shows it
      ;; draws it again.
      (let ((columns (overblock-window-columns)))
        (overlay-put block 'overblock-columns columns)
        (unless columns (setq overblock--columns nil)))
      ;; `modification-hooks' belong to the caller. The two slots of the
      ;; layer exist from the start, so every `plist-put' changes the
      ;; list in place.
      (overlay-put block 'overblock (append props (list :newline nil
                                                        :parts nil)))
      (when (eq (without-restriction (char-after anchor-end)) ?\n)
        (let ((ov (make-overlay anchor-end (1+ anchor-end) nil t)))
          (overlay-put ov 'evaporate t)
          (overlay-put ov 'overblock-part t)
          ;; Plain, as the anchor.
          (overlay-put ov 'face overblock--plain)
          (overlay-put ov 'priority -60)
          (overblock-set block :newline ov)))
      (overblock-refresh block)
      block)))

(defun overblock--dress (block ov)
  "Give OV the keymap and the help echo of BLOCK, and return OV.
Every overlay of a block answers the same click and shows the same
help.  A hidden block answers nothing.

The overlays carry the keymap, not only the string: a click finds the
keymap of the string it lands on, but a key at point does not, because
point never enters a display string."
  ;; Written also when nil, to remove an old value.
  (let ((hidden (overblock-get block :hidden)))
    (overlay-put ov 'keymap (unless hidden (overblock-get block :keymap)))
    (overlay-put ov 'help-echo (unless hidden
                                 (overblock-get block :help-echo))))
  ov)

(defun overblock--cloak (block beg end)
  "Return an overlay of BLOCK that hides BEG..END and stays hidden.
A cloak covers the lines that have no piece, from the newline that
ends the row above them up to, not through, their own last newline,
which stays to end that row.  It must start at the end of a visible
line: `scroll-down' signals a beginning-of-buffer error over a run that
starts a line.

Invisible, not a display of one newline: point moves over invisible
text, but stops on each position of a run under a display string.
The newline that stays gets `overblock--newline-guard'."
  (let ((ov (make-overlay beg end nil t)))
    (overlay-put ov 'evaporate t)
    (overlay-put ov 'overblock-part t)
    (overlay-put ov 'invisible t)
    (overlay-put ov 'overblock-cloak t)
    (overblock--dress block ov)))

(defun overblock--newline-guard (block at)
  "Return an overlay of BLOCK that draws the newline AT as a plain newline.
Return nil where AT is no newline, as at the end of a file without a
final newline.
The newline a cloak leaves keeps the `display' of the text, and
indent-bars writes one on the newline of every blank line.  The
display of an overlay outranks that of the text.  Its priority is
below the body of a result on the same newline, so a body wins."
  (when (eq (char-after at) ?\n)
    (let ((ov (make-overlay at (1+ at) nil t)))
      (overlay-put ov 'evaporate t)
      (overlay-put ov 'overblock-part t)
      (overlay-put ov 'display "\n")
      (overlay-put ov 'priority -60)
      ;; Part of the cloak.
      (overlay-put ov 'overblock-cloak t)
      (overblock--dress block ov))))

(defun overblock--lines (text)
  "Split TEXT into the lines that can stand on a row of their own.
A newline inside an image run stays where it is.  Such a run draws one
image however many lines it covers, and display math covers three:
the two dollar rows and the formula.  A piece for each of those lines
would carry the same run and draw the same image again."
  (let ((pos 0) (from 0) lines)
    ;; Search for the newlines, much faster than a walk.
    (while (setq pos (string-search "\n" text pos))
      (if (eq (car-safe (get-text-property pos 'display text)) 'image)
          (setq pos (1+ pos))
        (push (substring text from pos) lines)
        (setq pos (1+ pos)
              from pos)))
    (push (substring text from) lines)
    (nreverse lines)))

(defun overblock--rows (beg end)
  "Return the lines of BEG..END as a list of (FROM . TO), in order.
TO is where the text of a line ends, so an empty line gives a pair
with nothing between.

The walk is without the narrowing, and stops when `forward-line' does
not move: overlay positions ignore a narrowing, and END can be past
the end of the buffer, so a test of END alone could loop for ever."
  (without-restriction
    (save-excursion
      (goto-char beg)
      (let (rows (moved 0))
        (while (and (< (point) end) (zerop moved))
          (push (cons (point) (min end (pos-eol))) rows)
          (setq moved (forward-line 1)))
        (nreverse rows)))))

(defun overblock--piece (block from to text)
  "Return an overlay of BLOCK that shows TEXT in place of FROM..TO.
A piece with an image cannot be a `display' property, because display
properties do not nest and the image would be swallowed.  Such a
piece hides its line with an empty display string and shows TEXT on
the before-string, which draws images.  The line keeps its own row
either way, so a region scrolls a line at a time.

The before-string, not the after-string: a cloak starts at the end of
the piece before it, and Emacs does not draw an overlay string whose
position is inside invisible text."
  (let ((ov (make-overlay from to nil t)))
    (overlay-put ov 'evaporate t)
    (overlay-put ov 'overblock-part t)
    ;; Plain, as the anchor: a display string also has the face of the
    ;; text under it. Needed here because a block with `:indent' paints
    ;; no anchor. Below `hl-line'.
    (overlay-put ov 'face overblock--plain)
    (overlay-put ov 'priority -60)
    (if (overblock-image-in text)
        (let ((text (copy-sequence text)))
          ;; Else the string has the face of the text under it, such as
          ;; the stipple of an indentation guide.
          (add-face-text-property 0 (length text) 'default t text)
          (overlay-put ov 'display "")
          (overlay-put ov 'before-string text))
      (overlay-put ov 'display text))
    (overblock--dress block ov)))

(defun overblock--deal (lines slots)
  "Return LINES dealt into SLOTS chunks, as evenly as the two counts allow.
Chunk I takes the lines up to COUNT*(I+1)/SLOTS, so a remainder is
spread over the chunks rather than heaped on the last.

Rounded up, so the first chunk is never empty while there is a line
for it: only the first row of a region starts where the block does,
and the first line of a rendering is written for that column.  Three
lines over five rows deal (1 1 0 1 0).

The chunks come off a walking list, so the deal is linear."
  (let* ((count (length lines))
         (rest lines)
         ;; Ceiling division: how many lines the first I chunks hold.
         (upto (lambda (i) (/ (+ (* i count) slots -1) slots)))
         chunks)
    (dotimes (i slots)
      (let ((wanted (- (funcall upto (1+ i)) (funcall upto i))))
        (push (take wanted rest) chunks)
        (setq rest (nthcdr wanted rest))))
    (nreverse chunks)))

(defun overblock--cloak-from (open from)
  "Return where the cloak covering the row at FROM starts.
OPEN is where an open cloak starts, or nil.  An open cloak grows; a
new one starts at the newline above FROM, the end of the visible line
before it (see `overblock--cloak').

Return nil for a row at the start of the buffer, which has no newline
above it.  Such a row keeps its text."
  (cond (open open)
        ((> from (point-min)) (1- from))))

(defun overblock--cloak-to (block from end)
  "Return the guard and the cloak of BLOCK over FROM up to the row end END.
END is past the newline that ends the last hidden row, which stays in
view with its guard.  A buffer that ends without a newline ends the
region on the reader's last character, and the cloak takes that too."
  (let ((to (if (eq (char-before end) ?\n) (1- end) end)))
    (list (overblock--newline-guard block to)
          (overblock--cloak block from to))))

(defun overblock--region-end (block)
  "Return where the region of BLOCK ends, its last newline included.
The anchor stops before that newline, and a cloak must cover it.

The newline overlay is tested by its buffer: deleting the last newline
of the region deletes that overlay but not the anchor, and a deleted
overlay has no end."
  (let ((nl (overblock-get block :newline)))
    (if (and (overlayp nl) (overlay-buffer nl))
        (overlay-end nl)
      (overlay-end block))))

(defun overblock--piece-rows (block end)
  "Return the rows of BLOCK up to END as (BOL FROM TO BLANK), in order.
BOL is where the line starts, FROM where its piece starts, TO where its
text ends and BLANK whether the line is blank.  The piece starts at the
block on the first row and `:indent' columns in on every other, or at
the end of a shorter line, which then carries nothing."
  (let ((beg (overlay-start block))
        (indent (overblock-get block :indent)))
    (mapcar (lambda (row)
              (let ((bol (car row)) (to (cdr row)))
                (list bol
                      (if (and indent (> bol beg))
                          ;; A column: a tab is wider than one.
                          (without-restriction
                            (save-excursion
                              (goto-char bol)
                              (move-to-column indent)
                              (min to (point))))
                        bol)
                      to
                      ;; A row can be outside a narrowing.
                      (without-restriction
                        (string-blank-p
                         (buffer-substring-no-properties bol to))))))
            (overblock--rows beg end))))

(defun overblock--piece-lines (text slots)
  "Return the lines of TEXT to deal over SLOTS rows, as (LONG . LINES).
LONG is non-nil where TEXT has more lines than there are rows."
  (let ((all (overblock--lines (string-trim text "\\(?:[ \t]*\n\\)+"
                                            "\\(?:\n[ \t]*\\)+"))))
    (cons (> (length all) slots) all)))

(defun overblock--spread (rows lines slots long)
  "Return LINES dealt over the rows of ROWS with text, a chunk a row.
SLOTS is how many rows have text.  A LONG rendering loses its blank
lines, and the blank lines of the source stay in view instead."
  (let ((dealt (overblock--deal (if long (seq-remove #'string-blank-p lines)
                                  lines)
                                slots)))
    (mapcar (lambda (row) (and (overblock--carries-p row) (pop dealt)))
            rows)))

(defvar overblock--keys nil
  "The keys `overblock--align' has made, by line.")

(defun overblock--key (text)
  "Return what TEXT and its rendering have in common: its first letters.
Markup goes (quotes, bullets, pipes, dollars, comment marks), and so
does case, so a source line and the line it renders to have one key."
  (with-memoization (gethash text overblock--keys)
    (let ((bare (downcase (replace-regexp-in-string "[^[:alnum:]]+" "" text))))
      (substring bare 0 (min 6 (length bare))))))

(defun overblock--keys-match-p (key row-key)
  "Return non-nil where the line KEY is the key of the row ROW-KEY."
  (and row-key
       (not (string-empty-p key))
       (not (string-empty-p row-key))
       (or (string-prefix-p key row-key) (string-prefix-p row-key key))))

(defun overblock--ahead (line keys)
  "Return the index in KEYS of the row LINE was rendered from.
Only the first four rows count; nil where none of them matches."
  (let ((key (overblock--key line))
        (index 0)
        found)
    (while (and keys (< index 4) (not found))
      (if (overblock--keys-match-p key (pop keys))
          (setq found index)
        (setq index (1+ index))))
    found))

(defun overblock--carries-p (row)
  "Return non-nil where ROW has text to carry a piece."
  (> (nth 2 row) (nth 1 row)))

(defun overblock--row-keys (rows)
  "Return the `overblock--key' of each of ROWS, nil for a row without text."
  (mapcar (lambda (row)
            (and (overblock--carries-p row)
                 (overblock--key (without-restriction
                                   (buffer-substring-no-properties
                                    (nth 1 row) (nth 2 row))))))
          rows))

(defun overblock--line-by-line-p (lines keys)
  "Return non-nil where half the LINES with text match one of KEYS."
  (let ((text (seq-remove #'string-blank-p lines))
        (whole (make-hash-table :test #'equal))
        short)
    ;; Keys of six letters match only when equal; a shorter one can
    ;; begin another, and those are few.
    (dolist (row-key keys)
      (when (and row-key (not (string-empty-p row-key)))
        (puthash row-key t whole)
        (when (< (length row-key) 6) (push row-key short))))
    (>= (* 2 (seq-count
              (lambda (line)
                (let ((key (overblock--key line)))
                  (or (gethash key whole)
                      (and (not (string-empty-p key))
                           (seq-some (lambda (row-key)
                                       (overblock--keys-match-p key row-key))
                                     (if (< (length key) 6) keys short))))))
              text))
        (length text))))

(defun overblock--no-false-gap (lines keys)
  "Return LINES without the blank lines before the line of the first of KEYS.
Such a blank line is a gap the source does not have there: shr puts one
after a heading."
  (while (and (cdr lines) (string-blank-p (car lines))
              (eql (overblock--ahead (cadr lines) keys) 0))
    (pop lines))
  lines)

(defun overblock--take (lines keys)
  "Return the chunk of LINES a row carries, and the rest, as (CHUNK . REST).
KEYS are the keys of the rows after it.  The row takes the next line,
and then every line that belongs to no row near while the next line
that does belongs to the next row with text: a source line the renderer
wrapped.  A blank line is never taken this way: it is a gap, and it
goes to the next row."
  (let ((chunk (and lines (list (pop lines))))
        ;; The next row with text: a gap between does not count.
        (next (seq-position keys t (lambda (key _) key))))
    (while (and lines
                (not (string-blank-p (car lines)))
                (null (overblock--ahead (car lines) keys))
                ;; the next line that belongs to a row belongs to the next
                (eql next (seq-some (lambda (line) (overblock--ahead line keys))
                                    (cdr lines))))
      (setq chunk (append chunk (list (pop lines)))))
    (cons chunk lines)))

(defun overblock--align (rows lines)
  "Return the chunk of LINES each of ROWS carries, nil for none.
Each rendering line goes to the row it was rendered from, found by
`overblock--key' among the next few rows.  A row nothing was rendered
from carries nothing: an underline, a fence, a table rule.  A row
without text takes a blank line where one comes next, so the gaps of
the rendering fall on the gaps of the source; such a row carries `:gap'
and stays in view.  The last row with text
takes whatever is left.  See `overblock--take' for a wrapped line.

Nil as a whole where fewer than half the lines with text match a row:
the rendering is no line by line one of its source, and
`overblock--spread' deals it instead."
  (let* ((overblock--keys (make-hash-table :test #'eq))
         (carry (seq-count #'overblock--carries-p rows))
         (keys (overblock--row-keys rows))
         chunks)
    (when (overblock--line-by-line-p lines keys)
      (while rows
        (let ((row (pop rows))
              (key (pop keys)))
          (cond
           ((not (overblock--carries-p row))
            (push (and lines (string-blank-p (car lines)) (pop lines) :gap)
                  chunks))
           ((and (> carry 1) lines
                 ;; The first row always carries: it begins the block.
                 (seq-some #'consp chunks)
                 (memq (overblock--ahead (car lines) (cons key keys))
                       '(1 2 3)))
            ;; The next line belongs further down: nothing here.
            (setq carry (1- carry))
            (push nil chunks))
           ((= carry 1)
            (setq carry 0)
            (push lines chunks)
            (setq lines nil))
           (t
            (setq lines (overblock--no-false-gap lines (cons key keys)))
            (setq carry (1- carry))
            (pcase-let ((`(,chunk . ,rest) (overblock--take lines keys)))
              (push chunk chunks)
              (setq lines rest))))))
      (nreverse chunks))))

(defun overblock--piece-text (chunk indent)
  "Return the lines of CHUNK as the text of one piece, INDENT columns in.
A line that shares a row with another starts at the left edge of the
window, so it is padded to INDENT, the column where the piece of the
row starts."
  (string-join chunk (if indent
                         (concat "\n" (make-string indent ?\s))
                       "\n")))

(defun overblock--pieces (block text)
  "Hang TEXT over the lines of BLOCK, a piece to a line.
Return the overlays that carry the pieces and the cloaks.

Each line of the rendering goes to the row it was rendered from, by
`overblock--align'; the rows that carry nothing go under a cloak.  A
rendering that is not line by line is dealt evenly (`overblock--spread').

A piece covers the text of its line and leaves the newline alone, so
every line keeps its height; `overblock--piece' makes one.

A line without text cannot carry a piece.  Under a longer rendering a
blank line stays in view, unless a cloak is already open: a cloak that
starts after a row of two rendered lines would put the wheel step on
its newline, in invisible text, and redisplay would jump to the top of
the buffer.  Under a shorter rendering every line without a piece goes
under a cloak."
  (pcase-let* ((indent (overblock-get block :indent))
               (end (overblock--region-end block))
               (rows (overblock--piece-rows block end))
               (slots (max 1 (seq-count #'overblock--carries-p rows)))
               (`(,long . ,lines) (overblock--piece-lines text slots))
               (chunks (or (overblock--align rows lines)
                           (overblock--spread rows lines slots long)))
               (parts nil)
               (cloak-from nil))
    (pcase-dolist (`(,bol ,from ,to ,blank) rows)
      (let ((chunk (pop chunks)))
        (if (and (null chunk) (not (and long blank (null cloak-from))))
            (setq cloak-from (overblock--cloak-from cloak-from bol))
          (when (and chunk cloak-from)
            (setq parts (nconc (overblock--cloak-to block cloak-from bol)
                               parts)
                  cloak-from nil))
          ;; At FROM, also for two lines: `current-column' counts a
          ;; display string over the indentation.
          (when (consp chunk)
            (push (overblock--piece block from to
                                    (overblock--piece-text chunk indent))
                  parts)))))
    (when cloak-from
      (setq parts (nconc (overblock--cloak-to block cloak-from end) parts)))
    ;; Nils where a guard found no newline to draw.
    (nreverse (delq nil parts))))

(defun overblock--anchor-rows (lead strings)
  "Return STRINGS as the rows of an anchor, after LEAD, in a plain face.
Without a face of its own a row wears that of the line it ends,
`hl-line' beside a figure among them.  The break that ends the line
itself keeps it."
  (let ((rows (concat lead (string-join strings "\n"))))
    (add-face-text-property (length lead) (length rows)
                            overblock--plain t rows)
    rows))

(defun overblock--attach (block shown)
  "Show the header and the body of SHOWN after BLOCK.
SHOWN is the property list of what the block shows, or nil for a block
that shows nothing.  Each is on a row of its own.

A row that must be an overlay string is the after-string of the
anchor: a display string ignores `:align-to', and swallows an image.

A plain body is the display property of the newline that ends the
region, the cheapest place for text.

The newline is never hidden: with it replaced by an empty display
string, `pixel-scroll-precision-scroll-up' signals beginning-of-buffer
errors and cannot pass the block.

Each string carries the line breaks of its own rows."
  (let* ((header (plist-get shown :header))
         (body (plist-get shown :body))
         (newline (overblock-get block :newline))
         (on-display (and body
                          (not (overblock-image-in body))
                          ;; Without a live newline overlay, the body goes
                          ;; on the anchor.
                          (overlayp newline)
                          (overlay-buffer newline)))
         ;; The rows on the anchor, in order.
         (strings (delq nil (list header (unless on-display body))))
         ;; A break first, unless the region ends in a blank line. Without
         ;; the narrowing, as overlay positions ignore it.
         (lead (if (eq (without-restriction
                         (char-before (overlay-end block)))
                       ?\n)
                   ""
                 "\n")))
    (overlay-put block 'after-string
                 (when strings (overblock--anchor-rows lead strings)))
    (when (and newline (overlay-buffer newline))
      (overblock--dress-newline block newline
                                (when on-display
                                  (concat (if header "\n" lead) body "\n"))
                                strings))))

(defun overblock--dress-newline (block newline display rows)
  "Give NEWLINE of BLOCK its DISPLAY, nil for none, and dress it.
Under ROWS on the anchor the newline ends the last of them, and
`hl-line' (-50) would paint beside it, so it outranks `hl-line' then."
  (overlay-put newline 'display display)
  (overlay-put newline 'priority (if rows -40 -60))
  (overblock--dress block newline))

(defun overblock--stale-hook (block after beg end &optional _length)
  "Take BLOCK down where the text it covers, BEG..END, really changed.
AFTER marks the call that follows the change; see
`overblock-stale-when-edited', which puts this on a block and names
the function that takes it down.

An insertion is judged on the call after the change, when its text
exists.  A deletion is judged before, because the anchor evaporates
with the text it covers and no call follows.

A block survives one insertion: a single newline at the end of the
buffer, such as the one `require-final-newline' adds on save.  It
changes nothing the block shows.  A second character reaches the
interior of the anchor and takes the block down.  The end is that of
the whole buffer, not of a narrowing."
  (when (if after
            (not (and (equal (buffer-substring-no-properties beg end) "\n")
                      (= end (without-restriction (point-max)))))
          (/= beg end))
    (overblock-take-down block)))

(defun overblock-take-down (block)
  "Take BLOCK down the way its maker asked, or delete it.
The `:stale' function given to `overblock-stale-when-edited' removes
a block with all that belongs to it, such as a bar above a rendered
cell.  The region it covered is remembered, so the live cycle leaves
it as source while point stays in it; see `overblock-live--open'."
  (when (overlay-buffer block)
    (setq overblock-live--open (cons (copy-marker (overlay-start block))
                                     (copy-marker (overlay-end block) t))))
  (funcall (or (overblock-get block :stale) #'overblock-delete) block))

(defun overblock-show-rendering (beg end rendered face &rest props)
  "Show RENDERED over BEG..END in FACE, and return the block.
PROPS are those of `overblock-show'.  Its `:keymap' and `:help-echo'
also go on the rendering where it has none of its own: shr writes a
keymap on a link, and that one stays.

Return nil where RENDERED holds nothing to show, so a line that renders
to a lone HTML comment stays as it is.  Any edit of the region takes
the block down (see `overblock-stale-when-edited'): typing, a
replacement over the buffer, a macro, an undo.  Point moving into the
region reveals nothing.

Every mode here that renders text over its own source uses this."
  (when-let* (((not (string-empty-p (string-trim rendered))))
              (block (apply #'overblock-show beg end
                            :over (overblock-fill-props
                                   (overblock-faced rendered face)
                                   'keymap (plist-get props :keymap)
                                   'help-echo (plist-get props :help-echo))
                            props)))
    (overblock-stale-when-edited block)
    block))

(defun overblock-stale-when-edited (block &optional function)
  "Take BLOCK down on the next edit of the text it covers.
FUNCTION is called with the block instead, where the caller has more to
do than delete it, such as a bar to remove or a move to ignore.

Three hooks: `modification-hooks' runs for a change inside an overlay,
`insert-in-front-hooks' for one at its first character and
`insert-behind-hooks' for one at its end.  An anchor stops one
character short of the newline that ends its region, so typing at the
end of the last line is an insertion at the end."
  (when function (overblock-set block :stale function))
  (let ((hooks (list #'overblock--stale-hook)))
    (overlay-put block 'modification-hooks hooks)
    (overlay-put block 'insert-in-front-hooks hooks)
    (overlay-put block 'insert-behind-hooks hooks)))

(defvar-local overblock-edit--source nil
  "What this edit buffer feeds, as (BUFFER BEG END PUT).
PUT is the function that writes the edited text back; see
`overblock-edit-in-buffer'.")

(defvar-keymap overblock-edit-mode-map
  :doc "Keymap of `overblock-edit-mode'.
The two keys of `org-edit-special', and no other."
  "C-c C-c" #'overblock-edit-commit
  "C-c C-k" #'overblock-edit-abort)

(define-minor-mode overblock-edit-mode
  "Edit the text under a block, as `org-edit-special' edits a source block."
  ;; The :lighter also keeps the body out of the deprecated positional
  ;; INIT-VALUE argument.
  :lighter " BlockEdit")

(defun overblock-edit-in-buffer (beg end props)
  "Edit the text of BEG..END in a buffer of its own, and show it.
PROPS is a plist:

  :name   the name of the edit buffer.  One buffer for each region,
          not one for the file, so a second edit does not replace the
          first.  Put a line number in the name.
  :label  what the buffer calls the thing, for the hint on its header
          line.
  :mode   the major mode of the edit buffer.
  :text   called with BEG and END in this buffer; returns the plain
          text to edit.
  :put    called with BEG, END and the edited string, in this buffer,
          and writes it back.  It also renders it again.

A pending edit of the same region comes back as it is, not a new copy
of the file text.  The test is the region, not the name: two regions
can have the same line number at different times.  A pending edit of
another region is discarded only after the reader confirms."
  (let* ((source (current-buffer))
         ;; Markers, so a commit lands on the region even after the
         ;; source buffer changed above it.
         (beg (copy-marker beg))
         (end (copy-marker end t))
         (text (funcall (plist-get props :text) beg end))
         (put (plist-get props :put))
         (buffer (get-buffer-create (plist-get props :name))))
    (with-current-buffer buffer
      (let ((pending (and overblock-edit-mode (buffer-modified-p)))
            (mine (equal (take 3 overblock-edit--source)
                         (list source beg end))))
        (when (and pending (not mine)
                   (not (yes-or-no-p
                         (format "Discard the unsaved edit of another %s? "
                                 (plist-get props :label)))))
          (user-error "Kept the unsaved edit"))
        (unless (and pending mine)
          (erase-buffer)
          (insert text)
          (funcall (plist-get props :mode))
          (overblock-edit-mode)
          ;; The keys come from the keymap, so the hint stays true when
          ;; the bindings or the prefix change.
          (setq header-line-format
                (substitute-command-keys
                 (format " %s: \\[overblock-edit-commit] applies, \
\\[overblock-edit-abort] discards"
                         (capitalize (plist-get props :label)))))
          (set-buffer-modified-p nil)))
      (setq overblock-edit--source (list source beg end put)))
    (pop-to-buffer buffer)))

(defun overblock-edit-commit ()
  "Put the edited text back where it came from."
  (interactive)
  (pcase-let ((`(,source ,beg ,end ,put) overblock-edit--source)
              ;; Without properties: the edit buffer marks its text as
              ;; fontified, and the source buffer would keep its faces.
              (text (string-trim-right
                     (buffer-substring-no-properties (point-min) (point-max)))))
    (unless (and source (buffer-live-p source))
      (user-error "The buffer this text came from is gone"))
    (with-current-buffer source
      (save-excursion (funcall put beg end text)))
    (quit-window t)))

(defun overblock-edit-abort ()
  "Discard the edit."
  (interactive)
  (quit-window t))

(defun overblock-goto-event (event)
  "Select the window of EVENT and move point to the click.
Any other event leaves point where it is: a command reads EVENT from
`last-input-event', so it can be any event, such as a `switch-frame'
or a click on a mode line.

A command bound to the mouse calls this before `overblock-at'."
  (when-let* (((consp event))
              ;; `event-start' signals on some events.
              (posn (ignore-errors (event-start event)))
              ((consp posn))
              ;; An event from a keyboard macro can name no window.
              (window (posn-window posn))
              ((window-live-p window))
              (pos (posn-point posn)))
    (select-window window)
    (goto-char pos)))

(defun overblock-only-in (mode &rest parents)
  "Leave the minor mode MODE off unless the major mode derives from PARENTS.
MODE is the variable of the mode, which `define-minor-mode' has just
set; this resets it and signals.  Each mode reads one kind of buffer,
such as markdown or Python."
  (unless (seq-some #'derived-mode-p parents)
    (set mode nil)
    (user-error "%s is for %s buffers" mode
                (mapconcat #'symbol-name parents " or "))))

(defcustom overblock-live-idle 0.2
  "Seconds of quiet before a live cycle renders again.
Rendering happens when the reader stops, not on every command, such as
each repeat of a held `C-n'.  One value for every live cycle."
  :type 'number
  :group 'overblock)

(defvar-local overblock-live--specs nil
  "How this buffer renders itself: one (KIND RENDER) a live cycle.
A buffer can have several, such as the markdown cells and the doc
strings of a notebook, each from a mode of its own.
`overblock-live-start' adds one and `overblock-live-stop' removes it.")

(defvar-local overblock-live--timer nil
  "The timer that renders what the reader has finished with.")

(defvar-local overblock-live-source-at-point t
  "Whether the region point is in shows its source, wherever point went.
With t, the region at point is not rendered, and renders when point
leaves it.  A rendering already there stays.  A mode sets this nil
where the reader works with the rendering (a markdown cell of a
notebook, a doc string among code): then only the region a rendering
came off stays source until point leaves it (see
`overblock-live--open').")

(defvar-local overblock-live--open nil
  "The region a rendering last came off, as (BEG . END) markers, or nil.
While point stays in it the region is not rendered again, whatever
`overblock-live-source-at-point' says; `overblock-live--settle' lets
it go once point has left.")

(defun overblock-live-drop-if (pred)
  "Take down every live block of this buffer that PRED answers to.
PRED is called with a block.  Nothing is drawn here: the live cycle
draws when the reader stops.  Public for a package that must render
again, after a theme change or when a preview arrives."
  (when overblock-live--specs
    (dolist (spec overblock-live--specs)
      (dolist (block (overblock-in (point-min) (point-max) (car spec)))
        (when (funcall pred block)
          (overblock-delete block))))
    (overblock-live--settle)))

(defun overblock-live--settle (&rest _)
  "Render the buffer again once the reader has stopped.
Point does not take a rendering off: `overblock-live-edit' and an
edit of the region do.  Scrolling moves point through renderings, and
revealing them would make the buffer grow and shrink under the window.

An active region is the exception: the renderings it reaches come
down, so the reader copies or cuts the source.  They come back when
the mark is gone, through the timer that renders what has no
rendering."
  (when (use-region-p)
    (dolist (spec overblock-live--specs)
      (mapc #'overblock-take-down
            (overblock-in (region-beginning) (region-end) (car spec)))))
  (pcase overblock-live--open
    (`(,from . ,to)
     (unless (<= from (point) to)
       (overblock-live--close))))
  (when (timerp overblock-live--timer)
    (cancel-timer overblock-live--timer))
  (setq overblock-live--timer
        (run-with-idle-timer
         overblock-live-idle nil
         (let ((buffer (current-buffer)))
           (lambda ()
             (when (buffer-live-p buffer)
               (with-current-buffer buffer
                 (dolist (spec overblock-live--specs)
                   (funcall (nth 1 spec))))))))))

(defun overblock-live--close ()
  "Forget the region a rendering last came off, and free its markers."
  (pcase overblock-live--open
    (`(,from . ,to) (set-marker from nil) (set-marker to nil)))
  (setq overblock-live--open nil))

(defun overblock-live-wanted-p (beg end kind)
  "Return non-nil where the region BEG..END still wants a rendering of KIND.
Three regions do not: one that has a rendering already, one the active
region reaches, and the one the reader is at.  Which region that is
depends on `overblock-live-source-at-point': the region point is in,
or only the one a rendering came off while point is still in it.  No
region wants one where no live cycle of KIND is on.

A process caller asks twice: before the conversion, and when the
answer comes back, because the reader can click, type, move or turn
the mode off meanwhile."
  (not (or (not (assq kind overblock-live--specs))
           (if overblock-live-source-at-point
               (<= beg (point) end)
             (pcase overblock-live--open
               (`(,from . ,to)
                (and (<= from (point) to) (< beg to) (> end from)))))
           (and (use-region-p)
                (< beg (region-end))
                (> end (region-beginning)))
           (overblock-in beg end kind))))

;;;###autoload
(defun overblock-live-edit (&optional event)
  "Show the source of the region at point, or of the one EVENT clicked.
The rendering comes down; it renders again when the reader has moved
on and stopped.  A mode binds this to the mouse."
  (interactive (list last-input-event))
  (overblock-goto-event event)
  (when-let* ((block (seq-some (lambda (spec)
                                 (or (overblock-at (car spec))
                                     (car (overblock-in (pos-bol) (pos-eol)
                                                        (car spec)))))
                               overblock-live--specs)))
    (overblock-take-down block)))

(defun overblock-live-start (kind render)
  "Keep this buffer rendered, and let the reader edit what they click.
KIND names the blocks, as for `overblock-show'.  RENDER is called with
no arguments to render what is not rendered yet; there a mode can send
all regions through one converter.  `overblock-live-idle' is the quiet
before RENDER is called again.

RENDER is called once here and then whenever the reader stops.  It
must leave alone what `overblock-live-wanted-p' says wants no
rendering.

A rendering comes off when the reader asks (`overblock-live-edit',
which a mode binds to a click) and when its region is edited (see
`overblock-stale-when-edited').  Point moving into a rendering reveals
nothing, so scrolling does not make the text grow and shrink."
  (setf (alist-get kind overblock-live--specs) (list render))
  (setq overblock--columns (overblock-window-columns))
  (add-hook 'post-command-hook #'overblock-live--settle nil t)
  ;; A rendering is built for its columns (see
  ;; `overblock--width-changed'). Both hooks are buffer-local: the
  ;; first runs for every window of this buffer whose frame changed,
  ;; the second for the text scale.
  (add-hook 'window-configuration-change-hook #'overblock--width-changed nil t)
  (add-hook 'text-scale-mode-hook #'overblock--width-changed nil t)
  (funcall render))

(defun overblock-live-stop (kind)
  "Stop rendering the blocks of KIND in this buffer, and take them off.
The hooks and the timer go with the last cycle of the buffer."
  (setq overblock-live--specs (assq-delete-all kind overblock-live--specs))
  ;; Nil, not the bounds: under a narrowing those would leave the
  ;; blocks outside it, cloaks among them.
  (overblock-clear nil nil kind)
  (unless overblock-live--specs
    (remove-hook 'post-command-hook #'overblock-live--settle t)
    (remove-hook 'window-configuration-change-hook #'overblock--width-changed t)
    (remove-hook 'text-scale-mode-hook #'overblock--width-changed t)
    (when (timerp overblock-live--timer)
      (cancel-timer overblock-live--timer)
      (setq overblock-live--timer nil))
    (overblock-live--close)))

(defun overblock-refresh (block)
  "Show BLOCK again from its properties.
Call it after `overblock-set'.  Everything the block shows is made
again, so nothing has to be saved.

A deleted block draws nothing: it has no start.  The drawing happens in
the buffer of the block, because `make-overlay' uses the current
buffer."
  (when-let* ((buffer (overlay-buffer block)))
    (with-current-buffer buffer
      (mapc #'delete-overlay (overblock-get block :parts))
      (overblock-set block :parts nil)
      ;; A hidden block shows nothing.
      (let ((shown (unless (overblock-get block :hidden)
                     (overlay-get block 'overblock))))
        (overblock--dress block block)
        (when-let* ((over (plist-get shown :over)))
          (overblock-set block :parts (overblock--pieces block over)))
        (overblock--attach block shown)
        block))))

(defun overblock--image-spec (display)
  "Return the image in the DISPLAY spec, or nil.
Emacs 31 slices an image taller than `shr-sliced-image-height' into a
row for each line, and a slice reads ((slice X Y W H) IMAGE): the
image is its second element."
  (cond ((eq (car-safe display) 'image) display)
        ((and (eq (car-safe (car-safe display)) 'slice)
              (eq (car-safe (cadr display)) 'image))
         (cadr display))))

(defun overblock-image-in (text)
  "Return the `display' spec of the first image in TEXT, or nil.
The value is (image . PLIST), so a caller can read `:data' or `:type'
from it.  A `raise' spec, which shr uses for a superscript, is no
image.  For a slice of an image, the image inside it is returned."
  (let ((len (length text))
        (pos 0)
        img)
    ;; Run to run: a display property that is no image can cover a
    ;; long run.
    (while (and (not img)
                (setq pos (text-property-not-all pos len 'display nil text)))
      (let ((disp (get-text-property pos 'display text)))
        (if-let* ((image (overblock--image-spec disp)))
            (setq img image)
          (setq pos (or (next-single-property-change pos 'display text) len)))))
    img))

(defun overblock-image-label (text)
  "Return TEXT with every image in it replaced by \"[figure]\".
For a display that draws no images: an image is on a space, which
would show as a blank row."
  (let ((label "[figure]")
        (len (length text))
        (pos 0)
        pieces)
    (while (< pos len)
      (let ((next (or (next-single-property-change pos 'display text) len)))
        (push (if (overblock--image-spec (get-text-property pos 'display text))
                  label
                (substring text pos next))
              pieces)
        (setq pos next)))
    (apply #'concat (nreverse pieces))))

(defun overblock--image-capped (image limit)
  "Return IMAGE with its height held to LIMIT, or nil where it has one.
An image that already has a `:max-height' keeps it."
  (unless (plist-get (cdr image) :max-height)
    (cons 'image (plist-put (copy-sequence (cdr image)) :max-height limit))))

(defun overblock--image-runs (string)
  "Return a list of (BEG END IMAGE SLICED) for the images STRING draws.
Each is one run of a `display' property.  SLICED is non-nil where the
run draws a slice of the image rather than the whole of it."
  (let ((pos 0)
        (len (length string))
        runs)
    (while (< pos len)
      (let* ((next (or (next-single-property-change pos 'display string) len))
             (spec (get-text-property pos 'display string))
             (image (overblock--image-spec spec)))
        (when image
          (push (list pos next image (not (eq image spec))) runs))
        (setq pos next)))
    (nreverse runs)))

(defun overblock-image-cap (string)
  "Return STRING with every image in it capped to `overblock-image-height'.
STRING itself is not touched: this copies before it caps, so a caller
keeps the original to save or to pop out.

Emacs 31 slices an image taller than `shr-sliced-image-height' into a
row for each line of the window it was rendered in, as ((slice X Y W
H) IMAGE).  Slicing does not make the image smaller, and an image
cannot be capped under its slices, whose fractions are for the old
height.  So a run of slices of one image becomes the whole image,
capped, on its first row, and nothing on the later rows."
  (if-let* ((limit (overblock-image-limit))
            ((overblock-image-in string)))
      (overblock--image-cap-runs (copy-sequence string) limit)
    string))

(defun overblock--image-cap-runs (string limit)
  "Cap every image of STRING to LIMIT pixels, in place, and return STRING.
STRING is the caller's copy to write on."
  (let ((seen nil))
    (pcase-dolist (`(,beg ,end ,image ,sliced)
                   (overblock--image-runs string))
      (if (and sliced (memq image seen))
          ;; A later slice: the whole image is on the first.
          (put-text-property beg end 'display "" string)
        (when sliced (push image seen))
        (when-let* ((capped (overblock--image-capped image limit)))
          (put-text-property beg end 'display capped string)))))
  string)

(defun overblock-image-limit ()
  "Return how many pixels tall an image may be drawn, or nil for no cap.
The share is `overblock-image-height' of the window that shows the
buffer.  A block can be drawn while its buffer is not shown, and then
the selected window is a guess at the size; a small guess only draws a
smaller figure."
  (when-let* (((numberp overblock-image-height))
              ((> overblock-image-height 0))
              (window (or (get-buffer-window nil t) (selected-window)))
              (limit (round (* overblock-image-height
                               (window-body-height window t))))
              ((> limit 0)))
    limit))

;;;; Alignment made literal

(defun overblock--space-columns (spec column)
  "Return the columns that the space SPEC covers at COLUMN, or nil.
A `:align-to' spec names where the space ends and a `:width' spec how
wide it is.  Both count pixels in a list and characters in a bare
number; a terminal pixel is a column, a graphic one is
`frame-char-width' wide."
  (let* ((plist (cdr spec))
         (to (plist-get plist :align-to))
         (width (plist-get plist :width))
         ;; A number, or a list that starts with one. A spec such as
         ;; `(- right (N))' from `overblock-bar' aligns to the window
         ;; and stays as it is: this can run in a process filter, where
         ;; an error is costly. At most 10000 columns.
         (chars (lambda (n)
                  (let ((pixels (cond ((and (consp n) (numberp (car n)))
                                       (car n))
                                      ((numberp n) (* n (frame-char-width))))))
                    (and pixels
                         (min 10000 (round pixels (frame-char-width))))))))
    (cond ((and to (funcall chars to))
           (max 0 (- (funcall chars to) column)))
          ((and width (funcall chars width))
           (max 0 (funcall chars width))))))

(defun overblock-flatten-alignment ()
  "Turn the space stretches of this buffer into real spaces.
`overblock-flattened' is the string form of this.
shr aligns table columns with `(space :align-to (N))' display specs,
and vtable, with which comint-mime shows a DataFrame, with
`(space :width (N))'.  Both count from the window they were measured
in, and a block is shown with another indentation (line numbers,
margins).  Literal padding aligns anywhere.  The walk is left to
right, so `current-column' sees the padding inserted before it."
  (goto-char (point-min))
  (let (match)
    (while (setq match (text-property-search-forward 'display))
      (let ((spec (prop-match-value match)))
        (when (eq (car-safe spec) 'space)
          (let* ((beg (prop-match-beginning match))
                 (end (prop-match-end match))
                 (pad (overblock--space-columns
                       spec (save-excursion (goto-char beg)
                                            (current-column)))))
            (when pad
              (goto-char beg)
              (delete-region beg end)
              ;; Zero inserts nothing: the column is already there.
              (insert (make-string pad ?\s)))))))))

(defun overblock-flattened (text)
  "Return TEXT with its space stretches as real spaces.
See `overblock-flatten-alignment' for why a copy needs them literal."
  (with-temp-buffer
    (insert text)
    (overblock-flatten-alignment)
    (buffer-string)))

;;;; Bars, buttons and glyphs

;; A bar is a line with text at the left and icons at the right
;; window edge; a button is a label that answers a click; a glyph is a
;; character this frame can draw.

(defconst overblock-button-type
  '(repeat
    (list (symbol :tag "Key")
          (repeat :tag "Glyph candidates" string)
          (string :tag "Tooltip")
          (function :tag "Command")
          (choice :tag "Shows"
                  (const :tag "Always" t)
                  (const :tag "With an image" image)
                  (const :tag "With output" lines)
                  (const :tag "While it is still being written" running)
                  (const :tag "Once it is written" done))))
  "The customize type of a list of header buttons.")

(defun overblock-faced (string face)
  "Add FACE below the faces STRING already carries.  Return STRING.
STRING is modified in place.
An overlay string without a face inherits one from the buffer text
next to it, so every block needs at least a base face."
  (add-face-text-property 0 (length string) face t string)
  string)

(defun overblock-fill-props (string &rest properties)
  "Set the PROPERTIES that STRING does not carry yet.
PROPERTIES is a plist, and STRING is modified in place and returned.
shr gives a link its own keymap and help echo; a plain `propertize'
would replace both."
  (let ((len (length string)))
    (while properties
      (let ((prop (pop properties))
            (value (pop properties))
            (pos 0))
        (while (< pos len)
          (let ((next (or (next-single-property-change pos prop string) len)))
            (unless (get-text-property pos prop string)
              (put-text-property pos next prop value string))
            (setq pos next))))))
  string)

(defvar overblock--glyphs (make-hash-table :test #'equal)
  "What `overblock-glyph' answered, by display, font and candidates.
The answer does not change while a frame keeps its font, and
`char-displayable-p' asks the font backend for each character, five
times a second on a running header.")

(defvar overblock--button-rows (make-hash-table :test #'equal)
  "The icon row each set of descriptors and states draws.
The header of a running result is built five times a second, and its
buttons change only with the option, the image, the output or the
running flag.  The descriptors are part of the key, so a changed
option builds new rows.

The key also holds what `overblock-glyph' keys on (the kind of
display, the frame font and `overblock-terminal-glyphs'), so a graphic
frame and a terminal frame of one daemon get their own rows, and a
plain `setq' of the option takes effect.")

(defun overblock-bars-stale ()
  "Mark every bar of this buffer stale, so the next draw rebuilds it.
For a change that no bar can see: another glyph, another list of
buttons, another window width."
  (mapc #'overblock-bar-stale (overblock-bars)))

(defun overblock--forget-glyphs ()
  "Forget the glyphs answered so far, and draw the bars again.
The rows of buttons built from those glyphs go too.  This sets no
option; the `:set' of `overblock-terminal-glyphs' calls `set-default'
first."
  (clrhash overblock--glyphs)
  (clrhash overblock--button-rows)
  (overblock-bars-stale))

(defcustom overblock-terminal-glyphs nil
  "Whether this terminal draws the glyphs a graphic frame draws.
In a terminal, `char-displayable-p' tests the coding system, not the
font, so a missing character shows as an empty box.  A terminal
therefore gets the plain last candidate of every list, unless this is
non-nil because the terminal font has the icons.

The coding system is still tested."
  :type 'boolean
  ;; The default initializer calls `:set' before the functions it
  ;; calls are defined.
  :initialize #'custom-initialize-default
  :set (lambda (symbol value)
         (set-default symbol value)
         (overblock--forget-glyphs))
  :group 'overblock)

(defun overblock--glyph-drawn-p (candidate)
  "Return non-nil where this frame draws every character of CANDIDATE.
`char-displayable-p' tests the font on a graphic frame and the coding
system on a terminal.  `overblock-glyph' decides whether a terminal is
tested at all."
  (seq-every-p #'char-displayable-p candidate))

(defun overblock-glyph (&rest candidates)
  "Return the first of CANDIDATES this frame can draw.
The last candidate is the answer when none can be drawn, and in a
terminal unless `overblock-terminal-glyphs' is non-nil.

Every character of a candidate must be drawable, not only the first:
some start with a space."
  (with-memoization (gethash (list (display-graphic-p)
                                   (frame-parameter nil 'font)
                                   overblock-terminal-glyphs
                                   candidates)
                             overblock--glyphs)
    (or (and (or (display-graphic-p) overblock-terminal-glyphs)
             (seq-find #'overblock--glyph-drawn-p candidates))
        (car (last candidates)))))

;; The press runs the command, not the release: in the text area a
;; press reaches `mouse-drag-region', which keeps the release. The
;; release and the drag go to `ignore': a command that moves the text
;; under the pointer, such as a move button, turns the release into a
;; drag, which would leave a region.
(defvar overblock--button-keymaps (make-hash-table :test #'eq)
  "The keymap each button command is pressed through.
A keymap depends only on the command, and the header of a running
result is built five times a second.")

(defun overblock-button (label help command)
  "Return LABEL as a button.
A left click calls COMMAND, and HELP becomes the tooltip.  The keymap
is kept per command in `overblock--button-keymaps'."
  (propertize label 'mouse-face 'highlight 'help-echo help
              'keymap (with-memoization
                          (gethash command overblock--button-keymaps)
                        (define-keymap
                          "<down-mouse-1>" command
                          "<mouse-1>" #'ignore
                          "<drag-mouse-1>" #'ignore))))

(defun overblock-buttons (descriptors &optional imagep lines runningp)
  "Return the icon group that DESCRIPTORS ask for.
Each descriptor is (KEY GLYPHS HELP COMMAND WHEN), the shape of
`overblock-button-type' and of every button option here:

- KEY names the button for you; nothing else reads it.
- GLYPHS are the candidates for its label.  The first one the frame
  can draw wins, and the last one is the fallback, so put something
  every display has at the end.  The packages here use three: a nerd
  glyph, a character of an ordinary monospace font, and a short word
  (a word, because a letter such as `u' means nothing).  Make sure
  common fonts have the middle one.

  Every nerd glyph here is a codicon (names nf-cod-, the set of VS
  Code), because those shapes share one hairline weight and one size.
  `overblock-glyph' skips a glyph that an old nerd font lacks.  No
  candidate of a button is a candidate of another button of the same
  bar, or of a button with another meaning on another bar, so two
  buttons never look the same.

  A terminal also takes the last candidate, unless
  `overblock-terminal-glyphs' says its font has the icons.
- HELP is the tooltip.
- COMMAND runs on a click.
- WHEN says when the button shows: t always, `image' only with a
  picture in the result, `lines' only with output, `running' only
  while the region runs, and `done' only once it has ended.

IMAGEP says the block holds an image, LINES how many lines it has and
RUNNINGP that it is still being written, for a WHEN of `image',
`lines', `running' or `done'."
  (with-memoization (gethash (list descriptors imagep (> (or lines 0) 0)
                                   runningp (display-graphic-p)
                                   (frame-parameter nil 'font)
                                   overblock-terminal-glyphs)
                             overblock--button-rows)
    (overblock--buttons descriptors imagep lines runningp)))

(defun overblock--buttons (descriptors imagep lines runningp)
  "Return the icon group DESCRIPTORS ask for, built afresh.
IMAGEP, LINES and RUNNINGP are those of `overblock-buttons', which is
this function behind a table."
  (concat
   (string-join
    (seq-keep
     (lambda (descriptor)
       (pcase-let ((`(,_key ,glyphs ,help ,command ,when) descriptor))
         (when (pcase when
                 ('image imagep)
                 ('lines (> (or lines 0) 0))
                 ('running runningp)
                 ('done (not runningp))
                 (_ t))
           ;; The space after the glyph is part of the button, which
           ;; makes the target two columns wide.
           (overblock-button (concat (apply #'overblock-glyph glyphs) " ")
                             help command))))
     descriptors)
    " ")))

(defconst overblock--pixel-width-takes-a-buffer
  (> (cdr (func-arity #'string-pixel-width)) 1)
  "Whether `string-pixel-width' takes the buffer to measure in.
Emacs 31 does; an older one measures without any face remapping.")

(defun overblock--pixel-width (string)
  "Return the width of STRING in pixels, as this buffer would draw it.
Emacs 31 takes the buffer whose face remapping to measure with.  An
older one measures without remapping, so under `text-scale-mode' or
`buffer-face-mode' the width is wrong there."
  ;; Through `apply', so the compiler of an older Emacs does not reject
  ;; two arguments. The arity is read once, at load.
  (apply #'string-pixel-width string
         (when overblock--pixel-width-takes-a-buffer
           (list (current-buffer)))))

(defun overblock--window-min (measure)
  "Return the smallest MEASURE of the windows that show this buffer.
The smallest, because one string is drawn in all of them.  Only
`visible' frames count, not invisible or iconified ones.

Return nil where no visible window shows the buffer.  A bar is then
not cut at all.

Never below zero: `window-max-chars-per-line' is negative where the
font is much larger than the window."
  (when-let* ((windows (get-buffer-window-list nil nil 'visible)))
    ;; The measures select their window, which sets point of this
    ;; buffer to the point of that window. A caller that walks with
    ;; point, such as the walk that draws the bars, would loop.
    (save-excursion
      (max 0 (apply #'min (mapcar measure windows))))))

(defun overblock-window-width ()
  "Return the pixel width of the narrowest window that shows this buffer.
`window-max-chars-per-line' leaves out the line-number area and the
margins, unlike `window-body-width', and uses the font of the window.
Return nil where no window shows the buffer; see
`overblock--window-min'."
  (overblock--window-min (lambda (window)
                           (* (window-max-chars-per-line window)
                              (window-font-width window)))))

(defun overblock-window-columns ()
  "Return the columns of the narrowest window that shows this buffer.
Columns, not pixels: `window-max-chars-per-line' uses the font of the
window, which `text-scale-adjust' makes differ from that of the frame.

Return nil where no window shows the buffer; see
`overblock--window-min'."
  (overblock--window-min #'window-max-chars-per-line))

(defvar-local overblock-width-functions nil
  "Functions called with no arguments when this buffer changes width.
Buffer-local, and run from `overblock--width-changed' when a window
that shows the buffer changes width or text scale.  A mode that builds
bars for a width adds what draws them again; the live cycle handles
its own blocks.

`overblock-live-start' adds the hooks that lead here, so this runs only
while a live cycle is on.")

(defun overblock--width-changed ()
  "Follow a change of the width this buffer is drawn at.
For the buffer-local `window-configuration-change-hook' (a split, a
resize, a frame size change, a window that shows this buffer again)
and for `text-scale-mode-hook'.  In columns, not pixels: the text scale
changes the columns, and a rendering is filled to columns.

A rendering is filled to its width, and its rules reach the window
edge.  The blocks of the live cycle carry the width they were built
for (see `overblock-show'); those built for another are dropped, and
the cycle renders them again when the reader stops.  Dropping, not
rendering, so a drag of the window edge does not start a converter at
every column.

Every bar is marked stale: a bar is cut in pixels for the current
font, and `overblock-bar-draw' does not redraw a label it has seen."
  (when-let* ((columns (overblock-window-columns))
              ((not (eql columns overblock--columns))))
    (setq overblock--columns columns)
    (when overblock-live--specs
      (dolist (spec overblock-live--specs)
        (dolist (block (overblock-in (point-min) (point-max) (car spec)))
          (unless (eql columns (overlay-get block 'overblock-columns))
            (overblock-delete block))))
      (overblock-live--settle))
    (overblock-bars-stale)
    (run-hooks 'overblock-width-functions)))

(defun overblock--cut (text face room)
  "Return TEXT cut with an ellipsis to ROOM pixels, drawn in FACE.
Return TEXT itself where it fits, and where ROOM is nil (no window
shows the buffer).

Pixels, not columns: a label starts with an icon glyph, which a
fallback font draws wider than a character cell.

The columns give a first guess, then one character comes off at a
time."
  (if (or (null room)
          (<= (overblock--pixel-width (propertize text 'face face)) room))
      text
    (let ((cut (truncate-string-to-width
                text (max 1 (/ room (frame-char-width))) nil nil t)))
      (while (and (> (length cut) 1)
                  (> (overblock--pixel-width (propertize cut 'face face)) room))
        (setq cut (concat (substring cut 0 -2) "…")))
      cut)))

(defun overblock--bar-left (glyph label)
  "Return the left of a bar: GLYPH, a space, LABEL.
Every bar here has this shape.  An empty part leaves no space: a rule
has neither and is a bare row."
  (string-join (seq-remove #'string-empty-p (list glyph label)) " "))

(defun overblock--bar-padded (left icons face indent)
  "Return LEFT and ICONS in FACE as a row INDENT columns in.
This is a row of a rendering, so the gap is made of spaces: a row is
a display property, and display properties do not nest, so a stretch
in it draws nothing.

INDENT is the column the row starts at, as every row of a doc string
rendering does.  Those columns are not part of the padding.

The padding is counted in columns and measured in pixels, because a
nerd glyph draws wider than it counts.  A column of slack keeps the row
from wrapping.  The row is built for the current width, which the
layer writes on the block for `overblock--width-changed'."
  (let* ((width (overblock-window-width))
         (cell (frame-char-width))
         ;; The room of LEFT: the window less the indent, the icons and
         ;; a cell of slack. A longer label is cut with an ellipsis.
         (room (and width (- width (* (1+ indent) cell)
                             (overblock--pixel-width (propertize icons 'face face))
                             cell)))
         (left (overblock--cut left face room))
         (text (overblock-faced (concat left icons) face))
         (pad (and width (floor (- width
                                   (* (1+ indent) cell)
                                   (overblock--pixel-width text))
                                cell))))
    (if (and pad (> pad 0))
        (overblock-faced (concat left (make-string pad ?\s) icons) face)
      text)))

(defun overblock-bar (glyph label icons face &optional indent)
  "Return a header line: GLYPH, LABEL, and ICONS at the right window edge.
All of it in FACE.  `overblock--bar-left' joins GLYPH and LABEL;
ICONS is what `overblock-buttons' returned.  INDENT makes it a row of
a rendering instead, drawn by `overblock--bar-padded'.

The alignment is in pixels: icon glyphs draw wider than `string-width'
counts, and (N) in the display spec means N pixels.  The slack is one
character cell in a graphic frame and three columns in a terminal (see
`overblock--bar-stretched').

The label is cut, in pixels, where the icons leave no room for it: the
stretch shrinks to nothing after the label passes its target, and the
icons would wrap.  The room is what `overblock-window-width' measures,
less the icons, the slack and one more character cell.

A buffer in no visible window is not cut at all, because the cut stays
in the string.  A finished header is rebuilt only when what it shows
or the width changes."
  (let ((left (overblock--bar-left glyph label)))
    (if indent
        (overblock--bar-padded left icons face indent)
      (overblock--bar-stretched left icons face))))

(defun overblock--bar-stretched (left icons face)
  "Return LEFT and ICONS in FACE, held apart by a stretch to the edge."
  (let* (;; Slack, also in a graphic frame: a row that ends at the
         ;; right edge exactly can wrap or not, at the whim of
         ;; redisplay. A terminal keeps three columns, one of them for
         ;; the ellipsis of an outline fold.
         (slack (if (display-graphic-p) (frame-char-width) 3))
         (width (+ (overblock--pixel-width (propertize icons 'face face))
                   slack))
         (available (overblock-window-width))
         ;; One more column of slack: a label cut exactly still wraps.
         (room (and available (- available width (frame-char-width)))))
    ;; Not even the icons fit, so they go, and the bar stays one row.
    (when (and room (<= room 0))
      (setq icons "" width slack left "…")
      ;; In a window one character wide, the ellipsis is the whole bar.
      (when (< available (* 2 (frame-char-width)))
        (setq width 0 slack 0)))
    (setq left (overblock--cut left face room))
    (overblock-faced
     (concat left
             (propertize " " 'display
                         `(space :align-to (- right (,width))))
             icons)
     face)))

(defun overblock-bar-over (beg end)
  "Return an overlay that shows a bar in place of the text BEG..END.
The text stays in the buffer and draws as nothing until
`overblock-bar-draw' puts the bar on this overlay, again whenever the
label or the window width changes.

Most of the bar is an overlay string, not a display property: a
display string ignores (space :align-to (- right ...))."
  (let ((ov (make-overlay beg end nil t)))
    (overlay-put ov 'evaporate t)
    (overlay-put ov 'display "")
    ;; A bar from the start, of kind t, so a `C-g' before
    ;; `overblock-bar-draw' leaves an overlay that `overblock-bars'
    ;; still finds.
    (overlay-put ov 'overblock-bar t)
    ov))

(defun overblock-bar-draw (ov kind glyph label icons)
  "Draw the bar of KIND on OV: GLYPH, LABEL, and ICONS at the edge.
OV comes from `overblock-bar-over'.  KIND is the word of the caller
for what the bar stands on, which `overblock-bar-kind' returns.
`overblock--bar-left' joins GLYPH and LABEL, and the bar has the face
`overblock-bar'.

Where nothing changed the bar stays as it is: a caller draws from a
change hook, and a walk over a long buffer would rebuild every bar.
The text of the line is compared too, because the label is usually on
it.  The width is not: `overblock--width-changed' marks every bar
stale."
  (let ((state (list (buffer-substring-no-properties (overlay-start ov)
                                                     (overlay-end ov))
                     glyph label icons)))
    (overlay-put ov 'overblock-bar kind)
    (unless (equal state (overlay-get ov 'overblock-bar-state))
      (overlay-put ov 'overblock-bar-state state)
      (overlay-put ov 'overblock-bar-text
                   (overblock-bar glyph label icons 'overblock-bar))
      (overblock--bar-wear ov (overlay-get ov 'overblock-bar-text)))))

(defun overblock-bar-line (bol eol kind glyph label icons)
  "Draw the bar of KIND over the line BOL..EOL, and return its overlay.
GLYPH, LABEL and ICONS are those of `overblock-bar-draw'.  Every bar on
a line of the buffer (the boundary line of a cell, the header of an R
chunk) is drawn through here.

A bar of another kind on that line is not reused: the bar of a
rendered markdown cell belongs to its block.

The overlay is moved to the line every time, because text typed at its
end falls outside it."
  (let* ((there (overblock-bar-in bol (min (point-max) (1+ eol))))
         (ov (if (eq (overblock-bar-kind there) kind)
                 there
               (overblock-bar-over bol eol))))
    (move-overlay ov bol eol)
    (overblock-bar-draw ov kind glyph label icons)
    ov))

(defun overblock--bar-wear (ov text)
  "Put TEXT on OV in place of the line, with room for the caret.
All of TEXT but its last character is the `before-string', where
\\(space :align-to \\(- right ...)) works, unlike in a display string.
The last character is the display itself and has `cursor', which
gives the caret a glyph: over an empty display, point on a boundary
line would not show.

Never the `after-string': it draws at the end of the overlay, where a
cloak can start and hide it.  The stretch aligns to the right edge, so
the split moves nothing."
  (if (string-empty-p text)
      (overlay-put ov 'display "")
    (overlay-put ov 'before-string (substring text 0 -1))
    (overlay-put ov 'display (propertize (substring text -1) 'cursor t)))
  (overlay-put ov 'after-string nil))

(defun overblock-bar-stale (ov)
  "Make OV forget what it was drawn from, so the next draw rebuilds it.
`overblock-bar-draw' leaves a bar as it is where nothing it compares
changed.  A change it cannot see is declared here, such as a new
width, from `overblock--width-changed', or a customized button list."
  (overlay-put ov 'overblock-bar-state nil))

(defun overblock-bar-kind (ov)
  "Return what OV was drawn as, or nil where OV is no bar of this layer.
Also nil for nil, which `overblock-bar-in' often returns."
  (and ov (overlay-get ov 'overblock-bar)))

(defun overblock-bar-in (beg end)
  "Return a bar overlay that covers any of BEG..END, or nil.
A region, not a position: text inserted at the start of a line moves
the start of its bar."
  (seq-find #'overblock-bar-kind (overlays-in beg end)))

(defun overblock-bars ()
  "Return the bar overlays of this buffer.
Without the narrowing, so a caller that redraws every bar reaches
all of them."
  (without-restriction
    (seq-filter #'overblock-bar-kind (overlays-in (point-min) (point-max)))))

(provide 'overblock)
;;; overblock.el ends here
