;;; overblock.el --- Text blocks over a buffer  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Marcel Arpogaus

;; Author: Marcel Arpogaus <znepry.necbtnhf@tznvy.pbz>
;; Assisted-by: Claude:claude-opus-5
;; Assisted-by: Claude:claude-fable-5
;; Version: 0.1.0
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
;;     (overblock-show BEG END :kind 'result :body TEXT :header TEXT)
;;
;; An anchor overlay covers the region and holds the state.  A second
;; overlay covers the newline that ends the region and carries what
;; shows after it: the header and the body, each on a row of its own, in
;; the slot that suits it.  Measured in a graphical frame: a
;; bar puts its icons at the window edge with `(space :align-to (- right
;; ...))', which a display property ignores and a string does not, and
;; an image in a display string is swallowed while one in a string
;; draws.  So the header is a string, and the body rides
;; the display property unless it holds an image.
;;
;; Text shown over the region hangs on its lines, a piece to a line.
;; Emacs lays a display string out whole on every redisplay, so one
;; string for a tall region costs its full height on every scroll event,
;; while a piece to a line costs only what the window shows.  Lines with
;; no piece left for them go under a cloak.
;;
;; See `overblock-show' for what a caller may pass.

;;; Code:

(require 'seq)
(require 'subr-x)
;; `overblock--flatten-alignment' reads a match with `prop-match-value'.
;; `text-property-search-forward' is autoloaded and its accessors are
;; not, so the file otherwise compiles clean only when something else
;; has pulled the library in first.
(require 'text-property-search)

(defgroup overblock nil
  "Blocks of text shown over a buffer."
  :group 'convenience
  :prefix "overblock-")

(defcustom overblock-image-height 0.8
  "How tall an image may be drawn inline, as a share of the window.
Zero draws it at whatever size it came in.  `pycell-pop-output' and
`pycell-save-image' always work from the original.

A block taller than the window cannot be scrolled past: the wheel
bounces backwards off it and starts over, and the buffer below it
stays out of reach.  Measured in a 437 pixel text area, 25 pixels a
step: a figure at 0.9 of the area bounced 40 times in 399 steps and
never got past, one at 0.8 went by in 94 steps without a single step
backwards.  The difference is the two lines of text a block carries
besides the figure.

The share is taken when the block is drawn, from the window showing
the buffer then, or from the selected window when the notebook is not
on screen; a window resized afterwards keeps the size the figure
had."
  :type 'number
  :group 'overblock)

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
The order is whatever `overlays-in' gives, so a caller that takes the
first is relying on one block of a kind to a region.  Without KIND every
kind answers."
  (seq-filter (lambda (ov)
                (and (overlay-get ov 'overblock)
                     (or (null kind) (eq kind (overblock-get ov :kind)))))
              (overlays-in beg end)))

(defun overblock-at (&optional kind)
  "Return the block of KIND at point, or nil.
Point counts as inside a block that begins or ends on it, so a block
whose region starts where point sits answers.  A caller that works from
a click moves point there first."
  (car (overblock-in (max (1- (point)) (point-min))
                     (min (1+ (point)) (point-max))
                     kind)))

(defun overblock--carriers (block)
  "Return the overlays that carry what BLOCK shows, the anchor apart.
One list for the two readers: `overblock-delete' takes them down and
`overblock-sweep-orphans' keeps them, so a slot named in one place and
not the other would have the sweep delete a live overlay."
  (delq nil (append (list (overblock-get block :newline))
                    (overblock-get block :parts)
                    (overblock-get block :attached))))

(defun overblock-delete (block)
  "Delete BLOCK and the overlays that carry what it shows."
  (mapc #'delete-overlay (overblock--carriers block))
  (delete-overlay block))

(defun overblock-clear (&optional beg end kind)
  "Delete the blocks of KIND that overlap BEG..END.
BEG and END default to the whole buffer, KIND to every kind.  A
narrowing hides nothing from this: the range is searched whole, so no
block is left behind outside it.

Clearing the whole buffer sweeps what lost its anchor as well; see
`overblock-sweep-orphans' for what that is and why it matters.  Only the
whole buffer: a range says nothing about which block an orphan belonged
to."
  ;; Asked before the widening, in the caller's own view of the buffer:
  ;; under a narrowing the bounds a caller passes are the narrowed ones,
  ;; and comparing them with the whole buffer's afterwards said no to a
  ;; caller who had asked for everything it could see.
  (let ((whole (and (null kind)
                    (<= (or beg (point-min)) (point-min))
                    (>= (or end (point-max)) (point-max)))))
    (without-restriction
      (mapc #'overblock-delete
            (overblock-in (or beg (point-min)) (or end (point-max)) kind))
      ;; Every kind, and only for the whole buffer: a sweep over a range
      ;; would take the parts of a block that reaches into it from
      ;; outside, and a sweep with a KIND cannot tell which kind an
      ;; orphan belonged to.
      (when whole (overblock-sweep-orphans)))))

(defun overblock-sweep-orphans ()
  "Remove the overlays of the layer that no live block owns.
A block is taken down through its anchor, which knows what it drew.
Where the anchor is gone and what it drew is not — a package that
deletes the overlays of a region, an anchor that evaporated with the
line it hung on — nothing knows those overlays any more, and one of
them can be a cloak that keeps lines of the buffer invisible.

Every overlay the layer makes says so, so what is left over can be
found.  A caller that cleared one kind of block reaches for this: an
orphan says nothing about the kind it belonged to, and a clear that
named a kind could not sweep it."
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

(defun overblock-show (beg end &rest props)
  "Show a block over the region BEG..END and return it.
Nil where BEG..END holds nothing to hang a block on: an anchor of no
length is deleted the moment it is made.
It replaces the blocks of its own kind in that region — every kind,
where no `:kind' is given.  PROPS is a plist, and every entry is
optional:

  :kind      a symbol that tells the blocks of one caller from another.
             Without it the block is anonymous: `overblock-in',
             `-at' and `-clear' reach it when they are asked for no
             kind, and no named kind answers for it.
  :data      anything the caller keeps with the block.  The layer stores
             it and never reads it, so its shape is the caller's own.
  :over      text shown instead of the lines of the region, a piece to
             a line; without it the region stays as it is.
  :body      text shown after the region, on the newline that ends it,
             or on the anchor where the region ends without one.
  :header    text shown above the body.
  :hidden    non-nil shows nothing at all, decorations included.
  :attached  overlays of the caller's own, deleted with the block.
  :keymap and :help-echo go on every overlay the block draws; an
             overlay of the caller's own under `:attached' keeps
             whatever the caller put on it.  A click lands on the
             string and is answered by whatever `overblock-fill-props'
             left there — shr's own keymap on a link, say — so the two
             live side by side: the string answers the mouse, the
             overlays answer point.

The caller renders the text and hands it over; a block never calls a
renderer itself.  Change a property with `overblock-set' and call
`overblock-refresh' to show the change.

The anchor ends before the newline of the region, so a window that
starts at the next line keeps the block out of view.  Both overlays
advance with text typed at their end.

A block also keeps the overlays that carry what it shows.  `:newline'
is readable, and a caller needs it to keep an outline fold off the
newline the block hangs on; `:parts' is the layer's own, made anew by
every `overblock-refresh'."
  (overblock-clear beg end (plist-get props :kind))
  (let* ((anchor-end (if (and (eq (without-restriction (char-before end))
                                  ?\n)
                              ;; a region that is only a newline keeps a
                              ;; non-empty anchor
                              (> (1- end) beg))
                         (1- end)
                       end))
         ;; Nothing to hang a block on answers nothing.  `evaporate'
         ;; deletes a zero-length overlay the moment it goes on, so the
         ;; caller was handed an anchor that was already dead and that
         ;; `overblock-in' and `overblock-clear' could never find
         ;; again: a markdown cell emptied by its reader left the bar
         ;; of a block that did not exist.
         (block (and (> anchor-end beg)
                     (make-overlay beg anchor-end nil t t))))
    (when block
      (overlay-put block 'evaporate t)
      (overlay-put block 'overblock-part t)
      ;; `modification-hooks' is left to the caller.  What an edit of the
      ;; region means is the caller's business — a stale result goes, a
      ;; rendering goes with its source — and a hook of the layer's own was
      ;; both unreachable and overwritten: every route that empties the
      ;; anchor takes it down through `evaporate' first, so it is never
      ;; live and empty at once, and each caller writes the property
      ;; wholesale.  What the layer carries is the mark above, so
      ;; `overblock-clear' can sweep an overlay whose anchor is gone.
      ;; The two slots the layer writes itself are there from the start, so
      ;; every `plist-put' after this mutates the list in place and no
      ;; reader can be left holding a head that is no longer the block's.
      (overlay-put block 'overblock (append props (list :newline nil
                                                        :parts nil)))
      (when (eq (without-restriction (char-after anchor-end)) ?\n)
        (let ((ov (make-overlay anchor-end (1+ anchor-end) nil t)))
          (overlay-put ov 'evaporate t)
          (overlay-put ov 'overblock-part t)
          (overblock-set block :newline ov)))
      (overblock-refresh block)
      block)))

(defun overblock--dress (block ov)
  "Give OV the keymap and the help echo of BLOCK, and return OV.
Everything a block shows answers to the same click and says the same
thing under the mouse, whichever overlay carries it.  A hidden block
shows nothing, and answers nothing: `:hidden' is documented to take the
decorations with it.

The overlays and not the string: a click resolves its keymap from the
string it landed on, but a key pressed at point does not — point never
enters a display string, and `get-char-property' at point reads the
buffer and the overlays alone.  Taking these off the overlays, so that a
rendering carrying keymaps of its own could answer for its links, left
RET in a rendered markdown cell running `newline': it split the source
line and took the rendering with it.  A link is followed by clicking
it, which works because the string answers the click."
  ;; Written either way: a block that no longer carries a keymap or a
  ;; help string has it taken off its overlays, where a `when' left the
  ;; old one on until the overlay was remade.
  (let ((hidden (overblock-get block :hidden)))
    (overlay-put ov 'keymap (unless hidden (overblock-get block :keymap)))
    (overlay-put ov 'help-echo (unless hidden
                                 (overblock-get block :help-echo))))
  ov)

(defun overblock--cloak (block beg end)
  "Return an overlay of BLOCK that hides BEG..END and stays hidden.
A cloak covers the lines that no piece was left for.  It has to start
at the end of a visible line: `scroll-down' answers a run that begins
a line with a beginning-of-buffer error, in the middle of the region."
  (let ((ov (make-overlay beg end nil t)))
    (overlay-put ov 'evaporate t)
    (overlay-put ov 'overblock-part t)
    (overlay-put ov 'invisible t)
    (overlay-put ov 'overblock-cloak t)
    (overblock--dress block ov)))

(defun overblock--lines (text)
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

(defun overblock--rows (beg end)
  "Return the lines of BEG..END as a list of (FROM . TO), in order.
TO is where the text of a line ends, so a line with nothing on it
answers a pair with nothing between.

The whole buffer, and a test on the move: an overlay's positions know
nothing of a narrowing, and under one that ends before END this walk
would never reach it — `forward-line' stops at the accessible end
without moving, the test never goes false, and the list grows until the
machine is out of memory.  A position past the end of the buffer — a
stale overlay, or a narrowing the widening cannot undo — used to spin
here for the same reason."
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
A piece with an image in it cannot ride a `display' property, because
display properties do not nest and the image would be swallowed.  Such
a piece hides its line with a display string of nothing and rides the
before-string instead, which draws images.  The line keeps its own row
either way, which is what makes a region scroll a line at a time.

The before-string and not the after-string: a cloak begins at the end
of the piece before it, and Emacs leaves out an overlay string whose
position is inside invisible text.  Measured on a frame, counting the
pixels of the image itself: 0 for an after-string with a cloak at the
piece's end, 32 for the same image on a before-string."
  (let ((ov (make-overlay from to nil t)))
    (overlay-put ov 'evaporate t)
    (overlay-put ov 'overblock-part t)
    (if (overblock-image-in text)
        (progn (overlay-put ov 'display "")
               (overlay-put ov 'before-string text))
      (overlay-put ov 'display text))
    (overblock--dress block ov)))

(defun overblock--deal (lines slots)
  "Return LINES dealt into SLOTS chunks, as evenly as the two counts allow.
Chunk I takes the lines from COUNT*I/SLOTS to COUNT*(I+1)/SLOTS, so a
remainder is spread over the chunks rather than heaped on the last.
The chunks follow one another, so they come off a walking list:
measured, `seq-subseq' from the front of a thousand lines cost 2.7
milliseconds against 0.3 for three hundred, which is the shape of a
quadratic."
  (let ((count (length lines))
        (rest lines)
        chunks)
    (dotimes (i slots)
      (let ((wanted (- (/ (* (1+ i) count) slots)
                       (/ (* i count) slots))))
        (push (take wanted rest) chunks)
        (setq rest (nthcdr wanted rest))))
    (nreverse chunks)))

(defun overblock--cloak-from (open from)
  "Return where the cloak covering the row at FROM starts.
OPEN is where a cloak already stands open, or nil.  An open one is left
to grow; a new one starts at the newline above FROM, which is the end
of the visible line before it — `scroll-down' answers a run that
begins a line with a beginning-of-buffer error, in the middle of the
region.

Nil for a row that begins the buffer: it has no newline above it, and
an overlay opened at position zero comes back dead or clamped to a line
start, which is that same error.  Such a row keeps its text."
  (cond (open open)
        ((> from (point-min)) (1- from))))

(defun overblock--pieces (block text)
  "Hang TEXT over the lines of BLOCK, a piece to a line.
Return the overlays that carry the pieces and the cloaks.

A rendering rarely has as many lines as the region.  Where it has more,
one line carries several of them, dealt out as evenly as the two counts
allow; where it has fewer, the lines left over go under a cloak.

A piece covers the text of its line and leaves the newline alone, so
the buffer keeps its line structure and every line keeps its height;
`overblock--piece' makes one.

A line without text cannot carry a piece — there is nothing to put the
display property on — and a rendering rarely fills as many lines as the
region has anyway.  Those lines go under a cloak."
  (let* ((beg (overlay-start block))
         ;; The last newline of the region belongs to it: the anchor
         ;; stops before that newline, and a cloak that stopped there
         ;; too would leave the last line of the region on the screen.
         ;;
         ;; A live overlay, tested by its buffer: `delete-overlay' leaves
         ;; an overlay that is still an overlay and answers nil to
         ;; `overlay-end'.  Deleting the region's last newline kills that
         ;; overlay without touching the anchor, whose range does not
         ;; cover it, so the slot can hold a corpse while the block is
         ;; otherwise sound — and END of nil ends the walk below in
         ;; `wrong-type-argument'.
         (nl (overblock-get block :newline))
         (end (if (and (overlayp nl) (overlay-buffer nl))
                  (overlay-end nl)
                (overlay-end block)))
         (lines (overblock--lines
                 (string-trim text "\\(?:[ \t]*\n\\)+"
                              "\\(?:\n[ \t]*\\)+")))
         (rows (overblock--rows beg end))
         (slots (max 1 (seq-count (lambda (row) (> (cdr row) (car row))) rows)))
         (chunks (overblock--deal lines slots))
         parts cloak-from)
    (dolist (row rows)
      (let* ((from (car row))
             (to (cdr row))
             ;; A line with text to cover takes the next chunk of the
             ;; rendering; one without carries nothing.
             (chunk (and (> to from) (pop chunks))))
        (if (null chunk)
            (setq cloak-from (overblock--cloak-from cloak-from from))
          (when cloak-from
            (push (overblock--cloak block cloak-from (1- from)) parts)
            (setq cloak-from nil))
          (push (overblock--piece block from to (string-join chunk "\n"))
                parts))))
    (when cloak-from
      (push (overblock--cloak block cloak-from (1- end)) parts))
    (nreverse parts)))

(defun overblock--attach (block shown)
  "Show the header and the body of SHOWN after BLOCK.
SHOWN is the property list of what the block shows, or nil for a block
that shows nothing.
Each stands on a row of its own, in the slot that suits it.

A row that must be a string rides the after-string of the anchor: a bar
draws its icons at the window edge with `(space :align-to (- right
...))', which a display property ignores, and an image in a display
string is swallowed while one in a string draws.

A plain body rides the display property of the newline that ends the
region, which is the cheapest place for text.

The newline is never hidden.  Measured: with the rows of a figure on the
after-string of that newline and the newline itself replaced by an empty
display string, `pixel-scroll-precision-scroll-up' refused to pass the
block — 280 refusals with a beginning-of-buffer error over six figures,
where the same buffer with the newline left alone scrolls to the top
without one.

Each string carries the line breaks that its own rows need."
  (let* ((header (plist-get shown :header))
         (body (plist-get shown :body))
         (newline (overblock-get block :newline))
         (on-display (and body
                          (not (overblock-image-in body))
                          ;; A live overlay, tested by its buffer: without
                          ;; a newline there is nothing to hang a display
                          ;; property on, so the body joins the rows on
                          ;; the anchor.  A deleted overlay is still an
                          ;; overlay, and testing the slot alone took the
                          ;; body off the anchor and then skipped the
                          ;; write below — the body showed nowhere at all.
                          (overlayp newline)
                          (overlay-buffer newline)))
         ;; the rows that ride the anchor, in the order they show
         (strings (delq nil (list header (unless on-display body))))
         ;; A row needs a break before it unless the newline it hangs on
         ;; already begins a line: a cell that ends in a blank line gives
         ;; that line to the header.
         ;; The whole buffer: an overlay's positions know nothing of a
         ;; narrowing, and `char-before' past the accessible portion
         ;; answers nil — which added a blank row above the header on
         ;; every refresh under a narrowing.
         (lead (if (eq (without-restriction
                         (char-before (overlay-end block)))
                       ?\n)
                   ""
                 "\n")))
    (overlay-put block 'after-string
                 (when strings
                   (concat lead (string-join strings "\n"))))
    (when (and newline (overlay-buffer newline))
      (overlay-put newline 'display
                   (when on-display
                     (concat (if header "\n" lead) body "\n")))
      (overblock--dress block newline))))

(defun overblock-refresh (block)
  "Show BLOCK again from its properties.
Call it after `overblock-set'.  Everything the block shows is made
anew, so nothing has to be saved and given back.

A block that is no longer in a buffer draws nothing: `delete-overlay'
leaves an overlay that answers nil to `overlay-start', and the drawing
reads that position.  What is drawn is drawn in the block's own buffer,
because the overlays the pieces ride are made with `make-overlay',
which puts them in whatever buffer is current — a refresh from
elsewhere would hang them over unrelated text, out of reach of
`overblock-clear' in the buffer they belong to."
  (when-let* ((buffer (overlay-buffer block)))
    (with-current-buffer buffer
      (mapc #'delete-overlay (overblock-get block :parts))
      (overblock-set block :parts nil)
      ;; What the drawing below makes is owned as it is made: a `quit' in
      ;; the middle used to leave the cloaks it had already created
      ;; invisible and unowned, and no later refresh would remove them
      ;; either, since the slot said there were none.
      ;; A hidden block shows nothing, so it reads nothing.
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
row for each line, and a slice reads ((slice X Y W H) IMAGE) rather
than (image . PLIST): the image is its second element.  The form is older
than Emacs 22, so reading it costs nothing on an Emacs that does not
slice."
  (cond ((eq (car-safe display) 'image) display)
        ((and (eq (car-safe (car-safe display)) 'slice)
              (eq (car-safe (cadr display)) 'image))
         (cadr display))))

(defun overblock-image-in (text)
  "Return the `display' spec of the first image in TEXT, or nil.
The value is (image . PLIST), so a caller can read `:data' or `:type'
from it.  A `raise' spec, which shr uses for a superscript, is not an
image and does not answer here.  A slice of an image is an image, and
the image inside it is what answers."
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
        (if-let* ((image (overblock--image-spec disp)))
            (setq img image)
          (setq pos (or (next-single-property-change pos 'display text) len)))))
    img))

(defun overblock-image-label (text &optional label)
  "Return TEXT with every image in it replaced by LABEL.
For a display that draws none: an image rides a character, and that
character is a space — a figure came out as a blank row, and a reader
had nothing to tell an empty result from a picture.  LABEL defaults to
\"[figure]\"."
  (let ((label (or label "[figure]"))
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
An image that already carries a `:max-height' was capped by whoever
made it, and to a height they chose."
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
row for each line of the window it was rendered in, and a slice reads
as ((slice X Y W H) IMAGE).  Slicing does not make an image smaller — it
cuts a full-height image into rows — so a figure sliced for a tall shell
window and shown in a shorter notebook window is exactly the block the
wheel cannot get past, which is what the cap exists to prevent.  Nor can
the image be capped under the slice: the fractions were worked out
against the height it had, and shrinking it beneath them draws bands
with gaps.  So a run of slices of one image becomes the whole image,
capped, on its first row, and nothing on the rows that followed it —
which is what the layer does with a rendering anyway: it deals the rows
out over the lines it has."
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
          ;; A later row of a run of slices: the whole image is on the
          ;; first of them now.
          (put-text-property beg end 'display "" string)
        (when sliced (push image seen))
        (when-let* ((capped (overblock--image-capped image limit)))
          (put-text-property beg end 'display capped string)))))
  string)

(defun overblock-image-limit ()
  "Return how many pixels tall an image may be drawn, or nil for no cap.
The share is `overblock-image-height' of the window that shows the
notebook.  A cell can finish while its notebook is elsewhere — sent and
switched away from, or one of a whole run — and no window at all would
mean no cap and a block the wheel cannot get past.  The selected window
is a guess at the size the notebook will have, and a guess that comes
out small only draws a smaller figure."
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
number; a terminal's pixel is a column, a graphic frame's is
`frame-char-width'."
  (let* ((plist (cdr spec))
         (to (plist-get plist :align-to))
         (width (plist-get plist :width))
         ;; A number, or a list whose first element is one: a spec can
         ;; also read `(- right (N))', which is the shape
         ;; `overblock-bar' writes, and `round' on the symbol raised —
         ;; inside a process filter, where an error takes the rest of the
         ;; filters with it and leaves the shell busy for good.  Such a
         ;; spec is left as it is: it aligns to the window, and there is
         ;; no column to answer for it here.
         ;;
         ;; And no more columns than a buffer can want: a spec asking to
         ;; align at two million pixels would otherwise build a string of
         ;; two hundred and fifty thousand spaces.
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

(defun overblock--flatten-alignment ()
  "Turn the space stretches of this buffer into real spaces.
shr aligns table columns with `(space :align-to (N))' display specs,
and vtable, which is how comint-mime shows a DataFrame, with
`(space :width (N))'.  Both count from the window they were measured
in.  A rendered cell and a result block are shown indented — line
numbers, margins — so the stretches land elsewhere there and the
columns of a row drift apart.  Literal padding aligns anywhere.  Left
to right, so `current-column' already sees the padding put in before
it."
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
              ;; Zero is a zero-width stretch: the column is already
              ;; there, and a forced space would push this row one past
              ;; its sisters.
              (insert (make-string pad ?\s)))))))))

(defun overblock-flattened (text)
  "Return TEXT with its space stretches as real spaces.
See `overblock--flatten-alignment' for why a copy needs them literal."
  (with-temp-buffer
    (insert text)
    (overblock--flatten-alignment)
    (buffer-string)))

;;;; Bars, buttons and glyphs

;; What a caller puts on the header of a block.
;; A bar is a line with text at the left and icons at the right
;; window edge; a button is a label that answers a click; a glyph
;; is a character this frame can actually draw.

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
                  (const :tag "While the cell runs" running))))
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
would clobber both, and the link would then run this block's commands
instead of following the URL.

A walk of the string for every property: measured, one walk over a
rendered cell of three hundred lines costs 3.4 milliseconds.  What the
plist saves is the call around each walk, not the walks."
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
  "What `overblock-glyph' answered, by frame font and candidates.
The answer cannot change while a frame keeps its font, and the question
is dear: `internal-char-font' asks the font backend once a character,
and one header of six icons asked it twenty times, five times a second.
Measured over a running cell, the header cost 0.71 milliseconds a tick
and 0.27 with this table.")

(defun overblock-glyph (&rest candidates)
  "Return the first of CANDIDATES this frame has a glyph for.
The last candidate is the answer when none of them has one.
`char-displayable-p' answers for the character set and not for the
font, so it says yes to characters that then draw as a hex box.

Every character of a candidate has to be there, not just the first:
several of them lead with a space, and a space is always available."
  (with-memoization (gethash (cons (and (display-graphic-p)
                                        (frame-parameter nil 'font))
                                   candidates)
                             overblock--glyphs)
    (or (and (display-graphic-p)
             (seq-find (lambda (c)
                         (seq-every-p (lambda (ch)
                                        (internal-char-font nil ch))
                                      c))
                       candidates))
        (car (last candidates)))))

;; The press answers, not the release.  A block is in the text area,
;; where a press reaches `mouse-drag-region', which follows the mouse
;; and keeps the release to itself.  Measured with real clicks at the
;; centre of every button of a cell bar, a keymap that bound the release
;; alone ran `mouse-set-point' and nothing else — while `key-binding' at
;; those same pixels answered with the command, which is how this
;; survived every test that asked the keymap instead of clicking.  A
;; header line has no drag to lose, which is why the buttons of one
;; always worked.
;;
;; The release goes to `ignore' so that it neither sets point nor reaches
;; whatever else would take it, and so does the drag: a command that
;; moves the text under the pointer — the move buttons do — turns the
;; release into a drag, and that drag left a region behind.
(defun overblock-button (label help command)
  "Return LABEL as a button.
A left click calls COMMAND, and HELP becomes the tooltip."
  (propertize label 'mouse-face 'highlight 'help-echo help
              'keymap (define-keymap
                        "<down-mouse-1>" command
                        "<mouse-1>" #'ignore
                        "<drag-mouse-1>" #'ignore)))

(defun overblock-buttons (descriptors &optional imagep lines runningp)
  "Return the icon group that DESCRIPTORS ask for.
Each descriptor is (KEY GLYPHS HELP COMMAND WHEN), the shape
`overblock-button-type' asks customize for.  IMAGEP says the block
holds an image, LINES how many lines it has and RUNNINGP that it is
still being written; a descriptor whose WHEN is `image', `lines' or
`running' waits for those."
  (concat
   (string-join
    (seq-keep
     (lambda (descriptor)
       (pcase-let ((`(,_key ,glyphs ,help ,command ,when) descriptor))
         (when (pcase when
                 ('image imagep)
                 ('lines (> (or lines 0) 0))
                 ('running runningp)
                 (_ t))
           ;; The space after the glyph belongs to the button, so the
           ;; place a reader can press is two columns wide rather than
           ;; one: measured in a window of 1554 pixels, the boxes were
           ;; ten pixels wide with twenty pixels of nothing between them.
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
Emacs 31 takes the buffer whose face remapping to measure with; an
older one measures without any, and the icons of a notebook under
`text-scale-mode' or `buffer-face-mode' are then placed by a width
they are not drawn at."
  ;; Through `apply' with a computed list, so an Emacs whose
  ;; `string-pixel-width' takes one argument does not reject the
  ;; two-argument call while compiling this file.  The arity is read
  ;; once: asked on every call it is both a cost on the ticker and a
  ;; question about whatever has advised the function since.
  (apply #'string-pixel-width string
         (when overblock--pixel-width-takes-a-buffer
           (list (current-buffer)))))

(defun overblock-window-width ()
  "Return the pixel width of the narrowest window that shows this buffer.
`window-max-chars-per-line' counts the line-number area and the
margins, as `window-body-width' does not, and it is taken in the
window's own font.  The narrowest, because one string is drawn in all
of them at once and a label cut to fit a wide window still wrapped in a
narrow one beside it; the wide one then loses a few characters of a
label it could have shown whole.

`visible' and not t: an invisible or iconified frame counts under t,
and a notebook shown in the root window of a hidden 20 column frame
had its header cut to that.

Nil where no visible window shows the buffer.  A bar is then not cut at
all — there is nothing to wrap in — and a caller that caches the width
has nothing to compare."
  (when-let* ((windows (get-buffer-window-list nil nil 'visible)))
    ;; `save-excursion': both of these select the window they measure,
    ;; which sets this buffer's point to that window's point, and
    ;; nothing puts it back when the buffer is not the selected
    ;; window's.  A caller that walks the buffer with point — the walk
    ;; that draws the bars does — was sent back to a line it had passed
    ;; and drew for ever: measured at 99.5% of a core and 91 GB of
    ;; memory in a notebook edited while the reader looked elsewhere.
    (save-excursion
      ;; Never below zero.  `window-max-chars-per-line' answers with a
      ;; negative count where the font is far larger than the window —
      ;; measured, a 32 column frame under `text-scale-set' 10 said -22,
      ;; and a width of -1430 pixels put the bar through the branch for
      ;; a window with no room at all and then wrapped it.
      (max 0 (apply #'min (mapcar (lambda (window)
                                    (* (window-max-chars-per-line window)
                                       (window-font-width window)))
                                  windows))))))

(defun overblock--cut (text face room)
  "Return TEXT cut with an ellipsis to ROOM pixels, drawn in FACE.
Pixels and not columns: a header label begins with an icon glyph, and
one the frame draws from a fallback font is wider than a character cell
— measured 19 pixels against a cell of 8.  A budget counted in columns
was then 11 pixels short at every width, and the bar took two rows.

The columns are the first guess, which is close, and then a character
comes off at a time."
  (let ((cut (truncate-string-to-width
              text (max 1 (/ room (frame-char-width))) nil nil t)))
    (while (and (> (length cut) 1)
                (> (overblock--pixel-width (propertize cut 'face face)) room))
      (setq cut (concat (substring cut 0 -2) "…")))
    cut))

(defun overblock-bar (left icons face)
  "Return a header line: LEFT text, ICONS at the right window edge, in FACE.
The alignment is pixel-exact: icon glyphs render wider than
`string-width' counts, and (N) in the display spec means N pixels.  A
terminal gets two columns of slack there: a bar that runs into the last
column makes the line a continuation, and the final icon wraps onto a
line of its own.

LEFT is cut where the icons leave no room for it, in pixels, because
that is how it is drawn.  The stretch between the two collapses to
nothing once the label has passed its target, so a label of 48 columns
in a window of 42 ran into the first icon and put the last two on a row
of their own.

The room is what `overblock-window-width' measures, less the icons and
two columns of slack: one keeps the icons off the right edge, and one
keeps the label off the icons.

A buffer in no visible window is not cut at all.  There is nothing to wrap
in, and the cut is baked into the string: measured, a long cell running
while the reader looked at another buffer had its header cut to the
width of that buffer's window — down to \" ▾…\" — and the cut stayed
there when the notebook came back into a window of 160 columns, because
nothing rebuilds the header after the cell has ended."
  (let* (;; A column of slack, in a graphic frame as well as in a
         ;; terminal.  Without it the icons end at the right edge
         ;; exactly, and whether such a row wraps is decided by
         ;; redisplay: measured in one window at one width, the same bar
         ;; drew all its icons when the notebook was opened and dropped
         ;; the last one onto a row of its own after the first command —
         ;; same string, same spec, same window.
         ;; A terminal keeps three columns: two for the reason below,
         ;; and one more for the ellipsis an outline fold hangs after
         ;; the line — measured with `truncate-lines' off, which is
         ;; Emacs's own default, every folded bar took two rows.  A
         ;; graphic frame needs no third: the same fold there stays on
         ;; one row.
         (slack (if (display-graphic-p) (frame-char-width) 3))
         (width (+ (overblock--pixel-width (propertize icons 'face face))
                   slack))
         (available (overblock-window-width))
         ;; A column of slack over and above the stretch's: a label cut
         ;; to the room exactly still put the last icon on a row of its
         ;; own.
         (room (and available (- available width (frame-char-width)))))
    ;; Not even the icons fit.  They go: such a window can show a bar or
    ;; a wrapped bar, and a wrapped bar is two rows of almost nothing.
    ;; Measured at 16 columns, two rows with the icons and one without.
    (when (and room (<= room 0))
      (setq icons "" width slack left "…")
      ;; And where the window is one character wide, the ellipsis is the
      ;; whole bar: the stretch that would follow it costs a second
      ;; character, which is a second row.
      (when (< available (* 2 (frame-char-width)))
        (setq width 0 slack 0)))
    (setq left (cond
                ;; No window to wrap in, so nothing to cut for.
                ((null room) left)
                ((> (overblock--pixel-width (propertize left 'face face))
                    room)
                 (overblock--cut left face room))
                (t left)))
    (overblock-faced
     (concat left
             (propertize " " 'display
                         `(space :align-to (- right (,width))))
             icons)
     face)))

(defun overblock-bar-over (beg end)
  "Return an overlay that shows a bar in place of the text BEG..END.
The text stays in the buffer and draws as nothing until
`overblock-bar-draw' puts the bar `overblock-bar' built on this
overlay, and puts it there again whenever the label or the window width
changes.

Most of the bar rides an overlay string rather than a display property
on the text: a display string ignores (space :align-to (- right ...)),
and the icons then sit beside the label instead of at the window edge."
  (let ((ov (make-overlay beg end nil t)))
    (overlay-put ov 'evaporate t)
    (overlay-put ov 'display "")
    ;; Marked as a bar of this layer from the start, with `t' — no
    ;; caller's kind, so `overblock-bar-kind' answers and a caller
    ;; asking for its own kind does not.  A `C-g' between this and
    ;; `overblock-bar-draw' used to leave an overlay that drew its line
    ;; as nothing and that no registry could see: not
    ;; `overblock-bars', not `overblock-sweep-orphans', not a mode's
    ;; own teardown.
    (overlay-put ov 'overblock-bar t)
    ov))

(defun overblock-bar-draw (ov kind label icons face)
  "Draw the bar LABEL and ICONS on OV, of KIND, in FACE.
OV comes from `overblock-bar-over'.  KIND is the caller's own word for
what this bar stands on, and `overblock-bar-kind' answers with it.

Where nothing has changed the bar is left as it is: a caller draws from
a change hook, and a walk over a long buffer would otherwise measure and
build every bar it passes.  The text of the line is part of what is
compared, because the label is usually written on it."
  (let ((state (list (buffer-substring-no-properties (overlay-start ov)
                                                     (overlay-end ov))
                     label icons face (overblock-window-width))))
    (overlay-put ov 'overblock-bar kind)
    (unless (equal state (overlay-get ov 'overblock-bar-state))
      (overlay-put ov 'overblock-bar-state state)
      (overlay-put ov 'overblock-bar-text (overblock-bar label icons face))
      (overblock--bar-wear ov (overlay-get ov 'overblock-bar-text)))))

(defun overblock--bar-wear (ov text)
  "Put TEXT on OV in place of the line, with room for the caret.
All of TEXT but its last character rides the `before-string', whose
own \\(space :align-to \\(- right ...)) is looked at — a display string's
is not, and the icons would sit beside the label instead of at the
window edge.  The last character is the display itself and carries
`cursor', which is what gives the caret a glyph to draw on: over an
empty display there is none, so point could stand on a boundary line
with nothing to show it — a line that could not be folded from.

The last character and not the first, and never the `after-string': a
string after the overlay draws at the overlay's end, and a rendering
whose cloak covers that place swallows it.  Measured on a markdown cell
at the top of a buffer — where the cloak begins at the boundary line's
own newline — the bar drew as one glyph and nothing else.

The stretch aligns to the window's right edge, so taking the last
character out of the string moves nothing: it draws where it drew."
  (if (string-empty-p text)
      (overlay-put ov 'display "")
    (overlay-put ov 'before-string (substring text 0 -1))
    (overlay-put ov 'display (propertize (substring text -1) 'cursor t)))
  (overlay-put ov 'after-string nil))

(defun overblock-bar-stale (ov)
  "Make OV forget what it was drawn from, so the next draw rebuilds it.
`overblock-bar-draw' leaves a bar as it is where nothing it compares
has changed.  A change it cannot see is the caller's to declare: the
buffer's font size is one, because the room a label has is the same
number of pixels at any size and the label's own width is not."
  (overlay-put ov 'overblock-bar-state nil))

(defun overblock-bar-kind (ov)
  "Return what OV was drawn as, or nil where OV is no bar of this layer.
Nil for nil as well: this answers a question about whatever a caller
found, and `overblock-bar-at' finds nothing often."
  (and ov (overlay-get ov 'overblock-bar)))

(defun overblock-bar-in (beg end)
  "Return a bar overlay that covers any of BEG..END, or nil.
A region and not a position: a bar advances off the start of its line
when text is inserted there, so a bar of the line BEG begins is not
always a bar at BEG."
  (seq-find #'overblock-bar-kind (overlays-in beg end)))

(defun overblock-bars ()
  "Return the bar overlays of this buffer.
The whole of it: an overlay knows nothing of a narrowing, and a caller
that draws every bar again — after a change of width, say — would
otherwise leave the ones outside the accessible part as they were."
  (without-restriction
    (seq-filter #'overblock-bar-kind (overlays-in (point-min) (point-max)))))

(provide 'overblock)
;;; overblock.el ends here
