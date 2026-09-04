;;; overblock-test.el --- Tests for overblock -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Marcel Arpogaus

;; Author: Marcel Arpogaus <znepry.necbtnhf@tznvy.pbz>
;; Assisted-by: Claude:claude-opus-5
;; Assisted-by: Claude:claude-fable-5
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

;; Run with: make test
;;
;; The layer that puts a block of text over a region: what it shows,
;; where each row rides, and the pieces a rendering hangs on.

;;; Code:

(require 'ert)
(require 'overblock)

(defconst overblock-test--image
  (propertize " " 'display '(image :type png :data "x"))
  "A stand-in for what comint-mime inserts for an image.")

(defun overblock-test--pieces (beg end text)
  "Return the pieces a block hangs TEXT on over the region BEG..END."
  (overblock-get (overblock-show beg end :over text) :parts))

(ert-deftest overblock-test-image-in-finds-the-first-one ()
  "The first image of a result is found, and plain text has none."
  (should (eq (car-safe (overblock-image-in (concat "a\n" overblock-test--image))) 'image))
  (should-not (overblock-image-in "just text")))

(ert-deftest overblock-test-glyph-falls-back-to-the-last-candidate ()
  "A candidate without a glyph is skipped, and the last one always answers."
  ;; A batch session has no graphical frame, so the fallback decides.
  (should (equal (overblock-glyph "⤓" "↧" "↓") "↓"))
  (should (equal (overblock-glyph "x") "x")))

(ert-deftest overblock-test-a-trusted-terminal-gets-the-icons ()
  "A terminal draws the best candidate where the reader says it can.
Emacs cannot ask a terminal what its font holds, so the icons are kept
from it until `overblock-terminal-glyphs' says otherwise.  Then the
coding system decides, which is the one thing a terminal can be asked."
  (let ((overblock-terminal-glyphs nil))
    (overblock-forget-glyphs)
    (should (equal (overblock-glyph "\uEBCC" "◫" "copy") "copy")))
  (let ((overblock-terminal-glyphs t))
    (overblock-forget-glyphs)
    (should (equal (overblock-glyph "\uEBCC" "◫" "copy") "\uEBCC"))
    ;; and what this terminal cannot encode it still does not get
    (cl-letf (((symbol-function 'char-displayable-p)
               (lambda (ch) (not (eq ch ?\uEBCC)))))
      (overblock-forget-glyphs)
      (should (equal (overblock-glyph "\uEBCC" "◫" "copy") "◫"))))
  (overblock-forget-glyphs))

(ert-deftest overblock-test-the-glyph-answer-is-forgotten-on-a-change ()
  "An answer kept from before the option changed is not reused.
The answers are memoized per display, font and option, and the option
is what a reader turns on once the bars are already drawn."
  (let ((overblock-terminal-glyphs nil))
    (should (equal (overblock-glyph "\uEBCC" "◫" "copy") "copy")))
  (let ((overblock-terminal-glyphs t))
    (should (equal (overblock-glyph "\uEBCC" "◫" "copy") "\uEBCC"))))

(ert-deftest overblock-test-glyph-weighs-every-character ()
  "A leading space must not answer for the glyph behind it.
Several candidates lead with one, and a space is always there, so
asking the first character alone accepted every candidate."
  (cl-letf (((symbol-function 'display-graphic-p) (lambda (&rest _) t))
            ;; a frame with the space and two of the three arrows
            ((symbol-function 'internal-char-font)
             (lambda (_frame ch) (memq ch '(?\s ?▶ ?>)))))
    (should (equal (overblock-glyph " ▸" " ▶" " >") " ▶"))
    (should (equal (overblock-glyph " ▸" " ▴" " >") " >"))))

(ert-deftest overblock-test-faced-gives-a-string-a-base-face ()
  "A block string carries a base face, so it inherits none."
  (let ((s (overblock-faced (copy-sequence "text") 'shadow)))
    (should (memq 'shadow (ensure-list (get-text-property 0 'face s))))))

(ert-deftest overblock-test-slots ()
  "Each row of a block lands in the slot that suits it.
The header is a string, where a bar can put its icons at
the window edge; a plain body rides the display property, the cheapest
slot; a body with an image rides a string, because a display property
swallows an image."
  (with-temp-buffer
    (insert "one\ntwo\n")
    (let* ((block (overblock-show 1 (point-max)
                                  :header "H" :body "B"))
           (nl (overblock-get block :newline)))
      (should (equal (overlay-get block 'after-string) "\nH"))
      (should (equal (overlay-get nl 'display) "\nB\n"))
      ;; a body with an image moves off the display property and joins
      ;; the header on the anchor, where an image draws; the newline keeps
      ;; its own character, which is what lets a wheel pass the block
      (overblock-set block :body (concat "B" overblock-test--image))
      (overblock-refresh block)
      (should-not (overlay-get nl 'display))
      (should (overblock-image-in (overlay-get block 'after-string))))))

(ert-deftest overblock-test-body-without-a-newline ()
  "A body shows even where the region ends without a newline.
The cheap slot is the display property of that newline, and a region at
the end of a buffer may have none: the body then joins the rows on the
anchor rather than going missing, which is what it did."
  (with-temp-buffer
    (insert "one\ntwo")
    (let ((block (overblock-show 1 (point-max) :header "H" :body "B")))
      (should-not (overblock-get block :newline))
      (should (string-match-p "B" (overlay-get block 'after-string)))
      (should (string-match-p "H" (overlay-get block 'after-string))))))

(ert-deftest overblock-test-lead ()
  "The first row takes a line of its own, and no more than it needs.
A region that ends in a blank line has a line to give away; one that
ends in text has not, and the row starts with a break."
  (with-temp-buffer
    (insert "code\n\n")
    (let ((block (overblock-show 1 (point-max) :header "H")))
      (should (equal (overlay-get block 'after-string) "H"))))
  (with-temp-buffer
    (insert "code\n")
    (let ((block (overblock-show 1 (point-max) :header "H")))
      (should (equal (overlay-get block 'after-string) "\nH")))))

(ert-deftest overblock-test-hidden-and-back ()
  "A hidden block shows nothing, and a refresh makes it all again."
  (with-temp-buffer
    (insert "one\ntwo\n")
    (let ((block (overblock-show 1 (point-max)
                                 :over "shown" :header "H")))
      (should (overblock-get block :parts))
      (overblock-set block :hidden t)
      (overblock-refresh block)
      (should-not (overblock-get block :parts))
      (should-not (overlay-get block 'after-string))
      (overblock-set block :hidden nil)
      (overblock-refresh block)
      (should (overblock-get block :parts))
      (should (equal (overlay-get block 'after-string) "\nH")))))

(ert-deftest overblock-test-kinds-keep-apart ()
  "A block replaces the blocks of its own kind, and leaves the others."
  (with-temp-buffer
    (insert "one\ntwo\n")
    (let ((first (overblock-show 1 (point-max) :kind 'a :header "A"))
          (other (overblock-show 1 (point-max) :kind 'b :header "B")))
      (should (overlay-buffer first))
      (should (equal (list first) (overblock-in 1 (point-max) 'a)))
      (should (equal (list other) (overblock-in 1 (point-max) 'b)))
      (let ((again (overblock-show 1 (point-max) :kind 'a :header "A2")))
        (should-not (overlay-buffer first))
        (should (overlay-buffer other))
        (should (equal (list again) (overblock-in 1 (point-max) 'a)))))))

(ert-deftest overblock-test-delete-takes-its-overlays ()
  "Deleting a block deletes what carries it, the caller's own included."
  (with-temp-buffer
    (insert "one\ntwo\n")
    (let* ((mine (make-overlay 1 2))
           (block (overblock-show 1 (point-max)
                                  :over "shown" :attached (list mine)))
           (parts (overblock-get block :parts))
           (nl (overblock-get block :newline)))
      (should parts)
      (overblock-delete block)
      (should-not (overlay-buffer block))
      (should-not (overlay-buffer nl))
      (should-not (overlay-buffer mine))
      (should-not (seq-some #'overlay-buffer parts)))))

(ert-deftest overblock-test-covers-its-last-line ()
  "The pieces of a block reach the last line of its region.
The anchor stops before the newline that ends the region, and a cloak
that stopped there with it would leave the last line on the screen."
  (with-temp-buffer
    ;; the last line is blank, so no row is left for it
    (insert "one\ntwo\n\n")
    (let* ((block (overblock-show 1 (point-max) :over "row one\nrow two"))
           (cloaks (seq-filter (lambda (ov) (overlay-get ov 'overblock-cloak))
                               (overblock-get block :parts))))
      (should cloaks)
      ;; up to the newline that ends the region, and not one line short
      (should (= (apply #'max (mapcar #'overlay-end cloaks))
                 (1- (point-max)))))))

(ert-deftest overblock-test-pieces-lose-no-line ()
  "The pieces together show the rendering, whole and in order.
A cell has as many lines as its author wrote and the rendering has as
many as it needs, so the two rarely match either way."
  (let ((shown (lambda (parts)
                 (mapconcat (lambda (p) (overlay-get p 'display))
                            (seq-remove (lambda (p) (overlay-get p 'overblock-cloak))
                                        parts)
                            "\n"))))
    ;; more rendering than lines to put it on
    (with-temp-buffer
      (insert "aaa\nbbb\n")
      (let ((text "one\ntwo\nthree\nfour\nfive"))
        (should (equal (funcall shown (overblock-test--pieces (point-min) (point-max) text))
                       text))))
    ;; more lines than rendering
    (with-temp-buffer
      (insert "aaa\nbbb\nccc\nddd\neee\n")
      (let* ((text "one\ntwo")
             (parts (overblock-test--pieces (point-min) (point-max) text)))
        (should (equal (funcall shown parts) text))
        (should (seq-some (lambda (p) (overlay-get p 'overblock-cloak)) parts))))))

(ert-deftest overblock-test-the-first-row-shows-the-first-line ()
  "The first line of a rendering stands on the first row of the region.
It is the only row that begins where the block does — every row after
it begins at a line start — and a rendering whose first line is
written for that column has nowhere else to go.  Dealt out with the
remainder rounded down, a rendering of fewer lines than the region has
rows left the first row empty and under a cloak: the bar of a rendered
doc string then hung at column 0, as many columns left of its own
prose as the doc string was indented.  Measured, and reported."
  (with-temp-buffer
    (insert "    aaa
    bbb
    ccc
    ddd
    eee
")
    (let* ((beg (+ (point-min) 4))
           (parts (overblock-test--pieces beg (point-max) "one\ntwo"))
           (first (car parts)))
      (should-not (overlay-get first 'overblock-cloak))
      (should (= (overlay-start first) beg))
      (should (equal (overlay-get first 'display) "one")))))

(ert-deftest overblock-test-fill-props-leaves-what-is-there ()
  "Properties are filled in only where the string carries none.
The rendered markdown keeps the keymap that shr gave its links."
  (let ((s (concat "plain" (propertize "link" 'keymap 'shr-map))))
    (overblock-fill-props s 'keymap 'block-map)
    (should (eq (get-text-property 0 'keymap s) 'block-map))
    (should (eq (get-text-property 6 'keymap s) 'shr-map))))

(ert-deftest overblock-test-the-overlays-answer-for-point ()
  "Every overlay a block draws carries its keymap and its help echo.
A click resolves its keymap from the string it landed on, and a
rendering may put shr's own map on a link there.  Point is the other
half: it never enters a display string, so a key pressed in a block is
answered by the overlays alone.  Taking these off them left RET in a
rendered cell running `newline', which split the source line and took
the rendering with it."
  (with-temp-buffer
    (insert "one\ntwo\n")
    (let ((block (overblock-show (point-min) (point-max)
                                 :over "plain text"
                                 :keymap 'block-map
                                 :help-echo "the block")))
      (should (eq (overlay-get block 'keymap) 'block-map))
      (should (equal (overlay-get block 'help-echo) "the block"))
      (should (seq-every-p (lambda (ov) (eq (overlay-get ov 'keymap)
                                            'block-map))
                           (overblock-get block :parts))))))

(ert-deftest overblock-test-bar-slack-on-a-terminal ()
  "The stretch ends three columns short of the right edge on a terminal.
A bar that runs into the last column makes the line a continuation, and
the final icon wraps onto a line of its own.  The third column is for
the ellipsis an outline fold hangs after the line: with `truncate-lines'
off, which is Emacs's own default, every folded bar took two rows."
  (cl-letf (((symbol-function 'display-graphic-p) #'ignore))
    (let* ((bar (overblock-bar "label" "^  x " 'shadow))
           (spec (get-text-property
                  (next-single-property-change 0 'display bar)
                  'display bar)))
      (should (equal spec
                     `(space :align-to
                             (- right (,(+ (string-pixel-width
                                            (propertize "^  x " 'face
                                                        'shadow))
                                           3)))))))))

(ert-deftest overblock-test-bar-slack-in-a-frame ()
  "The stretch ends a column short of the right edge in a graphic frame.
Icons that end at the right edge exactly leave redisplay to decide
whether the row wraps: measured in one window at one width, the same bar
drew all its icons when the buffer was opened and put the last one on
a row of its own after the first command."
  (cl-letf (((symbol-function 'display-graphic-p) (lambda (&rest _) t)))
    (let* ((bar (overblock-bar "label" "^  x " 'shadow))
           (spec (get-text-property
                  (next-single-property-change 0 'display bar)
                  'display bar))
           (icons (string-pixel-width (propertize "^  x " 'face 'shadow))))
      (should (equal spec `(space :align-to
                                  (- right (,(+ icons (frame-char-width)))))))
      (should (> (car (nth 2 (nth 2 spec))) icons)))))

(ert-deftest overblock-test-the-bar-label-is-cut-to-fit ()
  "A label wider than the room the icons leave is cut, not wrapped.
The stretch between the two collapses to nothing once the label has
passed its target: in a narrow window the label ran into the first icon
and the last icons wrapped onto a row of their own.  The room is
`window-max-chars-per-line', which counts the line-number area and the
margins, less the icons and one column of slack — a label cut to the
room exactly still put the last icon on a row of its own."
  (with-temp-buffer
    (set-window-buffer nil (current-buffer))
    (let* ((icons "uu")
           (room (- (window-max-chars-per-line)
                    (ceiling (+ (string-pixel-width
                                 (propertize icons 'face 'default))
                                (if (display-graphic-p)
                                    (frame-char-width)
                                  3))
                             (frame-char-width))
                    1))
           (bar (substring-no-properties
                 (overblock-bar (make-string (* 4 room) ?x) icons 'default))))
      ;; the icons are still there, the label is cut, and to the room
      (should (string-suffix-p icons bar))
      (should (string-search "…" bar))
      (should (= (string-width (substring bar 0 (string-search "…" bar)))
                 (1- room)))
      ;; a label that fits is left whole
      (should (string-prefix-p
               "ok" (substring-no-properties
                     (overblock-bar "ok" icons 'default)))))))

(ert-deftest overblock-test-a-bar-for-no-window-is-not-cut ()
  "A buffer in no window has its label left whole.
There is nothing to wrap in, and the cut is baked into the string: a
long cell running while the reader looked at another buffer had its
header cut to the width of that buffer's window, and the cut stayed
when the buffer came back."
  (with-temp-buffer
    (let ((label (make-string 400 ?x)))
      (should-not (get-buffer-window-list nil nil 'visible))
      (should (string-prefix-p
               label (substring-no-properties
                      (overblock-bar label "uu" 'default)))))))

(ert-deftest overblock-test-pieces-carry-an-image ()
  "A piece with an image rides the before-string, the others a display.
Display properties do not nest, so a piece with an image in a display
string would lose it.  Hiding the line with a display string of
nothing and hanging the piece on a string keeps the image and the
line, and a cell with a preview then scrolls a line at a time like any
other.

The before-string and not the after-string: an after-string draws at
the end of the piece, where the next cloak begins, and Emacs leaves
out an overlay string inside invisible text.  Measured on a frame by
the pixels of the image itself: 0 with the cloak there, 32 on the
before-string."
  (with-temp-buffer
    (insert "one\ntwo\nthree\n")
    (let* ((image '(image :type png :data "x"))
           (text (concat "plain piece\n"
                         "piece with " (propertize " " 'display image) "\n"
                         "plain again"))
           (parts (overblock-test--pieces (point-min) (point-max) text))
           (specs (mapcar (lambda (ov)
                            (list (overlay-get ov 'display)
                                  (overlay-get ov 'before-string)
                                  (overlay-get ov 'after-string)))
                          parts)))
      (should (= (length parts) 3))
      ;; the first and the last carry their text as a display string
      (should (equal (nth 0 specs) '("plain piece" nil nil)))
      (should (equal (nth 2 specs) '("plain again" nil nil)))
      ;; the middle one hides its line and shows the image beside it
      (should (equal (car (nth 1 specs)) ""))
      (should (overblock-image-in (cadr (nth 1 specs))))
      ;; and never on the after-string, which a cloak would swallow
      (should-not (nth 2 (nth 1 specs))))))

(ert-deftest overblock-test-space-columns-counts-pixels-and-characters ()
  "A space stretch answers with the columns it covers.
vtable, which is how comint-mime shows a DataFrame, sets the width of
a stretch; shr says where it ends.  A list counts pixels, a bare
number characters."
  (cl-letf (((symbol-function 'frame-char-width) (lambda (&rest _) 8)))
    ;; a width in pixels, and one that is not a whole character
    (should (= (overblock--space-columns '(space :width (16)) 0) 2))
    (should (= (overblock--space-columns '(space :width (5.5)) 0) 1))
    ;; a width in characters
    (should (= (overblock--space-columns '(space :width 3) 0) 3))
    ;; a target counts from where the line starts
    (should (= (overblock--space-columns '(space :align-to (104)) 3) 10))
    ;; nothing to say about a stretch of another kind
    (should-not (overblock--space-columns '(space :relative-width 2) 0))))

(ert-deftest overblock-test-buttons-come-from-their-descriptors ()
  "The header shows the buttons of the option, in its order.
A descriptor whose WHEN is `image' or `lines' waits for those."
  (let ((descriptors '((one ("1") "first" ignore t)
                       (two ("2") "second" ignore lines)
                       (three ("3") "third" ignore image))))
    (should (equal (substring-no-properties
                    (overblock-buttons descriptors nil 0))
                   "1 "))
    (should (equal (substring-no-properties
                    (overblock-buttons descriptors nil 3))
                   "1  2 "))
    (should (equal (substring-no-properties
                    (overblock-buttons descriptors t 3))
                   "1  2  3 "))
    ;; the order is the order of the list
    (should (equal (substring-no-properties
                    (overblock-buttons (reverse descriptors) t 3))
                   "3  2  1 "))
    ;; and a button carries its command and its tooltip
    (let ((row (overblock-buttons descriptors nil 0)))
      (should (equal (get-text-property 0 'help-echo row) "first")))))

(ert-deftest overblock-test-pieces-keep-a-multiline-image-whole ()
  "An image run that covers several lines becomes one piece.
Display math renders as three lines under one image run, and a piece
for each of them drew the same image three times."
  (let* ((image '(image :type png :data "x"))
         (block (propertize "$$\na = b\n$$" 'display image))
         (text (concat "before\n" block "\nafter")))
    ;; three pieces: the prose, the whole block, the prose
    (should (equal (mapcar #'substring-no-properties (overblock--lines text))
                   '("before" "$$\na = b\n$$" "after")))
    (with-temp-buffer
      (insert "one\ntwo\nthree\nfour\nfive\n")
      (let* ((parts (overblock-test--pieces (point-min) (point-max) text))
             (withimage (seq-filter
                         (lambda (ov)
                           (overblock-image-in (or (overlay-get ov 'before-string)
                                                   (overlay-get ov 'display) "")))
                         parts)))
        ;; the image is on one piece, and only one
        (should (= (length withimage) 1))
        (should (equal (substring-no-properties
                        (overlay-get (car withimage) 'before-string))
                       "$$\na = b\n$$"))))))

(ert-deftest overblock-test-image-in-sees-a-slice ()
  "Emacs 31 slices a tall image, and a slice of an image is an image.
The spec is then ((slice X Y W H) IMAGE), and what answers is the image
inside it, so a caller can still read its `:data' and cap its height."
  (let* ((image '(image :type png :data "x"))
         (sliced (propertize " " 'display (list '(slice 0.0 0.0 1.0 0.25)
                                                image))))
    (should (equal (overblock-image-in sliced) image))
    (should (equal (overblock--image-spec (list '(slice 0 0 1 1) image)) image))
    (should-not (overblock--image-spec '(raise 0.5)))
    (should-not (overblock--image-spec '((slice 0 0 1 1) "not an image")))))

(ert-deftest overblock-test-refresh-leaves-a-dead-block-alone ()
  "A block that is no longer in a buffer draws nothing and signals nothing.
`delete-overlay' leaves an overlay that is still an overlay and answers
nil to `overlay-start', which is the position the drawing reads."
  (with-temp-buffer
    (insert "one\ntwo\nthree\n")
    (let ((block (overblock-show (point-min) (point-max) :over "A\nB")))
      (delete-overlay block)
      (should-not (overlay-buffer block))
      ;; Neither the path with a rendering nor the one without it.
      (should (null (overblock-refresh block)))
      (overblock-set block :over nil)
      (should (null (overblock-refresh block))))))

(ert-deftest overblock-test-refresh-draws-in-the-blocks-own-buffer ()
  "The pieces of a block go in the buffer the block is in.
`make-overlay' puts an overlay in whatever buffer is current, so a
refresh from elsewhere would hang the pieces over unrelated text, out of
reach of `overblock-clear' in the buffer they belong to."
  (let ((home (generate-new-buffer "overblock-home"))
        (other (generate-new-buffer "overblock-other")))
    (unwind-protect
        (let (block)
          (with-current-buffer home
            (insert "one\ntwo\nthree\n")
            (setq block (overblock-show (point-min) (point-max) :over "A\nB")))
          (with-current-buffer other
            (insert "elsewhere\n")
            (overblock-refresh block)
            (should (= 0 (length (overlays-in (point-min) (point-max))))))
          (with-current-buffer home
            (should (overblock-get block :parts))
            (dolist (part (overblock-get block :parts))
              (should (eq (overlay-buffer part) home)))))
      (kill-buffer home)
      (kill-buffer other))))

(ert-deftest overblock-test-refresh-under-a-narrowing-terminates ()
  "A block that reaches past a narrowing is still drawn line by line.
An overlay's positions know nothing of a narrowing, and the walk over
the region used to run until the machine was out of memory: the end was
outside the accessible portion, and `forward-line' stops at its edge
without moving."
  (with-temp-buffer
    (insert "one\ntwo\nthree\nfour\nfive\n")
    (let ((block (overblock-show 1 (point-max) :over "A\nB")))
      (narrow-to-region 1 8)
      ;; Bounded rather than timed: `with-timeout' schedules a timer, and
      ;; a timer does not preempt a tight Lisp loop, so the old shape of
      ;; this test could only hang the suite or die of memory.  The region
      ;; is five lines, so the walk makes at most five rows and the parts
      ;; that come out of it cannot outnumber them.
      (overblock-refresh block)
      (let ((parts (overblock-get block :parts)))
        (should parts)
        (should (<= (length parts) 5)))
      (widen)
      (should (overblock-get block :parts)))))

(ert-deftest overblock-test-a-dead-newline-overlay-keeps-the-body ()
  "The body of a block shows even when the newline overlay is gone.
The slot that says where the body rides was tested for an overlay and
not for a live one: a deleted overlay took the body off the anchor and
then refused the write, and the body showed nowhere at all."
  (with-temp-buffer
    (insert "one\ntwo\nthree\n")
    (let* ((block (overblock-show 1 (point-max) :body "RESULT BODY"))
           (newline (overblock-get block :newline)))
      (should (overlay-buffer newline))
      (delete-region (1- (point-max)) (point-max))
      (should-not (overlay-buffer newline))
      (overblock-refresh block)
      (should (string-search "RESULT BODY"
                             (or (overlay-get block 'after-string) ""))))))

(ert-deftest overblock-test-show-under-a-narrowing-keeps-its-shape ()
  "A block made under a narrowing has the anchor and the newline it needs.
`char-before' answers nil past the accessible portion, so the anchor
swallowed the region's last newline and no newline overlay was made."
  (with-temp-buffer
    ;; A region that ends in a blank line: that line is the one the
    ;; header stands on, so a correct `lead' is the empty string.
    (insert "one\ntwo\nthree\n\n")
    (let ((end (point-max)))
      (narrow-to-region 1 8)
      (overblock-test--narrowed-shape end))))

(defun overblock-test--narrowed-shape (end)
  "Check the shape of a block over 1..END made under a narrowing."
  (let ((block (overblock-show 1 end :body "BODY")))
    ;; The anchor stops before the region's last newline, and the
    ;; newline has an overlay of its own — both read with `char-before'
    ;; and `char-after', which answer nil past the accessible portion.
    (should (= (overlay-end block) (1- end)))
    (should (overlay-buffer (overblock-get block :newline)))
    (overblock-set block :header "HDR")
    (overblock-refresh block)
    ;; The row above the header is the region's blank last line, so the
    ;; header needs no break of its own — `char-before' answering nil
    ;; past the accessible portion put one there.
    (should-not (string-prefix-p "\n"
                                 (or (overlay-get block 'after-string) "")))))

(ert-deftest overblock-test-clear-sweeps-the-whole-buffer-however-asked ()
  "A sweep follows the range, not whether the arguments were given.
Every caller but one passes a range, and the whole buffer named
explicitly used to skip the sweep."
  (with-temp-buffer
    (insert "one\ntwo\nthree\n")
    (dolist (args (list (list (point-min) (point-max)) (list (point-min))
                        (list nil (point-max)) nil))
      (let ((block (overblock-show (point-min) (point-max) :over "A")))
        (delete-overlay block)
        (should (overlays-in (point-min) (point-max)))
        (apply #'overblock-clear args)
        (should-not (overlays-in (point-min) (point-max)))))
    ;; A kind still spares the other kinds: an orphan says nothing about
    ;; which kind it belonged to.
    (let ((block (overblock-show (point-min) (point-max) :over "A")))
      (delete-overlay block)
      (overblock-clear (point-min) (point-max) 'markdown)
      (should (overlays-in (point-min) (point-max)))
      (overblock-clear))))

(ert-deftest overblock-test-a-hidden-block-shows-nothing-at-all ()
  "A hidden block gives up its keymap and its help string."
  (with-temp-buffer
    (insert "one\ntwo\n")
    (let ((block (overblock-show (point-min) (point-max)
                                 :over "A" :keymap (make-sparse-keymap)
                                 :help-echo "H")))
      (should (overlay-get block 'keymap))
      (should (overblock-get block :parts))
      (overblock-set block :hidden t)
      (overblock-refresh block)
      (should-not (overlay-get block 'keymap))
      (should-not (overlay-get block 'help-echo))
      (should-not (overblock-get block :parts)))))

(ert-deftest overblock-test-a-blank-last-line-takes-no-row ()
  "A rendering that ends in a blank line does not spend a row on it.
`string-trim' with \"\\n\" took one newline, and a blank line that
carries a space or a tab is still a blank line."
  (with-temp-buffer
    (insert "one\ntwo\nthree\nfour\n")
    (dolist (text '("A\nB\n   \n" "A\nB\n\n   \n\n" "\t\nA\nB"))
      (let* ((block (overblock-show (point-min) (point-max) :over text))
             (pieces (seq-remove (lambda (ov) (overlay-get ov 'overblock-cloak))
                                 (overblock-get block :parts))))
        (should (= (length pieces) 2))
        (overblock-delete block)))))



(ert-deftest overblock-test-a-dead-newline-overlay-is-no-newline ()
  "The slot for the region's last newline can hold a deleted overlay.
Deleting that newline kills the overlay without touching the anchor,
whose range does not cover it, and the drawing then read nil as the end
of the region."
  (with-temp-buffer
    (insert "one\ntwo\nthree\n")
    ;; A region that ends on a newline: that newline is what the slot is
    ;; for, and the anchor stops in front of it.
    (let* ((block (overblock-show 1 (point-max) :over "A"))
           (newline (overblock-get block :newline)))
      (should (overlay-buffer newline))
      (delete-region (1- (point-max)) (point-max))
      (should-not (overlay-buffer newline))
      (should (overlay-buffer block))
      (should (overblock-refresh block)))))

(ert-deftest overblock-test-clear-sweeps-what-lost-its-anchor ()
  "An overlay of the layer whose anchor is gone is swept by a clear.
A package that deletes the overlays it finds in a region can take the
anchor and leave a cloak, which keeps lines of the buffer invisible with
nothing left that knows to take it off."
  (with-temp-buffer
    (insert "one\ntwo\nthree\nfour\n")
    (let ((block (overblock-show (point-min) (point-max) :over "A")))
      ;; Only the anchor, as `delete-overlay' on one found overlay does.
      (delete-overlay block)
      (should (> (length (overlays-in (point-min) (point-max))) 0))
      (overblock-clear)
      (should (= 0 (length (overlays-in (point-min) (point-max))))))))

(ert-deftest overblock-test-dress-takes-a-property-off-again ()
  "A block that no longer carries a keymap has it taken off its overlays."
  (with-temp-buffer
    (insert "one\ntwo\n")
    (let ((block (overblock-show (point-min) (point-max)
                                 :over "A" :keymap (make-sparse-keymap)
                                 :help-echo "help")))
      (should (overlay-get block 'keymap))
      (overblock-set block :keymap nil)
      (overblock-set block :help-echo nil)
      (overblock-refresh block)
      (should-not (overlay-get block 'keymap))
      (should-not (overlay-get block 'help-echo)))))

(ert-deftest overblock-test-the-walk-stops-at-the-end-of-the-buffer ()
  "The row walk ends even where the block reaches past what it can read.
It used to test the position alone, so an end it could never reach spun
the loop and grew its list until the machine was out of memory — which a
test can only report if the loop stops."
  (with-temp-buffer
    (insert "one\ntwo\nthree\nfour\n")
    (let ((block (overblock-show (point-min) (point-max) :over "A\nB")))
      (narrow-to-region 1 5)
      (overblock-refresh block)
      ;; Four lines in the region, so four rows at the most.
      (should (<= (length (overblock-get block :parts)) 4))
      (widen))))

(ert-deftest overblock-test-orphans-go-and-the-living-stay ()
  "The sweep takes what no live block owns, and only that.
A caller that cleared one kind of block reaches for it: an orphan says
nothing about the kind it belonged to.  A bare `overblock-clear' in
its place deleted every live block of every kind first."
  (with-temp-buffer
    (insert "one\ntwo\nthree\n")
    (let ((block (overblock-show (point-min) 8 :over "A"))
          (orphan (make-overlay 9 10)))
      (overlay-put orphan 'overblock-part t)
      (overblock-sweep-orphans)
      (should (overlay-buffer block))
      (should (overblock-get block :parts))
      (should (seq-every-p #'overlay-buffer (overblock-get block :parts)))
      (should-not (overlay-buffer orphan)))))

(ert-deftest overblock-test-an-image-is-named-where-none-draws ()
  "`overblock-image-label' says which figure a display cannot draw.
An image rides a character, and that character is a space: the row was
blank and said nothing at all."
  (let ((text (concat "before "
                      (propertize " " 'display '(image :type png :data "x"))
                      " after")))
    (should (equal (overblock-image-label text) "before [figure] after"))
    (should (equal (overblock-image-label text "[plot]")
                   "before [plot] after"))
    ;; nothing to name, nothing changed
    (should (equal (overblock-image-label "plain") "plain"))))

(ert-deftest overblock-test-image-cap-caps-an-image ()
  "An image drawn inline is capped to a share of the window.
A block taller than the window bounces the wheel backwards off itself
and cannot be scrolled past at all."
  (let ((buffer (get-buffer-create "*overblock test fit*")))
    (unwind-protect
        (with-current-buffer buffer
          (set-window-buffer (selected-window) buffer)
          (let ((line (concat "x" overblock-test--image)))
            (let* ((overblock-image-height 0.5)
                   (fitted (overblock-image-cap line)))
              (should (= (plist-get (cdr (overblock-image-in fitted)) :max-height)
                         (round (* 0.5 (window-body-height
                                        (selected-window) t)))))
              ;; the line kept for the popup is not touched
              (should-not (plist-get (cdr (overblock-image-in line)) :max-height)))
            ;; zero draws it at its own size
            (let* ((overblock-image-height 0)
                   (fitted (overblock-image-cap line)))
              (should-not (plist-get (cdr (overblock-image-in fitted))
                                     :max-height)))))
      (kill-buffer buffer))))

(ert-deftest overblock-test-image-cap-caps-from-an-unshown-buffer ()
  "A block drawn while its buffer is elsewhere is capped too.
A caller works down a buffer while the reader looks at something else,
and no window at all would leave the figure at full size, which is the
block the wheel cannot get past."
  (let ((elsewhere (get-buffer-create "*overblock test elsewhere*"))
        (offscreen (get-buffer-create "*overblock test offscreen*")))
    (unwind-protect
        (progn
          (set-window-buffer (selected-window) elsewhere)
          (with-current-buffer offscreen
            (let* ((overblock-image-height 0.5)
                   (line (concat "x" overblock-test--image))
                   (fitted (overblock-image-cap line)))
              (should-not (get-buffer-window offscreen t))
              (should (= (plist-get (cdr (overblock-image-in fitted)) :max-height)
                         (round (* 0.5 (window-body-height
                                        (selected-window) t))))))))
      (kill-buffer elsewhere)
      (kill-buffer offscreen))))

(ert-deftest overblock-test-image-cap-unslices-a-tall-image ()
  "A run of slices becomes the whole image, capped, on its first row.
Emacs 31 slices an image taller than `shr-sliced-image-height' into a
row for each line of the window it was rendered in.  Slicing does not
make an image smaller, so leaving the slices alone left the cap with
nothing to cap — and the image cannot be capped under the slice either,
because the fractions were worked out against the height it had."
  (let* ((image '(image :type png :data "x"))
         (rows (list '(slice 0.0 0.0 1.0 0.5) '(slice 0.0 0.5 1.0 0.5)))
         (line (concat (propertize " " 'display (list (nth 0 rows) image))
                       "\n"
                       (propertize " " 'display (list (nth 1 rows) image)))))
    (cl-letf (((symbol-function 'overblock-image-limit) (lambda () 100)))
      (let* ((fitted (overblock-image-cap line))
             (first (get-text-property 0 'display fitted))
             (later (get-text-property (1- (length fitted)) 'display fitted)))
        ;; The first row carries the image, capped and no longer sliced.
        (should (eq (car-safe first) 'image))
        (should (= (plist-get (cdr first) :max-height) 100))
        ;; The rows that followed it carry nothing.
        (should (equal later ""))))))

(ert-deftest overblock-test-a-window-too-narrow-for-the-icons-loses-them ()
  "Where not even the icons fit, they go and the label is an ellipsis.
The label alone was cut there, so the icons wrapped onto a row of their
own: measured in a terminal, a bar of four buttons took two rows at 16
columns and one at 20.  A wrapped bar is two rows of almost nothing."
  (cl-letf (((symbol-function 'overblock-window-width) (lambda () 12)))
    (let ((bar (substring-no-properties
                (overblock-bar "a long label indeed" "u  d  a  r " 'default))))
      (should (string-prefix-p "…" (string-trim bar)))
      (should-not (string-search "u" bar))
      (should-not (string-search "r" bar))))
  ;; and where they do fit, they are all there
  (cl-letf (((symbol-function 'overblock-window-width) (lambda () 400)))
    (let ((bar (substring-no-properties
                (overblock-bar "label" "u  d  a  r " 'default))))
      (should (string-search "u  d  a  r" bar))
      (should (string-prefix-p "label" bar)))))

(ert-deftest overblock-test-a-button-is-wider-than-its-glyph ()
  "The space after a glyph belongs to its button.
Measured in a window of 1554 pixels, the places a reader could press
were ten pixels wide with twenty pixels of nothing between them."
  (let* ((icons (overblock-buttons
                 '((one ("A") "First" first-command t)
                   (two ("B") "Second" second-command t))))
         (at (lambda (pos) (get-text-property pos 'keymap icons))))
    ;; the glyph and the space after it answer to the same command
    (should (funcall at 0))
    (should (eq (funcall at 0) (funcall at 1)))
    ;; and the next button is a different one
    (should (funcall at 3))
    (should-not (eq (funcall at 0) (funcall at 3)))))

(provide 'overblock-test)
(ert-deftest overblock-test-a-block-keeps-out-of-the-way-of-hl-line ()
  "The plain paint of a block sits below `hl-line\', which draws at -50.
The source under a block is painted plain so that the face of a newline
does not run its background out to the window; with no priority at all
that outranked the stripe of `hl-line', and the stripe disappeared
wherever a block stood."
  (with-temp-buffer
    (insert "one\ntwo\nthree\n")
    (let ((block (overblock-show (point-min) (pos-eol 2) :body "over")))
      (should (eq (overlay-get block 'face) 'default))
      (should (< (overlay-get block 'priority) -50))
      (let ((newline (overblock-get block :newline)))
        (should (eq (overlay-get newline 'face) 'default))
        (should (< (overlay-get newline 'priority) -50))))))

(ert-deftest overblock-test-a-block-built-for-another-width-is-dropped ()
  "A block carries the columns it was built for, and loses them to a change.
A rendering is filled to the width it is shown at, so one built for
another width has to go; the mode that made it renders it again on its
own idle cycle."
  (let ((buffer (generate-new-buffer "overblock-width")))
    (unwind-protect
        (progn
          (set-window-buffer (selected-window) buffer)
          (with-current-buffer buffer
            (insert "one\ntwo\nthree\n")
            (let ((block (overblock-show (point-min) (pos-eol 2)
                                         :kind 'test :body "over")))
              ;; what the window says, which in a batch frame is a number
              (should (eql (overlay-get block 'overblock-columns)
                           (overblock-window-columns)))
              ;; a width that is still the same drops nothing
              (overblock-width-follow 'test)
              (should (overlay-buffer block))
              ;; and one that is not takes the block down
              (overlay-put block 'overblock-columns 12)
              (overblock-width-follow 'test)
              (should-not (overblock-in (point-min) (point-max) 'test)))))
      (set-window-buffer (selected-window) (other-buffer))
      (kill-buffer buffer))))

;;; overblock-test.el ends here
