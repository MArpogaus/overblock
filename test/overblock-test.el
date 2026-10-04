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
(require 'overblock-test-common)

(defun overblock-test--pieces (beg end text)
  "Return the pieces a block hangs TEXT on over the region BEG..END."
  (overblock-get (overblock-show beg end :over text) :parts))

(ert-deftest overblock-test-image-in-finds-the-first-one ()
  "The first image of a result is found, and plain text has none."
  (should (eq (car-safe (overblock-image-in (concat "a\n" overblock-test-common-image))) 'image))
  (should-not (overblock-image-in "just text")))

(ert-deftest overblock-test-glyph-falls-back-to-the-last-candidate ()
  "A candidate without a glyph is skipped, and the last one always answers."
  ;; A batch session has no graphical frame, so the fallback decides.
  (should (equal (overblock-glyph "⤓" "↧" "↓") "↓"))
  (should (equal (overblock-glyph "x") "x")))

(ert-deftest overblock-test-a-trusted-terminal-gets-the-icons ()
  "A terminal draws the best candidate where the reader says it can.
Emacs cannot test the font of a terminal, so a terminal gets no icons
unless `overblock-terminal-glyphs' is non-nil.  Then the coding system
decides."
  (let ((overblock-terminal-glyphs nil))
    (overblock--forget-glyphs)
    (should (equal (overblock-glyph "\uEBCC" "◫" "copy") "copy")))
  (let ((overblock-terminal-glyphs t))
    (overblock--forget-glyphs)
    (should (equal (overblock-glyph "\uEBCC" "◫" "copy") "\uEBCC"))
    ;; What the terminal cannot encode, it still does not get.
    (cl-letf (((symbol-function 'char-displayable-p)
               (lambda (ch) (not (eq ch ?\uEBCC)))))
      (overblock--forget-glyphs)
      (should (equal (overblock-glyph "\uEBCC" "◫" "copy") "◫"))))
  (overblock--forget-glyphs))

(ert-deftest overblock-test-the-glyph-answer-is-forgotten-on-a-change ()
  "An answer kept from before the option changed is not reused.
The answers are memoized per display, font and option, and a reader
can change the option after the bars are drawn."
  (let ((overblock-terminal-glyphs nil))
    (should (equal (overblock-glyph "\uEBCC" "◫" "copy") "copy")))
  (let ((overblock-terminal-glyphs t))
    (should (equal (overblock-glyph "\uEBCC" "◫" "copy") "\uEBCC"))))

(ert-deftest overblock-test-glyph-weighs-every-character ()
  "A leading space must not answer for the glyph behind it.
Some candidates start with a space, which is always drawable, so every
character is tested."
  (cl-letf (((symbol-function 'display-graphic-p) (lambda (&rest _) t))
            ;; A frame with the space and two of the three arrows.
            ((symbol-function 'char-displayable-p)
             (lambda (ch) (memq ch '(?\s ?▶ ?>)))))
    (should (equal (overblock-glyph " ▸" " ▶" " >") " ▶"))
    (should (equal (overblock-glyph " ▸" " ▴" " >") " >"))))

(ert-deftest overblock-test-faced-gives-a-string-a-base-face ()
  "A block string carries a base face, so it inherits none."
  (let ((s (overblock-faced (copy-sequence "text") 'shadow)))
    (should (memq 'shadow (ensure-list (get-text-property 0 'face s))))))

(ert-deftest overblock-test-slots ()
  "Each row of a block lands in the slot that suits it.
The header is an overlay string, where a bar can put its icons at the
window edge; a plain body is the display property, the cheapest slot;
a body with an image is an overlay string, because a display property
swallows an image."
  (with-temp-buffer
    (insert "one\ntwo\n")
    (let* ((block (overblock-show 1 (point-max)
                                  :header "H" :body "B"))
           (nl (overblock-get block :newline)))
      (should (equal (overlay-get block 'after-string) "\nH"))
      (should (equal (overlay-get nl 'display) "\nB\n"))
      ;; A body with an image joins the header on the anchor, where an
      ;; image draws. The newline keeps its character, so the wheel can
      ;; pass the block.
      (overblock-set block :body (concat "B" overblock-test-common-image))
      (overblock-refresh block)
      (should-not (overlay-get nl 'display))
      (should (overblock-image-in (overlay-get block 'after-string))))))

(ert-deftest overblock-test-body-without-a-newline ()
  "A body shows even where the region ends without a newline.
The cheap slot is the display property of that newline, and a region
at the end of a buffer can have none: the body then joins the rows on
the anchor."
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

(ert-deftest overblock-test-a-cloak-is-invisible-not-a-display ()
  "The lines a cloak hides are invisible text, never a display string.
Point stops on each position of a run under a display string, and
moves over invisible text.  Only the guard on the newline the cloak
leaves draws, and it draws one newline."
  (with-temp-buffer
    (insert "one\ntwo\nthree\nfour\n")
    (goto-char (point-min))
    (let ((block (overblock-show 1 (point-max) :over "row one")))
      (dolist (part (overblock-get block :parts))
        (when (overlay-get part 'overblock-cloak)
          (if (equal (overlay-get part 'display) "\n")
              (should (= 1 (- (overlay-end part) (overlay-start part))))
            (should (eq (overlay-get part 'invisible) t))
            (should-not (overlay-get part 'display))))))))

(ert-deftest overblock-test-no-final-newline-draws-no-newline ()
  "A file that ends without a newline gets no guard on its last line.
The guard goes where the cloak leaves a newline.  Where there is none,
the character there is text, and a guard would add a blank row."
  (with-temp-buffer
    (insert "one\ntwo\nthree")
    (let ((block (overblock-show 1 (point-max) :over "row one")))
      (should-not (seq-find (lambda (part)
                              (and (equal (overlay-get part 'display) "\n")
                                   (not (eq (char-after (overlay-start part))
                                            ?\n))))
                            (overblock-get block :parts))))))

(ert-deftest overblock-test-covers-its-last-line ()
  "The pieces of a block reach the last line of its region.
The anchor stops before the newline that ends the region, and a cloak
that stopped there with it would leave the last line on the screen."
  (with-temp-buffer
    ;; The last line is blank, so no row is left for it.
    (insert "one\ntwo\n\n")
    (let* ((block (overblock-show 1 (point-max) :over "row one\nrow two"))
           (cloaks (seq-filter (lambda (ov) (overlay-get ov 'overblock-cloak))
                               (overblock-get block :parts))))
      (should cloaks)
      ;; Through the newline that ends the region: the guard on that
      ;; newline is the last part of the cloak.
      (should (= (apply #'max (mapcar #'overlay-end cloaks))
                 (point-max))))))

(ert-deftest overblock-test-pieces-lose-no-line ()
  "The pieces together show the rendering, whole and in order.
The region and the rendering rarely have the same number of lines."
  (let ((shown (lambda (parts)
                 (mapconcat (lambda (p) (overlay-get p 'display))
                            (seq-remove (lambda (p) (overlay-get p 'overblock-cloak))
                                        parts)
                            "\n"))))
    ;; More rendering than lines.
    (with-temp-buffer
      (insert "aaa\nbbb\n")
      (let ((text "one\ntwo\nthree\nfour\nfive"))
        (should (equal (funcall shown (overblock-test--pieces (point-min) (point-max) text))
                       text))))
    ;; More lines than rendering.
    (with-temp-buffer
      (insert "aaa\nbbb\nccc\nddd\neee\n")
      (let* ((text "one\ntwo")
             (parts (overblock-test--pieces (point-min) (point-max) text)))
        (should (equal (funcall shown parts) text))
        (should (seq-some (lambda (p) (overlay-get p 'overblock-cloak)) parts))))))

(ert-deftest overblock-test-the-first-row-shows-the-first-line ()
  "The first line of a rendering stands on the first row of the region.
It is the only row that starts where the block does, and the first
line of a rendering is written for that column.  A rendering with
fewer lines than the region must not leave the first row empty."
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
    (overblock--fill-props s 'keymap 'block-map)
    (should (eq (get-text-property 0 'keymap s) 'block-map))
    (should (eq (get-text-property 6 'keymap s) 'shr-map))))

(ert-deftest overblock-test-the-overlays-answer-for-point ()
  "Every overlay a block draws carries its keymap and its help echo.
A click finds the keymap of the string it lands on, such as the map of
shr on a link.  Point never enters a display string, so a key pressed
in a block is answered by the overlays alone."
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
A bar that runs into the last column wraps its final icon.  The third
column is for the ellipsis of an outline fold, with `truncate-lines'
off (the default)."
  (cl-letf (((symbol-function 'display-graphic-p) #'ignore))
    (let* ((bar (overblock-bar "" "label" "^  x " 'shadow))
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
Icons that end at the right edge exactly can wrap or not, at the whim
of redisplay."
  (cl-letf (((symbol-function 'display-graphic-p) (lambda (&rest _) t)))
    (let* ((bar (overblock-bar "" "label" "^  x " 'shadow))
           (spec (get-text-property
                  (next-single-property-change 0 'display bar)
                  'display bar))
           (icons (string-pixel-width (propertize "^  x " 'face 'shadow))))
      (should (equal spec `(space :align-to
                                  (- right (,(+ icons (frame-char-width)))))))
      (should (> (car (nth 2 (nth 2 spec))) icons)))))

(ert-deftest overblock-test-the-bar-label-is-cut-to-fit ()
  "A label wider than the room the icons leave is cut, not wrapped.
The stretch between the two shrinks to nothing after the label passes
its target, and the icons would wrap.  The room is
`window-max-chars-per-line', less the icons, the slack and one more
column, because a label cut exactly still wraps."
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
                 (overblock-bar "" (make-string (* 4 room) ?x) icons 'default))))
      ;; The icons are there, and the label is cut to the room.
      (should (string-suffix-p icons bar))
      (should (string-search "…" bar))
      (should (= (string-width (substring bar 0 (string-search "…" bar)))
                 (1- room)))
      ;; A label that fits stays whole.
      (should (string-prefix-p
               "ok" (substring-no-properties
                     (overblock-bar "" "ok" icons 'default)))))))

(ert-deftest overblock-test-a-bar-for-no-window-is-not-cut ()
  "A buffer in no window has its label left whole.
There is nothing to wrap in, and a cut would stay in the string after
the buffer shows again."
  (with-temp-buffer
    (let ((label (make-string 400 ?x)))
      (should-not (get-buffer-window-list nil nil 'visible))
      (should (string-prefix-p
               label (substring-no-properties
                      (overblock-bar "" label "uu" 'default)))))))

(ert-deftest overblock-test-pieces-carry-an-image ()
  "A piece with an image rides the before-string, the others a display.
Display properties do not nest, so a piece with an image in a display
string would lose it.  An empty display string hides the line, and an
overlay string carries the piece, so the image shows and the cell
scrolls a line at a time.

The before-string, not the after-string: an after-string draws at the
end of the piece, where the next cloak starts, and Emacs does not draw
an overlay string inside invisible text."
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
      ;; The first and the last carry their text as a display string.
      (should (equal (nth 0 specs) '("plain piece" nil nil)))
      (should (equal (nth 2 specs) '("plain again" nil nil)))
      ;; The middle one hides its line and shows the image.
      (should (equal (car (nth 1 specs)) ""))
      (should (overblock-image-in (cadr (nth 1 specs))))
      ;; Never on the after-string, which a cloak would hide.
      (should-not (nth 2 (nth 1 specs))))))

(ert-deftest overblock-test-space-columns-counts-pixels-and-characters ()
  "A space stretch answers with the columns it covers.
vtable, with which comint-mime shows a DataFrame, sets the width of a
stretch; shr says where it ends.  A list counts pixels, a bare number
characters."
  (cl-letf (((symbol-function 'frame-char-width) (lambda (&rest _) 8)))
    ;; A width in pixels, and one that is not a whole character.
    (should (= (overblock--space-columns '(space :width (16)) 0) 2))
    (should (= (overblock--space-columns '(space :width (5.5)) 0) 1))
    ;; A width in characters.
    (should (= (overblock--space-columns '(space :width 3) 0) 3))
    ;; A target counts from the start of the line.
    (should (= (overblock--space-columns '(space :align-to (104)) 3) 10))
    ;; Nothing for a stretch of another kind.
    (should-not (overblock--space-columns '(space :relative-width 2) 0))))

(ert-deftest overblock-test-a-narrowed-stop-still-takes-every-block ()
  "Turning a cycle off under a narrowing leaves nothing behind.
The bounds of a narrowed buffer would leave the blocks outside it, and
with the mode off nothing removes them.  A stray cloak keeps lines
invisible."
  (with-temp-buffer
    (insert "one\ntwo\nthree\nfour\nfive\nsix\n")
    (overblock-live-start 'probe #'ignore)
    (unwind-protect
        (progn
          (overblock-show 1 4 :kind 'probe :over "A")
          (overblock-show 15 19 :kind 'probe :over "B")
          (should (= (length (overblock-in (point-min) (point-max) 'probe)) 2))
          (narrow-to-region 1 8)
          (overblock-live-stop 'probe)
          (widen)
          (should-not (overblock-in (point-min) (point-max) 'probe))
          (should-not (seq-filter (lambda (o) (overlay-get o 'overblock-part))
                                  (overlays-in (point-min) (point-max)))))
      (overblock-live-stop 'probe))))

(defun overblock-test--said ()
  "A button command for the tests.  The test stubs it."
  (interactive))

(ert-deftest overblock-test-a-button-release-says-the-newest-message-again ()
  "Reading the release of a click clears the echo area, so it says again.
The press runs the command and notes that it said something; the
release shows the newest message, which can be one logged since."
  (let* ((map (get-text-property 0 'keymap
                                 (overblock-button "x" "help" #'overblock-test--said)))
         (press (keymap-lookup map "<down-mouse-1>"))
         (said nil))
    (cl-letf (((symbol-function 'overblock-test--said)
               (lambda () (interactive) (setq said 'pressed)))
              ((symbol-function 'current-message) (lambda () "queued")))
      (call-interactively press))
    (should (eq said 'pressed))
    (cl-flet ((log (text)
                (with-current-buffer (messages-buffer)
                  (let ((inhibit-read-only t))
                    (goto-char (point-max))
                    (insert text "\n"))))
              (release ()
                (let (shown)
                  (cl-letf (((symbol-function 'message)
                             (lambda (_format text) (setq shown text))))
                    (call-interactively (keymap-lookup map "<mouse-1>")))
                  shown)))
      (log "queued")
      (should (equal (release) "queued"))
      ;; A message logged since the press, the end of a short pass,
      ;; wins.
      (setq overblock--pressed "queued")
      (log "done")
      (should (equal (release) "done"))
      ;; A press that said nothing has nothing said again.
      (should-not (release))
      ;; The log counts a repeat, which is not said; with no log, the
      ;; message of the press is.
      (setq overblock--pressed "queued")
      (log "queued [2 times]")
      (should (equal (release) "queued"))
      (setq overblock--pressed "queued")
      (log "older")
      (let ((message-log-max nil))
        (should (equal (release) "queued"))))))

(ert-deftest overblock-test-a-button-row-is-kept-per-display ()
  "The row a display draws is not the row another display draws.
`overblock-glyph' answers by the kind of display, the frame font and
`overblock-terminal-glyphs', so the key of a row holds those three.
Then a graphic frame and a terminal frame of one daemon get their own
rows, and a plain `setq' of the option takes effect."
  (skip-unless (not (display-graphic-p)))
  (let ((descriptors '((one ("\uEA76 " "x ") "first" ignore t))))
    (overblock--forget-glyphs)
    (let ((overblock-terminal-glyphs nil))
      ;; A private use glyph is refused in an untrusted terminal.
      (should (equal (substring-no-properties (overblock-buttons descriptors))
                     "x  ")))
    (let ((overblock-terminal-glyphs t))
      (should (equal (substring-no-properties (overblock-buttons descriptors))
                     "\uEA76  ")))
    ;; The first answer is still the same.
    (let ((overblock-terminal-glyphs nil))
      (should (equal (substring-no-properties (overblock-buttons descriptors))
                     "x  ")))))

(ert-deftest overblock-test-a-button-row-is-built-once ()
  "The row is built once for a question and read from the table after.
The header of a running result asks five times a second."
  (let ((descriptors '((one ("x ") "first" ignore t)))
        (built 0))
    (overblock--forget-glyphs)
    (cl-letf* ((real (symbol-function 'overblock--buttons))
               ((symbol-function 'overblock--buttons)
                (lambda (&rest args) (setq built (1+ built)) (apply real args))))
      (dotimes (_ 5) (overblock-buttons descriptors nil 3 t))
      (should (= built 1))
      ;; Another question, another build.
      (overblock-buttons descriptors nil 0 t)
      (should (= built 2)))))

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
    ;; The order is the order of the list.
    (should (equal (substring-no-properties
                    (overblock-buttons (reverse descriptors) t 3))
                   "3  2  1 "))
    ;; A button carries its tooltip.
    (let ((row (overblock-buttons descriptors nil 0)))
      (should (equal (get-text-property 0 'help-echo row) "first")))))

(ert-deftest overblock-test-pieces-keep-a-multiline-image-whole ()
  "An image run that covers several lines becomes one piece.
Display math renders as three lines under one image run, and a piece
for each would draw the image three times."
  (let* ((image '(image :type png :data "x"))
         (block (propertize "$$\na = b\n$$" 'display image))
         (text (concat "before\n" block "\nafter")))
    ;; Three pieces: the prose, the whole block, the prose.
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
        ;; The image is on exactly one piece.
        (should (= (length withimage) 1))
        (should (equal (substring-no-properties
                        (overlay-get (car withimage) 'before-string))
                       "$$\na = b\n$$"))))))

(ert-deftest overblock-test-image-in-sees-a-slice ()
  "Emacs 31 slices a tall image, and a slice of an image is an image.
The spec is then ((slice X Y W H) IMAGE), and the image inside it is
returned, so a caller can read its `:data' and cap its height."
  (let* ((image '(image :type png :data "x"))
         (sliced (propertize " " 'display (list '(slice 0.0 0.0 1.0 0.25)
                                                image))))
    (should (equal (overblock-image-in sliced) image))
    (should (equal (overblock--image-spec (list '(slice 0 0 1 1) image)) image))
    (should-not (overblock--image-spec '(raise 0.5)))
    (should-not (overblock--image-spec '((slice 0 0 1 1) "not an image")))))

(ert-deftest overblock-test-an-empty-rendering-is-a-block-that-shows-nothing ()
  "A region that renders to nothing keeps its text, and counts as done."
  (with-temp-buffer
    (insert "---\ntitle: x\n---\n")
    (let ((block (overblock-show-rendering 1 (1- (point-max)) "\n" 'default
                                           :kind 'md-preview)))
      (should block)
      (should (overblock-in 1 (point-max) 'md-preview))
      (should-not (overblock-get block :parts)))))

(ert-deftest overblock-test-refresh-leaves-a-dead-block-alone ()
  "A block that is no longer in a buffer draws nothing and signals nothing.
A deleted overlay has no start, which the drawing reads."
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
Overlay positions ignore a narrowing, and `forward-line' stops at its
edge without moving, so the walk must not loop."
  (with-temp-buffer
    (insert "one\ntwo\nthree\nfour\nfive\n")
    (let ((block (overblock-show 1 (point-max) :over "A\nB")))
      (narrow-to-region 1 8)
      ;; Bounded, not timed: a timer does not preempt a tight Lisp
      ;; loop. The region is five lines, so at most five rows.
      (overblock-refresh block)
      (let ((parts (seq-remove (lambda (ov) (overlay-get ov 'overblock-cloak))
                               (overblock-get block :parts))))
        (should parts)
        (should (<= (length parts) 5)))
      (widen)
      (should (overblock-get block :parts)))))

(ert-deftest overblock-test-a-dead-newline-overlay-keeps-the-body ()
  "The body of a block shows even when the newline overlay is gone.
A deleted newline overlay sends the body to the anchor."
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
`char-before' returns nil outside the accessible portion, so the
shape is read without the narrowing."
  (with-temp-buffer
    ;; A region that ends in a blank line, on which the header goes, so
    ;; the lead is the empty string.
    (insert "one\ntwo\nthree\n\n")
    (let ((end (point-max)))
      (narrow-to-region 1 8)
      (overblock-test--narrowed-shape end))))

(defun overblock-test--narrowed-shape (end)
  "Check the shape of a block over 1..END made under a narrowing."
  (let ((block (overblock-show 1 end :body "BODY")))
    ;; The anchor stops before the last newline of the region, and the
    ;; newline has an overlay of its own.
    (should (= (overlay-end block) (1- end)))
    (should (overlay-buffer (overblock-get block :newline)))
    (overblock-set block :header "HDR")
    (overblock-refresh block)
    ;; The row above the header is the blank last line of the region,
    ;; so the header needs no break.
    (should-not (string-prefix-p "\n"
                                 (or (overlay-get block 'after-string) "")))))

(ert-deftest overblock-test-clear-sweeps-the-whole-buffer-however-asked ()
  "A sweep follows the range, not whether the arguments were given.
The whole buffer, named or by default, sweeps."
  (with-temp-buffer
    (insert "one\ntwo\nthree\n")
    (dolist (args (list (list (point-min) (point-max)) (list (point-min))
                        (list nil (point-max)) nil))
      (let ((block (overblock-show (point-min) (point-max) :over "A")))
        (delete-overlay block)
        (should (overlays-in (point-min) (point-max)))
        (apply #'overblock-clear args)
        (should-not (overlays-in (point-min) (point-max)))))
    ;; A clear of one kind does not sweep: an orphan has no kind.
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
A blank line that carries a space or a tab is still a blank line."
  (with-temp-buffer
    (insert "one\ntwo\nthree\nfour\n")
    (dolist (text '("A\nB\n   \n" "A\nB\n\n   \n\n" "\t\nA\nB"))
      (let* ((block (overblock-show (point-min) (point-max) :over text))
             (pieces (seq-remove (lambda (ov) (overlay-get ov 'overblock-cloak))
                                 (overblock-get block :parts))))
        (should (= (length pieces) 2))
        (overblock-delete block)))))



(ert-deftest overblock-test-a-dead-newline-overlay-is-no-newline ()
  "The slot for the last newline of the region can hold a deleted overlay.
Deleting that newline deletes the overlay but not the anchor, whose
range does not cover it, and a deleted overlay has no end."
  (with-temp-buffer
    (insert "one\ntwo\nthree\n")
    ;; A region that ends on a newline, before which the anchor stops.
    (let* ((block (overblock-show 1 (point-max) :over "A"))
           (newline (overblock-get block :newline)))
      (should (overlay-buffer newline))
      (delete-region (1- (point-max)) (point-max))
      (should-not (overlay-buffer newline))
      (should (overlay-buffer block))
      (should (overblock-refresh block)))))

(ert-deftest overblock-test-clear-sweeps-what-lost-its-anchor ()
  "An overlay of the layer whose anchor is gone is swept by a clear.
A package that deletes the overlays of a region can remove the anchor
and leave a cloak, which keeps lines invisible."
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
A test of the position alone loops for ever on an end it cannot
reach."
  (with-temp-buffer
    (insert "one\ntwo\nthree\nfour\n")
    (let ((block (overblock-show (point-min) (point-max) :over "A\nB")))
      (narrow-to-region 1 5)
      (overblock-refresh block)
      ;; Four lines in the region, so four rows at the most.
      (should (<= (length (seq-remove (lambda (ov) (overlay-get ov 'overblock-cloak))
                                      (overblock-get block :parts)))
                  4))
      (widen))))

(ert-deftest overblock-test-orphans-go-and-the-living-stay ()
  "The sweep takes what no live block owns, and only that.
A caller that cleared one kind of block calls it, because an orphan
has no kind.  Live blocks of every kind stay."
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
An image is on a space, which shows as a blank row."
  (let ((text (concat "before "
                      (propertize " " 'display '(image :type png :data "x"))
                      " after")))
    (should (equal (overblock-image-label text) "before [figure] after"))
    ;; Nothing to name, nothing changed.
    (should (equal (overblock-image-label "plain") "plain"))))

(ert-deftest overblock-test-image-cap-holds-a-given-height ()
  "A :height taller than the limit comes down to it, with its width."
  (let ((capped (overblock--image-capped
                 '(image :type png :file "x.png" :width 200 :height 500) 300)))
    (should (= (plist-get (cdr capped) :height) 300))
    (should (= (plist-get (cdr capped) :width) 120))))

(ert-deftest overblock-test-image-cap-caps-an-image ()
  "An image drawn inline is capped to a share of the window.
A block taller than the window cannot be scrolled past."
  (let ((buffer (get-buffer-create "*overblock test fit*")))
    (unwind-protect
        (with-current-buffer buffer
          (set-window-buffer (selected-window) buffer)
          (let ((line (concat "x" overblock-test-common-image)))
            (let* ((overblock-image-height 0.5)
                   (fitted (overblock-image-cap line)))
              (should (= (plist-get (cdr (overblock-image-in fitted)) :max-height)
                         (round (* 0.5 (window-body-height
                                        (selected-window) t)))))
              ;; The line kept for the pop-out is not changed.
              (should-not (plist-get (cdr (overblock-image-in line)) :max-height)))
            ;; Zero draws it at its own size.
            (let* ((overblock-image-height 0)
                   (fitted (overblock-image-cap line)))
              (should-not (plist-get (cdr (overblock-image-in fitted))
                                     :max-height)))))
      (kill-buffer buffer))))

(ert-deftest overblock-test-image-cap-caps-from-an-unshown-buffer ()
  "A block drawn while its buffer is elsewhere is capped too.
Without a window the figure would be at full size, and could not be
scrolled past."
  (let ((elsewhere (get-buffer-create "*overblock test elsewhere*"))
        (offscreen (get-buffer-create "*overblock test offscreen*")))
    (unwind-protect
        (progn
          (set-window-buffer (selected-window) elsewhere)
          (with-current-buffer offscreen
            (let* ((overblock-image-height 0.5)
                   (line (concat "x" overblock-test-common-image))
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
make an image smaller, and an image cannot be capped under its slices,
whose fractions are for the old height."
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
Else the icons wrap, and the bar takes two rows."
  (cl-letf (((symbol-function 'overblock--window-width) (lambda () 12)))
    (let ((bar (substring-no-properties
                (overblock-bar "" "a long label indeed" "u  d  a  r " 'default))))
      (should (string-prefix-p "…" (string-trim bar)))
      (should-not (string-search "u" bar))
      (should-not (string-search "r" bar))))
  ;; Where they fit, they are all there.
  (cl-letf (((symbol-function 'overblock--window-width) (lambda () 400)))
    (let ((bar (substring-no-properties
                (overblock-bar "" "label" "u  d  a  r " 'default))))
      (should (string-search "u  d  a  r" bar))
      (should (string-prefix-p "label" bar)))))

(ert-deftest overblock-test-a-button-is-wider-than-its-glyph ()
  "The space after a glyph belongs to its button.
This makes each target two columns wide."
  (let* ((icons (overblock-buttons
                 '((one ("A") "First" first-command t)
                   (two ("B") "Second" second-command t))))
         (at (lambda (pos) (get-text-property pos 'keymap icons))))
    ;; The glyph and the space after it run the same command.
    (should (funcall at 0))
    (should (eq (funcall at 0) (funcall at 1)))
    ;; The next button is a different one.
    (should (funcall at 3))
    (should-not (eq (funcall at 0) (funcall at 3)))))

(ert-deftest overblock-test-a-block-keeps-out-of-the-way-of-hl-line ()
  "The plain paint of a rendering sits below `hl-line\', which draws at -50.
The source under a rendering is painted plain so that the face of a
newline does not extend its background to the window edge.  A higher
priority would hide the stripe of `hl-line'."
  (with-temp-buffer
    (insert "one\ntwo\nthree\n")
    (goto-char (point-min))
    (let ((block (overblock-show (point-min) (pos-eol 2) :over "over")))
      (should (equal (overlay-get block 'face) overblock--plain))
      (should (< (overlay-get block 'priority) -50))
      (let ((newline (overblock-get block :newline)))
        (should (equal (overlay-get newline 'face) overblock--plain))
        (should (< (overlay-get newline 'priority) -50))))))

(ert-deftest overblock-test-an-active-region-shows-its-source ()
  "The renderings an active region reaches come down, and stay down while it lasts.
The reader copies what the region marks, and a rendering is never in
the buffer.  When the mark is gone the region wants its rendering again."
  (with-temp-buffer
    (insert "one\ntwo\nthree\nfour\n")
    (transient-mark-mode 1)
    (overblock-live-start 'test #'ignore)
    (goto-char (point-min))
    (let ((first (overblock-show 1 (pos-eol 1) :kind 'test :body "ONE"))
          (last (overblock-show (pos-bol 4) (pos-eol 4) :kind 'test :body "FOUR")))
      (should (and first last))
      ;; A region over the first two lines.
      (push-mark (pos-eol 2) t t)
      (should (use-region-p))
      (overblock-live--settle)
      (should-not (overlay-buffer first))
      (should (overlay-buffer last))
      (should-not (overblock-live-wanted-p 1 (pos-eol 1) 'test))
      (deactivate-mark)
      (let ((end (pos-eol 1)))
        (goto-char (point-max))
        (should (overblock-live-wanted-p 1 end 'test))))
    (overblock-live-stop 'test)))

(ert-deftest overblock-test-a-result-leaves-the-faces-of-its-region-alone ()
  "Only a rendering paints the source under it plain.
A result hangs below its region and leaves the code in view.  The face
of an overlay, `default' included, outranks font lock."
  (with-temp-buffer
    (insert "import os\nprint(1)\n")
    (goto-char (point-min))
    (let ((result (overblock-show 1 (pos-eol 1) :kind 'result :body "1"))
          (rendering (overblock-show (pos-bol 2) (point-max) :kind 'md :over "one")))
      (should-not (overlay-get result 'face))
      (should (equal (overlay-get rendering 'face) overblock--plain)))))

(ert-deftest overblock-test-a-block-built-for-another-width-is-dropped ()
  "A block carries the columns it was built for, and loses them to a change.
A rendering is filled to its width, so one built for another width
goes; the live cycle renders it again."
  (let ((buffer (generate-new-buffer "overblock-width")))
    (unwind-protect
        (progn
          (set-window-buffer (selected-window) buffer)
          (with-current-buffer buffer
            (insert "one\ntwo\nthree\n")
            (let ((block (overblock-show (point-min) (pos-eol 2)
                                         :kind 'test :body "over"))
                  (redrawn 0))
              ;; A live cycle of that kind drops them.
              (overblock-live-start 'test #'ignore)
              (add-hook 'overblock-width-functions
                        (lambda () (cl-incf redrawn)) nil t)
              ;; In a batch frame the window gives a number.
              (should (eql (overlay-get block 'overblock-columns)
                           (overblock-window-columns)))
              ;; The same width drops and redraws nothing.
              (overblock--width-changed)
              (should (overlay-buffer block))
              (should (= redrawn 0))
              ;; Another width drops the block and redraws.
              (overlay-put block 'overblock-columns 12)
              (setq overblock--columns 12)
              (overblock--width-changed)
              (should-not (overblock-in (point-min) (point-max) 'test))
              (should (= redrawn 1)))))
      (set-window-buffer (selected-window) (other-buffer))
      (kill-buffer buffer))))

(ert-deftest overblock-test-a-strange-event-raises-nothing ()
  "An event that is not a click leaves point where it is, and raises nothing.
A command reads its event from `last-input-event', which can hold
anything, for example a bare cons such as `(1 . 0)', or a click whose
window slot is no window, as from a keyboard macro."
  (with-temp-buffer
    (insert "one\ntwo\n")
    (goto-char (point-min))
    (dolist (event (list nil ?a 'return "text" '(1 . 0)
                         (list 'mouse-1 (list '(1 . 0) 3))))
      (should-not (overblock-goto-event event))
      (should (= (point) (point-min))))))

(ert-deftest overblock-test-indent-leaves-the-indentation-in-view ()
  "With `:indent' a piece begins that many columns in, and the anchor paints nothing.
The indentation stays buffer text, with any indentation guide on it.
A line indented deeper is covered from that column on, and a shorter
line carries nothing."
  (with-temp-buffer
    (insert "    a\n        deeper\n  x\n    c\n")
    (put-text-property 1 5 'face 'bold)   ; as a guide would
    ;; From the first character after the indentation, as a doc string
    ;; starts at its quote.
    (let* ((block (overblock-show (+ (point-min) 4) (point-max)
                                  :over "A\nB\nC" :indent 4))
           (pieces (seq-remove (lambda (ov) (overlay-get ov 'overblock-cloak))
                               (overblock-get block :parts))))
      (should (= (length pieces) 3))
      (dolist (ov pieces)
        (goto-char (overlay-start ov))
        (should (= (current-column) 4))
        (should (equal (overlay-get ov 'face) overblock--plain)))
      ;; The short line has no piece and is cloaked.
      (goto-char (point-min)) (forward-line 2)
      (should (invisible-p (point)))
      ;; The anchor leaves the face of the indentation alone.
      (should-not (overlay-get block 'face))
      (should (eq (get-char-property 1 'face) 'bold)))
    ;; Two lines on one row: the second is padded to the same column,
    ;; and the piece starts after the indentation.
    (overblock-clear)
    (let* ((block (overblock-show (+ (point-min) 4) (+ (point-min) 5)
                                  :over "A\nB" :indent 4))
           (piece (car (overblock-get block :parts))))
      (should (= (overlay-start piece) (+ (point-min) 4)))
      (should (equal (overlay-get piece 'display) "A\n    B")))
    ;; Without `:indent' the anchor paints the whole region plain.
    (overblock-clear)
    (let ((block (overblock-show (point-min) (point-max) :over "A")))
      (should (equal (overlay-get block 'face) overblock--plain)))))

(ert-deftest overblock-test-indent-counts-columns-past-a-tab ()
  "`:indent' is a column, so a tab-indented line is covered from there.
Counted as characters, two tabs would leave source text in view."
  (with-temp-buffer
    (setq-local tab-width 8)
    (insert "\t\ta\n\t\tb\n")
    (let* ((block (overblock-show (+ (point-min) 2) (point-max)
                                  :over "A\nB" :indent 16))
           (pieces (seq-remove (lambda (ov) (overlay-get ov 'overblock-cloak))
                               (overblock-get block :parts))))
      (should (= (length pieces) 2))
      (dolist (ov pieces)
        (goto-char (overlay-start ov))
        (should (= (current-column) 16))))))

(ert-deftest overblock-test-a-piece-with-an-image-wears-its-own-face ()
  "A piece that rides a before-string carries `default' under its faces.
An overlay string without a face takes the face of the text under it,
such as the stipple of an indentation guide."
  (with-temp-buffer
    (insert "    a\n        b\n")
    (let* ((block (overblock-show (+ (point-min) 4) (point-max)
                                  :over (concat "A\n" overblock-test-common-image
                                                " b")
                                  :indent 4))
           (piece (seq-find (lambda (ov) (overlay-get ov 'before-string))
                            (overblock-get block :parts)))
           (text (overlay-get piece 'before-string)))
      (dotimes (i (length text))
        (should (memq 'default (ensure-list
                                (get-text-property i 'face text))))))))

(ert-deftest overblock-test-the-last-line-without-a-newline-is-cloaked ()
  "A short rendering at the end of a buffer with no final newline hides it all.
The last cloak reaches the last character."
  (with-temp-buffer
    (insert "one\ntwo\nthree")
    (overblock-show (point-min) (point-max) :over "A")
    (should (invisible-p (1- (point-max))))
    (should (invisible-p (- (point-max) 3)))))

(ert-deftest overblock-test-each-rendered-line-stands-on-its-source-row ()
  "A rendered line goes to the row it came from, past rows that render to nothing.
An underline has no line of its own in the rendering, so dealing the
lines in order put every line after it one row too high."
  (with-temp-buffer
    (insert "Title\n-----\ntext one\ntext two\n")
    (let* ((block (overblock-show (point-min) (point-max)
                                  :over "Title\ntext one\ntext two"))
           (shown (lambda (line)
                    (goto-char (point-min))
                    (forward-line (1- line))
                    (seq-some (lambda (ov) (overlay-get ov 'display))
                              (overlays-at (point))))))
      (should block)
      (should (equal (funcall shown 1) "Title"))
      (should (invisible-p (save-excursion (goto-char (point-min))
                                           (forward-line 1) (point))))
      (should (equal (funcall shown 3) "text one"))
      (should (equal (funcall shown 4) "text two")))))

(ert-deftest overblock-test-a-line-stays-on-its-row-when-a-later-row-looks-alike ()
  "Two source lines that begin alike keep their own rendered lines."
  (with-temp-buffer
    (insert "beta0 is one\nbeta0 is two\n")
    (overblock-show (point-min) (point-max)
                    :over "beta0 is one\nbeta0 is two")
    (goto-char (point-min))
    (should (equal (seq-some (lambda (ov) (overlay-get ov 'display))
                             (overlays-at (point)))
                   "beta0 is one"))))

(ert-deftest overblock-test-a-gap-of-the-rendering-stays-in-view ()
  "A blank source line that takes a blank line of the rendering stays in view."
  (with-temp-buffer
    (insert "alpha one\n\nbeta two\nbeta three\n")
    (overblock-show (point-min) (point-max)
                    :over "alpha one\n\nbeta two")
    ;; a cloak over the gap would begin at the newline before it
    (goto-char (point-min))
    (should-not (invisible-p (pos-eol)))))

(ert-deftest overblock-test-the-first-row-of-the-buffer-carries ()
  "The first row takes the first line, though it matches the row below.
A block that begins on its own row, as a doc string after a lone quote
line does, left that row empty and the rest one row off."
  (with-temp-buffer
    (insert "\"\"\"\nSummary here.\n")
    (overblock-show (point-min) (1- (point-max)) :over "Summary here.")
    (goto-char (point-min))
    (should (equal (seq-some (lambda (ov) (overlay-get ov 'display))
                             (overlays-at (point)))
                   "Summary here."))))

(ert-deftest overblock-test-a-long-wrap-keeps-to-its-row ()
  "A source line the renderer wraps into many lines keeps all of them."
  (with-temp-buffer
    (insert "apple\ncherry " (mapconcat #'number-to-string (number-sequence 0 20) " ")
            "\ndurian\nelder\n")
    (overblock-show (point-min) (point-max)
                    :over "apple\ncherry 0 1 2\n3 4 5\n6 7 8\n9 10 11\n12 13 14\ndurian\nelder")
    (goto-char (point-min))
    (forward-line 2)
    (should (equal (seq-some (lambda (ov) (overlay-get ov 'display))
                             (overlays-at (point)))
                   "durian"))))

(ert-deftest overblock-test-a-wrap-before-a-gap-keeps-to-its-row ()
  "A wrapped line before a blank source line keeps its continuation."
  (with-temp-buffer
    (insert "alpha one two three\n\nbeta four\n")
    (overblock-show (point-min) (point-max)
                    :over "alpha one\ntwo three\n\nbeta four")
    (goto-char (point-min))
    (forward-line 2)
    (should (equal (seq-some (lambda (ov) (overlay-get ov 'display))
                             (overlays-at (point)))
                   "beta four"))))

(ert-deftest overblock-test-a-block-drawn-unseen-is-drawn-again-when-seen ()
  "A block drawn with no window makes the next width check redraw.
A buffer in another tab during a theme change kept renderings built
for no width: the width it came back at was the width it had before."
  (with-temp-buffer
    (insert "one\n")
    (setq-local overblock--columns 80)
    (cl-letf (((symbol-function 'overblock-window-columns) #'ignore))
      (overblock-show (point-min) (point-max) :over "A"))
    (should-not overblock--columns)))

(ert-deftest overblock-test-rows-on-the-anchor-are-drawn-over-hl-line ()
  "Under rows on the anchor the newline outranks `hl-line', and the rows are plain.
`hl-line' painted a band as tall as a figure beside it."
  (with-temp-buffer
    (insert "code\n")
    (let* ((block (overblock-show (point-min) (point-max) :header "bar"
                                  :body overblock-test-common-image))
           (newline (overblock-get block :newline))
           (rows (overlay-get block 'after-string)))
      (should (> (overlay-get newline 'priority) -50))
      (let ((face (get-text-property (1- (length rows)) 'face rows)))
        (should (or (eq face overblock--plain) (memq overblock--plain face)))))))

(ert-deftest overblock-test-the-key-of-a-line-reads-past-its-padding ()
  "A padded table row keys on its cells, past any width of padding."
  (let ((overblock--keys (make-hash-table :test #'eq)))
    (should (equal (overblock--key (concat "| x" (make-string 40 ?\s) "| yyy |"))
                   "xyyy"))))

(ert-deftest overblock-test-a-fence-in-the-middle-of-a-file-carries-nothing ()
  "A block that begins on a fence line further down hides the fence.
The first row of a block carries only where it cannot be cloaked."
  (with-temp-buffer
    (insert "text\n```python\nx = 1\n```\n")
    (let ((beg (save-excursion (goto-char (point-min)) (forward-line 1) (point))))
      (overblock-show beg (point-max) :over "x = 1")
      (goto-char beg)
      (forward-line 1)
      (should (equal (seq-some (lambda (ov) (overlay-get ov 'display))
                               (overlays-at (point)))
                     "x = 1")))))

(ert-deftest overblock-test-a-short-block-of-wide-lines-is-aligned ()
  "Rows that all match keep their lines, however many lines wrapping adds.
Counted by lines, the continuations of two wrapped rows put the block
under the half, and its lines were dealt evenly instead."
  (with-temp-buffer
    (insert "alpha beta\ngamma delta\nepsilon\n")
    (overblock-show (point-min) (point-max)
                    :over "alpha\nx1\nx2\nx3\nx4\ngamma\nepsilon")
    (goto-char (point-min))
    (forward-line 1)
    (should (equal (seq-some (lambda (ov) (overlay-get ov 'display))
                             (overlays-at (point)))
                   "gamma"))))

(ert-deftest overblock-test-a-first-row-inside-a-line-carries ()
  "A block that begins inside a line keeps a line on its first row.
A cloak cannot begin inside a line, so the first row carries even where
its rendered line matches the row below."
  (with-temp-buffer
    (insert "x\n    \"\"\"\n    Summary here.\n")
    (let ((block (overblock-show 7 (point-max) :over "Summary here." :indent 4)))
      (should block)
      (should-not (seq-some (lambda (ov) (overlay-get ov 'overblock-cloak))
                            (overlays-in 7 10))))))

(ert-deftest overblock-test-a-whole-buffer-replace-holds-back-one-region ()
  "After an erase and a paste only the region point is in waits.
The region a rendering came off grew over the whole buffer, point could
not leave it, and no region rendered again."
  (with-temp-buffer
    (insert "one\n\ntwo\n")
    (overblock-live-start 'test-kind #'ignore t)
    (unwind-protect
        (progn
          (overblock--take-down (overblock-show 1 4 :kind 'test-kind :over "A"))
          (erase-buffer)
          (insert "one\n\ntwo\n")
          (goto-char 2)
          (should-not (overblock-live-wanted-p 1 4 'test-kind))
          (should (overblock-live-wanted-p 6 9 'test-kind)))
      (overblock-live-stop 'test-kind))))

(ert-deftest overblock-test-the-line-after-a-region-is-outside-it ()
  "Point at the start of the line after a region of whole lines is outside.
Point at the end of a region that ends inside a line is still in it."
  (with-temp-buffer
    (insert "one\ntwo\n")
    (overblock-live-start 'test-kind #'ignore)
    (unwind-protect
        (progn
          (goto-char 5)
          (should (overblock-live-wanted-p 1 5 'test-kind))
          (goto-char 4)
          (should-not (overblock-live-wanted-p 1 4 'test-kind)))
      (overblock-live-stop 'test-kind))))

(ert-deftest overblock-test-a-region-that-grew-renders-anew ()
  "A rendering of a region that grew is stale, and goes.
Text typed on the line after a region joins it.  The old rendering
does not cover the new text, so the region wants a new one."
  (with-temp-buffer
    (insert "> a\n")
    (overblock-live-start 'test-kind #'ignore)
    (unwind-protect
        (let ((old (overblock-show 1 5 :kind 'test-kind :over "a")))
          (goto-char (point-max))
          (insert "> b\n")
          (should (overlay-buffer old))
          (should (overblock-live-wanted-p 1 9 'test-kind))
          (should-not (overlay-buffer old))
          ;; A rendering of the region itself stays.
          (overblock-show 1 9 :kind 'test-kind :over "ab")
          (should-not (overblock-live-wanted-p 1 9 'test-kind))
          (should (overblock-in 1 9 'test-kind)))
      (overblock-live-stop 'test-kind))))

(ert-deftest overblock-test-a-narrowing-keeps-a-rendering ()
  "A narrowing that cuts a rendered block leaves the rendering alone.
The walk sees only the accessible part of the block, a shorter region."
  (with-temp-buffer
    (insert "> a\n> b\n")
    (overblock-live-start 'test-kind #'ignore)
    (unwind-protect
        (let ((block (overblock-show 1 9 :kind 'test-kind :over "ab")))
          (narrow-to-region 5 9)
          (goto-char (point-max))
          (should-not (overblock-live-wanted-p 5 9 'test-kind))
          (should (overlay-buffer block)))
      (overblock-live-stop 'test-kind))))

(ert-deftest overblock-test-a-cycle-renders-again-on-request ()
  "The render function of a cycle runs again when asked, and only its own."
  (with-temp-buffer
    (let ((calls 0))
      (setq-local overblock-live--specs
                  (list (list 'mine (lambda () (setq calls (1+ calls))) nil)))
      (overblock-live-render-again 'mine)
      (overblock-live-render-again 'other)
      (should (= calls 1)))))

(ert-deftest overblock-test-a-block-is-at-its-attached-bar ()
  "Point on an overlay a block has attached finds the block."
  (with-temp-buffer
    (insert "bar\nbody\n\nelse\n")
    (let* ((bar (make-overlay 1 4))
           (block (overblock-show 5 10 :kind 'mine :over "BODY"
                                  :attached (list bar))))
      (goto-char 2)
      (should (eq (overblock-at 'mine) block))
      (should (eq (overblock-at) block))
      (should-not (overblock-at 'other))
      (goto-char (point-max))
      (should-not (overblock-at)))))

(ert-deftest overblock-test-a-fold-takes-a-rendering-along ()
  "An outline fold hides a rendering over its source, not a block after it."
  (with-temp-buffer
    (insert "one\ntwo\nthree\n")
    (let ((over (overblock-show 1 4 :kind 'mine :over "ONE"))
          (after (overblock-show 5 8 :kind 'yours :body "result")))
      (overblock--fold 1 (point-max) t)
      (should (overblock-get over :hidden))
      (should-not (overblock-get over :parts))
      (should-not (overblock-get after :hidden))
      (overblock--fold 1 (point-max) nil)
      (should-not (overblock-get over :hidden))
      (should (overblock-get over :parts)))))

(defvar-local overblock-test--cache nil
  "The cache of `overblock-test-a-cache-holds-until-the-text-changes'.")

(ert-deftest overblock-test-a-cache-holds-until-the-text-changes ()
  "A cached answer is computed again after an edit or a new narrowing."
  (with-temp-buffer
    (insert "one\ntwo\n")
    (let* ((calls 0)
           (ask (lambda ()
                  (overblock-cached 'overblock-test--cache
                                    (lambda () (setq calls (1+ calls)))))))
      (funcall ask)
      (funcall ask)
      (should (= calls 1))
      (insert "three\n")
      (funcall ask)
      (should (= calls 2))
      (narrow-to-region 1 4)
      (funcall ask)
      (should (= calls 3))
      (widen)
      (should (= (funcall ask) 4)))))

(defvar overblock-test--option nil
  "An option for `overblock-test-a-set-option-takes-the-renderings-down'.")

(ert-deftest overblock-test-a-set-option-takes-the-renderings-down ()
  "Setting an option the renderings follow takes them down, to draw again."
  (with-temp-buffer
    (insert "one\ntwo\n")
    (setq-local overblock-live--specs (list (list 'mine #'ignore nil)))
    (overblock-show 1 4 :kind 'mine :over "ONE")
    (unwind-protect
        (progn
          (overblock-live-set-and-redraw 'overblock-test--option 'new)
          (should (eq overblock-test--option 'new))
          (should-not (overblock-in (point-min) (point-max) 'mine)))
      (overblock-live-stop 'mine))))

(ert-deftest overblock-test-each-live-cycle-says-what-point-reveals ()
  "A cycle that keeps the rendering at point does so for its own kind.
A notebook keeps its markdown cells, and a mode of the same buffer that
does not keep shows the source of the region at point."
  (with-temp-buffer
    (insert "one\n\ntwo\n")
    (overblock-live-start 'kept #'ignore t)
    (overblock-live-start 'shown #'ignore)
    (unwind-protect
        (progn
          (goto-char 2)
          (should (overblock-live-wanted-p 1 4 'kept))
          (should-not (overblock-live-wanted-p 1 4 'shown))
          (overblock-live-stop 'shown)
          (should (overblock-live-wanted-p 1 4 'kept)))
      (overblock-live-stop 'kept))))

(ert-deftest overblock-test-a-line-has-one-bar ()
  "A bar drawn on a line takes the place of every other bar there.
A bar of a block takes its block with it, and the source comes back."
  (with-temp-buffer
    (insert "head\nbody\n")
    (let* ((bar (overblock--bar-over 1 5))
           (block (overblock-show 6 10 :kind 'rendered :over "x"
                                  :attached (list bar))))
      (overblock--bar-draw bar 'rendered "" "old" "")
      (let ((new (overblock-bar-line 1 5 'source "" "new" "")))
        (should (equal (overblock-bars) (list new)))
        (should-not (overlay-buffer block))
        (should (eq new (overblock-bar-line 1 5 'source "" "again" "")))))))

(ert-deftest overblock-test-a-failing-cycle-leaves-the-others ()
  "One live cycle that signals does not stop the next one."
  (with-temp-buffer
    (let (ran fail)
      (overblock-live-start 'second (lambda () (setq ran t)))
      (overblock-live-start 'first (lambda () (when fail (error "Font lock"))))
      (setq ran nil fail t)
      (unwind-protect
          (cl-letf (((symbol-function 'run-with-idle-timer)
                     (lambda (_secs _repeat fn) (funcall fn) nil)))
            (overblock-live--settle)
            (should ran))
        (overblock-live-stop 'first)
        (overblock-live-stop 'second)))))

(ert-deftest overblock-test-a-heading-takes-no-gap-of-its-own ()
  "The blank line shr puts after a heading does not make its row taller."
  (with-temp-buffer
    (insert "Head\nbody one\nbody two\n")
    (overblock-show (point-min) (point-max)
                    :over "Head\n\nbody one\nbody two")
    (goto-char (point-min))
    (should (equal (seq-some (lambda (ov) (overlay-get ov 'display))
                             (overlays-at (point)))
                   "Head"))))

(ert-deftest overblock-test-a-missing-edit-mode-falls-back-to-text ()
  "The edit buffer opens in `text-mode' where its mode is not installed."
  (with-temp-buffer
    (insert "one\n")
    (overblock-edit-in-buffer
     1 4 (list :name " *overblock-test-edit*" :label "region"
               :mode 'overblock-test-no-mode
               :text #'buffer-substring-no-properties
               :put #'ignore))
    (unwind-protect
        (with-current-buffer " *overblock-test-edit*"
          (should (eq major-mode 'text-mode)))
      (kill-buffer " *overblock-test-edit*"))))

(ert-deftest overblock-test-text-typed-after-an-edited-region-stays ()
  "Text typed right after a region while its edit is open is not overwritten."
  (with-temp-buffer
    (insert "one\nREGION\nthree\n")
    (let ((source (current-buffer)))
      (overblock-edit-in-buffer
       5 11 (list :name " *overblock-test-edit*" :label "region"
                  :mode #'text-mode
                  :text #'buffer-substring-no-properties
                  :put (lambda (beg end text)
                         (goto-char beg)
                         (delete-region beg end)
                         (insert text))))
      (unwind-protect
          (progn
            (with-current-buffer source
              (goto-char 11)
              (insert " typed"))
            (erase-buffer)
            (insert "EDITED")
            (overblock-edit-commit)
            (with-current-buffer source
              (should (equal (buffer-string) "one\nEDITED typed\nthree\n"))))
        (when (get-buffer " *overblock-test-edit*")
          (kill-buffer " *overblock-test-edit*"))))))

(ert-deftest overblock-test-an-edit-lands-on-its-region-after-a-change ()
  "A commit writes over the region, though text was inserted above it.
The edit buffer holds the bounds of the region while the reader
writes, and the source can change meanwhile."
  (with-temp-buffer
    (insert "one\nREGION\nthree\n")
    (let ((source (current-buffer))
          put-at)
      (overblock-edit-in-buffer
       5 11 (list :name " *overblock-test-edit*" :label "region"
                  :mode #'text-mode
                  :text #'buffer-substring-no-properties
                  :put (lambda (beg end text)
                         (setq put-at (list (+ 0 beg) (+ 0 end)))
                         (goto-char beg)
                         (delete-region beg end)
                         (insert text))))
      (unwind-protect
          (progn
            (with-current-buffer source
              (goto-char (point-min))
              (insert "zero\n"))
            (erase-buffer)
            (insert "EDITED")
            (overblock-edit-commit)
            (should (equal put-at '(10 16)))
            (with-current-buffer source
              (should (equal (buffer-string) "zero\none\nEDITED\nthree\n"))))
        (when (get-buffer " *overblock-test-edit*")
          (kill-buffer " *overblock-test-edit*"))))))

(defvar overblock-test--mode nil "A stand-in for the variable of a minor mode.")

(ert-deftest overblock-test-only-in-leaves-a-mode-that-goes-off ()
  "A mode going off passes in any buffer; one going on is refused."
  (with-temp-buffer
    (let ((overblock-test--mode nil))
      (overblock-only-in 'overblock-test--mode 'python-mode)
      (setq overblock-test--mode t)
      (should-error (overblock-only-in 'overblock-test--mode 'python-mode)
                    :type 'user-error)
      (should-not overblock-test--mode))))

(provide 'overblock-test)
;;; overblock-test.el ends here
