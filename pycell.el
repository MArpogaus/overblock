;;; pycell.el --- Inline results for Python code cells -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Marcel Arpogaus

;; Author: Marcel Arpogaus <znepry.necbtnhf@tznvy.pbz>
;; Assisted-by: Claude:claude-opus-5
;; Assisted-by: Claude:claude-fable-5
;; Version: 0.1.3
;; Package-Requires: ((emacs "29.1") (code-cells "0.5") (comint-mime "0.4"))
;; Keywords: convenience, languages, tools
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

;; Notebook style results for Python code cells, built from python.el
;; and comint-mime alone -- no Jupyter kernel and no zmq module.
;;
;; Add `pycell-mode-maybe' to `code-cells-mode-hook' and the mode is
;; on in every Python buffer with cells.  Evaluating a cell sends it
;; to the inferior Python process as usual, so the REPL keeps the full
;; log.  While the cell runs, the result
;; grows below it: a header bar with a spinner, a stopwatch and buttons,
;; and the output as comint-mime rendered it, images included.
;;
;; Markdown cells, the `# %% [markdown]' ones that jupytext writes, are
;; rendered in place.  An external markdown command and shr produce
;; the text, which then hangs on the source lines it replaces, a piece
;; to a line, and the formulas that the converter passed through
;; become preview images through the formula machinery of Org mode.
;;
;; Rich output needs an IPython REPL, because comint-mime installs its
;; renderers there; a plain python3 shell yields text only.
;;
;; A result block is a display string on a single buffer line, and
;; Emacs cannot place point inside one.  The mouse wheel scrolls
;; through it a pixel at a time, but `next-line' and `previous-line'
;; cross it in one step, because a window can only start at a buffer
;; position.  A rendered markdown cell has lines of its own and moves
;; like ordinary text; it stands as tall as its source unless the
;; rendering is shorter, when the lines left over are hidden.

;;; Code:

(require 'code-cells)
(require 'comint-mime)
(require 'python)
(require 'ansi-color)
(require 'seq)
(require 'subr-x)

;; Org supplies the LaTeX preview machinery.  It is loaded on demand, in
;; `pycell--md-latex-image', so the symbols are declared rather than
;; required.
(declare-function org-create-formula-image "org"
                  (string tofile options buffer &optional type))
(declare-function org-combine-plists "org-macs" (&rest plists))
(defvar org-preview-latex-default-process)
(defvar org-preview-latex-process-alist)
(defvar org-format-latex-options)

;; shr parses the converter's HTML with this, and an Emacs built
;; without libxml2 does not have it; `pycell--md-program' answers nil
;; there and no markdown cell is rendered at all.  Declared so the
;; file still compiles on such a build.
(declare-function libxml-parse-html-region "xml.c"
                  (start end &optional base-url discard-comments))

(defgroup pycell nil "Inline results for Python code cells." :group 'python)

(defface pycell-header '((t :inherit code-cells-header-line))
  "Face for the header bar above a result.
It inherits the cell boundary face, so results match the cells.")

(defface pycell-output '((t :inherit shadow :extend t))
  "Face for the body of a result.")

(defface pycell-md-code '((t :inherit font-lock-constant-face))
  "Face for inline code in a rendered markdown cell.
shr draws code in a fixed pitch, and a rendered cell hangs on the
lines of a Python buffer, which is fixed pitch throughout: a pitch
says nothing there, so this face says it with a color.")

(defcustom pycell-markdown-command
  '("markdown" "pandoc" "markdown_py" "cmark" "cmark-gfm")
  "How to turn Markdown into HTML.
Either one shell command as a string, or a list of candidates, of
which the first one found in the variable `exec-path' is used.  The
program reads Markdown on standard input and writes HTML on standard
output, so arguments are allowed: \"pandoc -f gfm -t html\".

Markdown cells stay plain text while no candidate is installed.

Leave the math alone when choosing arguments.  Pandoc, for one, turns
simple formulas into text on its own and passes the rest through, and
`pycell--md-mathify' then makes preview images of what is left."
  :type '(choice (string :tag "Shell command")
                 (repeat (string :tag "Candidate command"))))

(defcustom pycell-max-lines 12
  "Number of result lines that show inline.
A result block is one buffer line however tall it is, so a long
result makes one long step for `next-line' and for the wheel.  Use
`pycell-pop-output' to see the whole of it.

Length is not what costs redisplay its time.  Measured in a 1000x700
window, forty lines of plain output scroll as cheaply as none, while
twelve lines full of face changes cost three times as much: the work
follows the number of face runs the text carries, not its size.

Width is another matter: see `pycell-max-line-length'."
  :type 'natnum)

(defcustom pycell-max-image-height 0.8
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
  :type 'number)

(defcustom pycell-max-line-length 2000
  "Number of characters of a result line that show inline.
Zero shows all of them.  A line longer than this is cut, and the cut
is marked with an ellipsis; `pycell-pop-output' has the whole of it.

One long line is one line, so `pycell-max-lines' does not bound it,
and a block laid out on every redisplay costs what it holds.
Measured in a 1200x800 window: a thousand characters on one line cost
1.4 milliseconds a wheel event, five thousand 2.4, twenty thousand
12.8, and a hundred thousand 226 — a fifth of a second an event, with
the wheel sending them by the dozen.  A `print' of a wide row, a long
list or a base64 blob is one such line."
  :type 'natnum)

;;;; Block display, shared by results and markdown cells

(defun pycell--overlays (beg end &optional prop)
  "Return the result overlays between BEG and END.
With PROP, return the overlays that carry PROP instead."
  (seq-filter (lambda (o) (overlay-get o (or prop 'pycell)))
              (overlays-in beg end)))

(defun pycell--delete (ov)
  "Delete the block overlay OV together with its helper overlays."
  (dolist (prop '(pycell-body pycell-head))
    (when-let* ((o (overlay-get ov prop)))
      (delete-overlay o)))
  (mapc #'delete-overlay (overlay-get ov 'pycell-parts))
  (delete-overlay ov))

(defun pycell-remove-overlays (&optional beg end prop)
  "Remove the result overlays between BEG and END.
BEG and END default to the whole buffer.  With PROP, remove the
overlays that carry PROP instead."
  (interactive)
  (without-restriction
    (mapc #'pycell--delete
          (pycell--overlays (or beg (point-min)) (or end (point-max))
                               prop))))

(defun pycell--faced (string face)
  "Add FACE below the faces STRING already carries.  Return STRING.
An overlay string without a face inherits one from the buffer text
next to it, so every block needs at least a base face."
  (add-face-text-property 0 (length string) face t string)
  string)

(defun pycell--glyph (&rest candidates)
  "Return the first of CANDIDATES this frame has a glyph for.
The last candidate is the answer when none of them has one.
`char-displayable-p' answers for the character set and not for the
font, so it says yes to characters that then draw as a hex box.

Every character of a candidate has to be there, not just the first:
several of them lead with a space, and a space is always available."
  (or (and (display-graphic-p)
           (seq-find (lambda (c)
                       (seq-every-p (lambda (ch) (internal-char-font nil ch))
                                    c))
                     candidates))
      (car (last candidates))))

(defun pycell--button (label help command)
  "Return LABEL as a button.
A left click calls COMMAND, and HELP becomes the tooltip."
  (propertize label 'mouse-face 'highlight 'help-echo help
              'keymap (let ((map (make-sparse-keymap)))
                        (define-key map [mouse-1] command)
                        map)))

(defun pycell--icons (&rest buttons)
  "Join the non-nil BUTTONS into the icon group of a header bar."
  (concat (string-join (delq nil buttons) "  ") " "))

(defun pycell--bar (left icons)
  "Return a header line: LEFT text, ICONS at the right window edge.
The alignment is pixel-exact: icon glyphs render wider than
`string-width' counts, and (N) in the display spec means N pixels.
A terminal gets one column of slack: a bar that runs into the last
column makes the line a continuation there, and the final icon wraps
onto a line of its own — measured at exactly one column, margins or
not."
  (pycell--faced
   (concat left
           (propertize " " 'display
                       `(space :align-to
                               (- right (,(+ (string-pixel-width
                                              (propertize icons 'face
                                                          'pycell-header))
                                             (if (display-graphic-p) 0 1))))))
           icons)
   'pycell-header))

(defun pycell--make-overlay (beg end)
  "Create the block overlay for the cell BEG..END.  Return it.
The overlay ends before the final newline: a window that starts at
the next boundary line then keeps the block out of view.  A second,
front-advancing overlay covers that newline and carries text bodies
\(see `pycell--attach'); rear-advance keeps both intact when the
user types at the end of the cell."
  (let* ((tip (if (and (eq (char-before end) ?\n) (> (1- end) beg))
                  (1- end)
                end))
         (ov (make-overlay beg tip nil t t)))
    (overlay-put ov 'evaporate t)
    (when (eq (char-after tip) ?\n)
      (overlay-put ov 'pycell-body
                   (let ((bov (make-overlay tip (1+ tip) nil t)))
                     (overlay-put bov 'evaporate t)
                     bov)))
    ov))

(defun pycell--attach (ov head body)
  "Attach HEAD and BODY as the block that OV shows.
HEAD goes into the after-string.  BODY without images goes onto the
newline after OV — real buffer text, which scrolls smoothly.  BODY
with images goes into the after-string: display properties do not
nest, a display string would swallow the images.

Only a real image sends it there.  Any display property would also
catch the raised text shr makes of a superscript, and inline math is
full of those; the string path costs five times as much per scroll
event."
  (let ((image (and body (pycell--image body)))
        (bov (overlay-get ov 'pycell-body)))
    (overlay-put ov 'after-string
                 (concat head (when (and body image) (concat "\n" body))))
    (when (and bov (overlay-buffer bov))
      ;; The string replaces the newline, so it restores the line
      ;; breaks on both of its sides.
      (overlay-put bov 'display
                   (and body (not image) (concat "\n" body "\n"))))))

(defun pycell--at-point (event prop)
  "Return the PROP overlay at point, or at the click in EVENT.
A click also selects its window and moves point there.  Anything else
leaves point where it is: the commands read EVENT from
`last-input-event', so it can be any event at all, and a
`switch-frame' is a cons whose start is a frame rather than a place."
  (when-let* (((consp event))
              (posn (event-start event))
              ((consp posn))
              (pos (posn-point posn)))
    (select-window (posn-window posn))
    (goto-char pos))
  (car (pycell--overlays (max (1- (point)) (point-min))
                            (min (1+ (point)) (point-max))
                            prop)))

;;;; Result blocks

(defun pycell--clean (text)
  "Strip prompts, Out[n] markers and outer whitespace from TEXT.
Whitespace only goes when it carries no display property: comint-mime
renders an image as one space with such a property, and `string-trim'
would delete it.  Call this in the shell buffer, where
`comint-prompt-regexp' has its value."
  (let ((rx (concat "\\(?:" comint-prompt-regexp "\\)")))
    ;; The (> ...) guard stops an endless loop if the prompt regexp
    ;; matches the empty string.  The last one keeps a figure: a cell
    ;; whose only output is one arrives as a space carrying it, which
    ;; the whitespace before the prompt would otherwise swallow, and
    ;; the block would come out empty.
    (while (and (string-match (concat "\\`[ \t\n]*" rx) text)
                (> (match-end 0) 0)
                (not (text-property-not-all 0 (match-end 0) 'display nil text)))
      (setq text (substring text (match-end 0))))
    (while (string-match (concat "\n[ \t]*" rx "[ \t\n]*\\'") text)
      (setq text (substring text 0 (match-beginning 0))))
    ;; Output that ends without a newline leaves the prompt on the
    ;; same line, where the rule above cannot see it: it looks for a
    ;; newline in front, and `comint-prompt-regexp' anchors to the
    ;; start of a line.  A plain python3 shell does that after a
    ;; `sys.stdout.write' without a newline.  Take that one off.
    (when (string-match (concat "\\(?:" (string-remove-prefix
                                         "^" comint-prompt-regexp)
                                "\\)[ \t]*\\'")
                        text)
      (setq text (substring text 0 (match-beginning 0)))))
  (setq text (replace-regexp-in-string "^Out\\[[0-9]+\\]: " "" text))
  (let* ((beg 0)
         (end (length text))
         (blank (lambda (i) (and (memq (aref text i) '(?\s ?\t ?\n ?\r))
                                 (not (get-text-property i 'display text))))))
    (while (and (< beg end) (funcall blank beg)) (setq beg (1+ beg)))
    (while (and (< beg end) (funcall blank (1- end))) (setq end (1- end)))
    ;; What the shell buffer shows is not what a copy of it shows.
    ;; comint-mime renders a DataFrame as a vtable, which aligns its
    ;; columns with pixel targets measured in that window and carries
    ;; the keymap of a live table.  In a result block the targets land
    ;; elsewhere, and no binding of that keymap can find a table.  So
    ;; the columns become literal spaces, and the promise of a click
    ;; goes: `pycell-pop-output' shows the whole output instead.
    (let ((copy (pycell--flattened (substring text beg end))))
      (remove-list-of-text-properties
       0 (length copy) '(keymap local-map mouse-face help-echo) copy)
      copy)))

(defun pycell--shorten (line)
  "Return LINE cut to `pycell-max-line-length' characters.
The cut is marked with an ellipsis.  See the option for what a line
left whole costs the scroller."
  (if (or (not (natnump pycell-max-line-length))
          (zerop pycell-max-line-length)
          (<= (length line) pycell-max-line-length))
      line
    (concat (substring line 0 pycell-max-line-length)
            (pycell--glyph "…" "..."))))

(defun pycell--fit (line)
  "Return LINE with its images capped to `pycell-max-image-height'.
The line kept for `pycell-pop-output' is not touched: this copies
before it caps."
  (if-let* (((numberp pycell-max-image-height))
            ((> pycell-max-image-height 0))
            ;; A cell can finish while its notebook is elsewhere —
            ;; sent and switched away from, or one of a whole run —
            ;; and no window at all would mean no cap and a block the
            ;; wheel cannot get past.  The selected window is a guess
            ;; at the size the notebook will have, and a guess that
            ;; comes out small only draws a smaller figure.
            (window (or (get-buffer-window nil t) (selected-window)))
            (limit (round (* pycell-max-image-height
                             (window-body-height window t))))
            ((> limit 0))
            ((pycell--image line)))
      (let ((line (copy-sequence line))
            (pos 0))
        (while (< pos (length line))
          (let ((next (or (next-single-property-change pos 'display line)
                          (length line)))
                (spec (get-text-property pos 'display line)))
            (when (and (eq (car-safe spec) 'image)
                       (not (plist-get (cdr spec) :max-height)))
              (put-text-property pos next 'display
                                 (cons 'image
                                       (plist-put (copy-sequence (cdr spec))
                                                  :max-height limit))
                                 line))
            (setq pos next)))
        line)
    line))

(defun pycell--body-lines (lines)
  "Return the leading LINES that show inline.
At most `pycell-max-lines', each cut to `pycell-max-line-length', and
nothing after the first line that carries an image it can draw: more
inline figures would grow the block, and the scroll jump with it,
without bound.  A display that shows no images has nothing to stop
for.  A line with an image on it is not cut, since the image may
sit past the cut; its images are capped to
`pycell-max-image-height' instead."
  (let (shown stop)
    (while (and lines (not stop) (< (length shown) pycell-max-lines))
      (let* ((l (pop lines))
             ;; Only where an image can be drawn.  A terminal shows
             ;; the space it rides on and nothing else, so stopping
             ;; there would cost the rest of the output and buy no
             ;; height back.
             (image (and (display-images-p) (pycell--image l))))
        (push (if image (pycell--fit l) (pycell--shorten l)) shown)
        (when image (setq stop t))))
    (nreverse shown)))

(defun pycell--header (folded total shown runtime running imagep)
  "Return the header bar of a result.
FOLDED is non-nil when only the header shows.
TOTAL and SHOWN count the lines and the inline subset.  RUNTIME is
the time in seconds since the cell started.  RUNNING is non-nil
while the cell runs.  IMAGEP marks a result with an image."
  (let* ((icons (pycell--icons
                 (when imagep
                   (pycell--button (pycell--glyph "⤓" "↧" "⇩" "↓")
                                      "Save the result's image to a file"
                                      #'pycell-save-image))
                 (when (> total 0)
                   (pycell--button (pycell--glyph "⧉" "❐" "▤" "≡")
                                      "Copy this result"
                                      #'pycell-copy-output))
                 (when (> total 0)
                   (pycell--button (pycell--glyph "↗" "⇗" "^")
                                      "Show this result in its own buffer"
                                      #'pycell-pop-output))
                 (pycell--button (pycell--glyph "✕" "×" "x")
                                    "Discard this result"
                                    #'pycell-discard-output)))
         ;; The stopwatch drives the spinner: one frame for each tick.
         (state (cond ((eq running t)
                       (let ((frames (pycell--glyph "⠋⠙⠹⠸⠼⠴⠦⠧⠇⠏" "|/-\\")))
                         (string ?\s (aref frames (mod (truncate runtime 0.2)
                                                       (length frames))))))
                      ((eq running 'died) (pycell--glyph " ⚠" " !"))
                      ;; A single line can still be tall: one image is
                      ;; one line, and that is the block worth folding.
                      ((> total 0)
                       (pycell--button (if folded
                                           (pycell--glyph " ▸" " ▶" " >")
                                         (pycell--glyph " ▾" " ▼" " v"))
                                          "Fold or unfold this result"
                                          #'pycell-toggle-output))
                      ((zerop total) (pycell--glyph " ✓" " √" " ."))
                      (t " ")))
         (label (cond ((> total 0)
                       (format "%d line%s%s" total (if (= total 1) "" "s")
                               (if (< shown total)
                                   (format ", showing %d" shown) "")))
                      ((not running) "no output")))
         (time (format "%.1fs" runtime)))
    (pycell--bar
     (concat state " " (string-join (delq nil (list label time)) " · "))
     icons)))

(defun pycell--update (ov)
  "Refresh the block of result overlay OV.
Call it with the overlay's buffer current."
  (pcase-let ((`(,folded ,text ,runtime ,running ,total)
               (overlay-get ov 'pycell)))
    (let* ((lines (unless (string-empty-p text) (split-string text "\n")))
           (shown (pycell--body-lines lines)))
      (pycell--attach
       ov
       (concat (unless (eq (char-before (overlay-end ov)) ?\n) "\n")
               (pycell--header folded (or total (length lines)) (length shown)
                                  runtime running
                                  (and lines (pycell--image text))))
       (when (and shown (not folded))
         (pycell--faced (string-join shown "\n") 'pycell-output))))))

(defun pycell--tab-filter (cmd)
  "Return CMD when point sits at the very end of a cell with a result."
  (and (eolp)
       (seq-some (lambda (o) (eq (point) (overlay-end o)))
                 (pycell--overlays (max (1- (point)) (point-min))
                                      (point)))
       cmd))

(defvar-keymap pycell-overlay-map
  :doc "Keymap inside a cell that shows a result."
  "TAB" '(menu-item "" pycell-toggle-output :filter pycell--tab-filter))

(defun pycell--show (beg end text runtime &optional running total)
  "Show TEXT as the result of the cell BEG..END.
RUNTIME is the time in seconds since the cell started.  RUNNING is
non-nil while the cell runs.  Empty TEXT gets a header that says
\"no output\", so the cell is recognizable as evaluated.  The fold
state of a replaced result is kept.
TOTAL is how many lines the cell has printed, for a running cell
whose TEXT is only the part that shows; without it the lines of TEXT
are counted."
  (let ((folded (when-let* ((old (car (pycell--overlays beg end))))
                  (car (overlay-get old 'pycell)))))
    (pycell-remove-overlays beg end)
    ;; The newline that ends the cell carries the result; give the
    ;; last cell of the buffer one.
    (when (and (= end (point-max)) (not (eq (char-before end) ?\n)))
      (save-excursion (goto-char end) (insert "\n")))
    (let ((ov (pycell--make-overlay beg end)))
      (overlay-put ov 'pycell (list folded text runtime running total))
      (overlay-put ov 'keymap pycell-overlay-map)
      (overlay-put ov 'modification-hooks
                   (list (lambda (o &rest _) (pycell--delete o))))
      (pycell--update ov)
      ov)))

(defun pycell--overlay (event)
  "Return the result overlay at point, or at the click in EVENT."
  (or (pycell--at-point event 'pycell)
      (car (apply #'pycell--overlays (code-cells--bounds)))
      (user-error "No result here")))

(defun pycell-toggle-output (&optional event)
  "Fold or unfold the result at point, or the one clicked in EVENT."
  (interactive (list last-input-event))
  (let* ((ov (pycell--overlay event))
         (state (overlay-get ov 'pycell)))
    (setcar state (not (car state)))
    (pycell--update ov)))

(defun pycell-discard-output (&optional event)
  "Discard the result at point, or the one clicked in EVENT."
  (interactive (list last-input-event))
  (pycell--delete (pycell--overlay event)))

(defun pycell--text (ov)
  "Return the text of the result overlay OV.
The one reader of that field: a record that each caller takes apart
by hand is a record that cannot change shape."
  (nth 1 (overlay-get ov 'pycell)))

(defun pycell--cell-buffer-name (kind position)
  "Return the name of the KIND buffer for the cell at POSITION.
KIND is the word after `pycell\=' in the name, or nil for a result.
The name carries the line of the cell, so each cell has a buffer of
its own and the buffers of two cells cannot collide."
  (format "*pycell%s: %s:%d*"
          (if kind (concat " " kind) "")
          (buffer-name)
          (line-number-at-pos position)))

(defun pycell-copy-output (&optional event)
  "Copy the result at point, or the one clicked in EVENT.
The copy keeps its text properties, so images survive a yank."
  (interactive (list last-input-event))
  (kill-new (pycell--text (pycell--overlay event)))
  (message "pycell: result copied"))

(defun pycell--image (text)
  "Return the first image that shows in TEXT, or nil."
  (let ((pos 0) img)
    (while (and (not img)
                (setq pos (text-property-not-all pos (length text)
                                                 'display nil text)))
      (let ((disp (get-text-property pos 'display text)))
        (if (eq (car-safe disp) 'image)
            (setq img disp)
          (setq pos (1+ pos)))))
    img))

(defun pycell-save-image (&optional event)
  "Save the first image of the result at point, or of the one in EVENT.
The file type comes from the image descriptor; `create-image' read
it from the data's magic bytes."
  (interactive (list last-input-event))
  (let* ((text (pycell--text (pycell--overlay event)))
         (img (or (pycell--image text)
                  (user-error "No image in this result")))
         (data (or (plist-get (cdr img) :data)
                   (user-error "This image carries no data")))
         (type (plist-get (cdr img) :type))
         (file (read-file-name
                "Save image to: " nil nil nil
                (format "figure.%s" (if (eq type 'jpeg) "jpg" type)))))
    (let ((coding-system-for-write 'no-conversion))
      (write-region data nil file))
    (message "pycell: image saved to %s" file)))

(defun pycell-pop-output (&optional event)
  "Show the result at point, or the one clicked in EVENT, in a buffer.
Each cell gets one buffer, so results are comparable side by side."
  (interactive (list last-input-event))
  (let* ((ov (pycell--overlay event))
         (text (pycell--text ov))
         (name (pycell--cell-buffer-name nil (overlay-start ov))))
    (with-current-buffer (get-buffer-create name)
      (special-mode)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert text))
      (goto-char (point-min))
      (pop-to-buffer (current-buffer)))))

;;;; Markdown cells

(defconst pycell--md-boundary
  "#+[[:blank:]]*%%+[[:blank:]]*\\[markdown\\]"
  "What marks a cell boundary line as a markdown cell.
Loose where `code-cells-boundary-regexp\=' is loose: any number of
comment characters, with or without a space, since VS Code and Spyder
write =#%% [markdown]= where jupytext writes =# %% [markdown]=.  A
tag list or a title may follow, as they may on a code cell.

The comment character is spelled out rather than asked of the syntax
table: this package reads Python and nothing else, and a caller with
another table current — a test, or a buffer whose mode has not been
set yet — would otherwise get a different answer.")

(defun pycell--md-head (pos)
  "Return the start of the =# %% [markdown]= line above POS, or nil.
A non-nil value marks POS as the body of a markdown cell."
  (save-excursion
    (goto-char pos)
    (forward-line -1)
    (and (looking-at-p pycell--md-boundary) (point))))

(defun pycell--fill-prop (string prop value)
  "Set PROP to VALUE where STRING does not carry PROP yet.
Return STRING.  shr gives a link its own keymap and help echo; a
plain `propertize' would clobber both, and the link would then run
this block's commands instead of following the URL."
  (let ((pos 0) (len (length string)))
    (while (< pos len)
      (let ((next (or (next-single-property-change pos prop string) len)))
        (unless (get-text-property pos prop string)
          (put-text-property pos next prop value string))
        (setq pos next))))
  string)

(defun pycell--fold (from to flag)
  "Hide the markdown blocks between FROM and TO when FLAG says so.
A block hangs on the newline that ends its cell, and
`outline-flag-region' stops one character short of that newline, so a
fold never covers it.  For a markdown cell the block is the content,
so it goes with the fold.  A result block stays: the fold hides the
code, and the block below keeps its own fold button.  Rather than
guess at the range that any one fold command uses, follow the call."
  ;; A fold that reaches the end of the buffer covers the newline a
  ;; result block hangs on; mid-buffer folds stop short of it.  Shrink
  ;; the fold there, so the last cell keeps its block like every other.
  (when flag
    (dolist (main (pycell--overlays from to 'pycell))
      (when-let* ((bov (overlay-get main 'pycell-body))
                  ((<= (overlay-end bov) to)))
        (dolist (o (overlays-in (overlay-start bov) (overlay-end bov)))
          (when (and (eq (overlay-get o 'invisible) 'outline)
                     (> (overlay-end o) (overlay-start bov)))
            (move-overlay o (overlay-start o)
                          (max (overlay-start o)
                               (overlay-start bov))))))))
  (dolist (ov (pycell--overlays from to 'pycell-md))
    (if-let* ((parts (overlay-get ov 'pycell-parts)))
        ;; The fold covers the very lines the pieces hang on, but it
        ;; stops one character short of the last newline, so the last
        ;; piece would stay on screen.  Hide them along.  The cloak
        ;; over the spare lines is invisible either way.
        (dolist (part parts)
          (overlay-put part 'invisible
                       (or flag (overlay-get part 'pycell-cloak))))
      ;; A cell with an image is one string below the cell, where no
      ;; fold reaches.  Put it aside and give it back.  The string is
      ;; the main overlay's; the body overlay carries the text of a
      ;; cell that has one, and the last cell of a buffer that ends
      ;; without a newline has none.
      (let ((bov (overlay-get ov 'pycell-body)))
        (if flag
            (unless (overlay-get ov 'pycell-folded)
              (overlay-put ov 'pycell-folded
                           (list (overlay-get ov 'after-string)
                                 (and bov (overlay-get bov 'display))
                                 (and bov (overlay-get bov 'after-string))))
              (overlay-put ov 'after-string nil)
              (when bov
                (overlay-put bov 'display nil)
                (overlay-put bov 'after-string nil)))
          (when-let* ((saved (overlay-get ov 'pycell-folded)))
            (overlay-put ov 'after-string (nth 0 saved))
            (when bov
              (overlay-put bov 'display (nth 1 saved))
              (overlay-put bov 'after-string (nth 2 saved)))
            (overlay-put ov 'pycell-folded nil)))))))

;; At load, not at activation: by the time this file loads, the user
;; has turned the mode on.  The advice is inert in buffers without
;; blocks, because it filters on the block properties.
(advice-add 'outline-flag-region :after #'pycell--fold)

(defun pycell--md-uncomment (text)
  "Strip the comment prefixes from the markdown cell TEXT."
  (replace-regexp-in-string "^# ?" "" text))

(defun pycell--md-comment (text)
  "Prefix each line of TEXT as a jupytext markdown comment."
  (mapconcat (lambda (l) (if (string-empty-p l) "#" (concat "# " l)))
             (split-string text "\n") "\n"))

(defvar pycell--md-latex-warned nil
  "Non-nil once a failed LaTeX preview was reported in this session.")

(defun pycell--md-latex-image (frag)
  "Return a preview image for the LaTeX fragment FRAG, or nil.
Org's formula machinery renders it.  The cache lives under ~/.cache,
keyed by content and theme color.  Org runs LaTeX in that directory
as well: a LaTeX in a container reaches the home directory, but not
the host's /tmp."
  (when (and (require 'org nil t) (fboundp 'org-create-formula-image))
    (let* ((fg (face-attribute 'default :foreground))
           (ext (or (plist-get
                     (cdr (assq org-preview-latex-default-process
                                org-preview-latex-process-alist))
                     :image-output-type)
                    "png"))
           (dir (expand-file-name
                 "pycell-math/" (or (getenv "XDG_CACHE_HOME") "~/.cache")))
           (file (expand-file-name
                  (concat (md5 (concat fg frag)) "." ext) dir)))
      (condition-case err
          (progn
            (unless (file-exists-p file)
              (make-directory dir t)
              (let ((temporary-file-directory dir))
                (org-create-formula-image
                 frag file
                 (org-combine-plists
                  org-format-latex-options
                  (list :foreground fg :background "Transparent"))
                 (current-buffer))))
            (create-image file nil nil :ascent 'center))
        ;; Report once: without a LaTeX installation, every fragment of
        ;; every cell would report the same thing.
        (error (unless pycell--md-latex-warned
                 (setq pycell--md-latex-warned t)
                 (message "pycell: no LaTeX preview (%s), formulas stay as text"
                          (error-message-string err)))
               nil)))))

(defconst pycell--md-math-regexp
  (rx (or (seq "$$" (+? anychar) "$$")
          (seq "$" (not (any "$" space)) (*? (not (any "$" "\n"))) "$")
          (seq "\\(" (+? anychar) "\\)")
          (seq "\\[" (+? anychar) "\\]")))
  "What a LaTeX fragment looks like in rendered markdown.
Most converters leave the dollar delimiters alone.  Pandoc renders
simple formulas as text and passes the rest through, either in dollars
or, when told to use MathJax, in parentheses and brackets.")

(defun pycell--md-mathify (text)
  "Replace the LaTeX fragments in TEXT with preview images.
Only fragments the converter left behind reach this function; a
fragment that fails to render here stays plain, and so does one
inside a table \(see `pycell--md-tag-table').

Only where the display can draw an image: a preview made in a
terminal cannot be seen."
  (if (not (display-images-p))
      text
    (replace-regexp-in-string
     pycell--md-math-regexp
     (lambda (frag)
       ;; `replace-regexp-in-string' uses the match data after the
       ;; replacement function returns; rendering must not touch it.
       (save-match-data
         (if-let* (((not (get-text-property 0 'pycell-md-table frag)))
                   (img (pycell--md-latex-image frag)))
             (propertize frag 'display img)
           frag)))
     text t t)))

(defun pycell--md-program ()
  "Return the markdown converter as a list of program and arguments.
The first candidate of `pycell-markdown-command' that is installed
wins; the result is nil when none of them is, and nil as well where
this Emacs cannot read the HTML that comes back: shr parses it with
`libxml-parse-html-region', which a build without libxml2 does not
have."
  (and (fboundp 'libxml-parse-html-region)
       (seq-some (lambda (command)
                   (let ((argv (split-string-shell-command command)))
                     (and (executable-find (car argv)) argv)))
                 (ensure-list pycell-markdown-command))))

(defconst pycell--md-marker "pycellcellbreak8f2b1c"
  "What stands between cells when they go to the converter together.
A word of its own in a paragraph of its own: every converter passes
that through as a paragraph, where anything with markup would be
reshaped into something else.")

(defun pycell--md-html (md)
  "Return the HTML `pycell-markdown-command' makes of MD."
  (let ((program (pycell--md-program)))
    (with-temp-buffer
      (insert md)
      ;; Send standard error nowhere: pandoc warns about math it cannot
      ;; convert, and the text would land in the HTML.
      (let ((status (apply #'call-process-region
                           (point-min) (point-max) (car program)
                           t '(t nil) nil (cdr program))))
        (unless (eq status 0)
          (error "%s exited with status %s" (car program) status)))
      (buffer-string))))

(defun pycell--md-htmls (texts)
  "Return the HTML of each of TEXTS, converted in one go.
Opening a notebook renders every markdown cell, and a converter
process costs more than the markdown: 44 milliseconds a cell with
`markdown_py\=', which is two seconds for fifty cells and nine for two
hundred.  One process for the buffer costs that once.

Nil when the marker does not come back once between every pair of
cells, or when a cell holds it already; the caller then asks for one
call per cell, as it always did."
  (unless (seq-some (lambda (text) (string-search pycell--md-marker text))
                    texts)
    (let* ((joined (string-join texts (format "\n\n%s\n\n"
                                             pycell--md-marker)))
           (pieces (split-string
                    (pycell--md-html joined)
                    (format "<p>[ \t\n]*%s[ \t\n]*</p>" pycell--md-marker))))
      (and (= (length pieces) (length texts)) pieces))))

(defun pycell--md-verbatim-math (md)
  "Return MD with its display-math blocks wrapped in <pre>.
A $$ block carries its line structure on purpose, one equation to a
line, and shr fills a paragraph: math that stays text comes back as
one rewrapped soup.  <pre> passes through every converter as raw HTML
and shr keeps its lines.

Whatever the display can draw, because a fragment stays text for more
reasons than that: a display can draw images and still have no LaTeX
to make one with, and a fragment LaTeX cannot compile stays text on
any display.  The wrapping costs a preview nothing, since the block is
matched across its lines and replaced whole."
  (replace-regexp-in-string
   "^\\$\\$\n\\(\\(?:.*\n\\)*?\\)\\$\\$$"
   "<pre>$$\n\\1$$</pre>"
   md))

(defun pycell--md-tag-th (dom)
  "Render the header cell DOM in bold.
shr has no function for a =th=, so a header cell reads like any other
row.  A table wants its header to stand out."
  (shr-fontize-dom dom 'bold))

(defun pycell--md-tag-code (dom)
  "Render the inline code DOM in `pycell-md-code'.
shr draws code in a fixed pitch face, which says nothing in a buffer
that is fixed pitch throughout: code came out as prose."
  (shr-fontize-dom dom 'pycell-md-code))

(defun pycell--md-tag-table (dom)
  "Render the table DOM and mark the text it covers.
`pycell--md-mathify\=' leaves marked text alone.  A table is padded to
the width of its text, and a preview image is never as wide as the
text it replaces, so a formula in a cell would pull the columns of its
row out of line."
  (let ((start (point)))
    (shr-tag-table dom)
    (put-text-property start (point) 'pycell-md-table t)))

(defconst pycell--md-rendering-functions
  (list (cons 'th #'pycell--md-tag-th)
        (cons 'code #'pycell--md-tag-code)
        (cons 'table #'pycell--md-tag-table))
  "How this package renders the tags shr renders differently.
See `shr-external-rendering-functions'.")

(defun pycell--md-flatten-alignment ()
  "Turn the `:align-to\=' spaces of this buffer into real spaces.
shr aligns table columns with `(space :align-to (N))\=' display specs,
and so does vtable, which is how comint-mime shows a DataFrame.  N
counts from the visual start of the line.  A rendered cell and a
result block are shown indented — line numbers, margins — so every
target left of the indent collapses to a single space and the columns
drift.  Literal padding aligns anywhere.  Left to right, so
`current-column\=' already sees the padding put in before it."
  (goto-char (point-min))
  (let (match)
    (while (setq match (text-property-search-forward 'display))
      (let ((spec (prop-match-value match)))
        (when (and (eq (car-safe spec) 'space)
                   (consp (plist-get (cdr spec) :align-to)))
          (let* ((target (car (plist-get (cdr spec) :align-to)))
                 (beg (prop-match-beginning match))
                 (end (prop-match-end match))
                 ;; The targets are pixels; a text terminal's pixel is
                 ;; a column, a graphic frame's is `frame-char-width'.
                 (pad (max 0 (- (round target (frame-char-width))
                                (save-excursion (goto-char beg)
                                                (current-column))))))
            (goto-char beg)
            (delete-region beg end)
            ;; Zero is a zero-width stretch: the column is already
            ;; there, and a forced space would push this row one past
            ;; its sisters.
            (insert (make-string pad ?\s))))))))

(defun pycell--flattened (text)
  "Return TEXT with its `:align-to\=' spaces as real spaces.
See `pycell--md-flatten-alignment' for why a copy needs them literal."
  (with-temp-buffer
    (insert text)
    (pycell--md-flatten-alignment)
    (buffer-string)))

(defun pycell--md-rendered (md &optional html)
  "Render the markdown MD to a propertized string.
`pycell-markdown-command\=' produces HTML, shr renders it, and LaTeX
fragments become preview images.  With HTML, that is rendered instead
and MD is not converted again: `pycell-md-render-all\=' converts the
whole buffer at once.

shr renders without its font arithmetic here: a cell\='s text hangs on
source lines at whatever indent the buffer wears, and only literal
columns survive a move.  The `:align-to\=' specs shr leaves behind are
flattened to real spaces for the same reason."
  (require 'shr)
  (let ((dom (with-temp-buffer
               (insert (or html (pycell--md-html (pycell--md-verbatim-math md))))
               (libxml-parse-html-region (point-min) (point-max))))
        (shr-use-fonts nil)
        (shr-external-rendering-functions
         (append pycell--md-rendering-functions
                 shr-external-rendering-functions)))
    (with-temp-buffer
      (shr-insert-document dom)
      (pycell--md-flatten-alignment)
      ;; Trim whole blank lines, never a first line's indent: the
      ;; columns are literal now, and a table that starts the cell
      ;; must keep the indent its sister rows have.
      (pycell--md-mathify
       (string-trim (buffer-string) "\\(?:[ \t]*\n\\)+")))))

(defvar-keymap pycell-md-map
  :doc "Keymap on rendered markdown cells."
  "RET" #'pycell-md-edit
  "<mouse-2>" #'pycell-md-edit
  "<mouse-1>" #'pycell-md-raw)

(defun pycell--md-cloak (beg end)
  "Return an overlay that hides BEG..END and stays hidden.
It carries `pycell-cloak' so that unfolding leaves it alone."
  (let ((ov (make-overlay beg end nil t)))
    (overlay-put ov 'evaporate t)
    (overlay-put ov 'invisible t)
    (overlay-put ov 'pycell-cloak t)
    ov))

(defun pycell--md-parts (beg end text)
  "Show TEXT over the source lines BEG..END, a piece to a line.
Return the overlays that carry the pieces.

Emacs lays a display string out whole on every redisplay, however
little of it the window shows.  One string for a whole cell therefore
costs as much per scroll event as the cell is tall, and a tall cell
is what makes a trackpad stutter.  A piece to a line costs only what
is on screen.  Hanging the text on the source lines also makes
folding work by itself, since a fold covers those very lines.

A piece covers the text of its line and leaves the newline alone, so
the buffer keeps its line structure and every line keeps its height.
A piece with an image in it cannot ride a `display' property, because
display properties do not nest and the image would be swallowed.  Such
a piece hides its line with a display string of nothing and rides the
after-string instead, which draws images.  The line keeps its own row
either way, which is what makes the cell scroll a line at a time.
A line without text cannot carry a piece — there is nothing to put
the display property on — and a cell rarely renders to as many lines
as it has anyway.  Those lines go under a cloak: an invisible run
from the newline of the line above to the end of the last line it
hides, which leaves that line's newline to end the line.  A cloak has
to start at the end of a visible line like that: `scroll-down'
answers a run that begins a line with a beginning-of-buffer error, in
the middle of the cell."
  (let* ((lines (split-string (string-trim text "\n" "\n") "\n"))
         (count (length lines))
         (rows (save-excursion
                 (goto-char beg)
                 (let (rows)
                   (while (< (point) end)
                     (push (cons (point) (min end (pos-eol))) rows)
                     (forward-line 1))
                   (nreverse rows))))
         (slots (max 1 (seq-count (lambda (row) (> (cdr row) (car row))) rows)))
         (taken 0)
         parts spare)
    (dolist (row rows)
      (let ((from (car row))
            (to (cdr row))
            chunk)
        (when (> to from)                    ; a line with text to cover
          (setq chunk (seq-subseq lines
                                  (/ (* taken count) slots)
                                  (/ (* (1+ taken) count) slots))
                taken (1+ taken)))
        (if (null chunk)
            ;; Nothing to show on this line.  Open a cloak at the
            ;; newline above, or leave the open one to grow.
            (unless spare (setq spare (1- from)))
          (when spare
            (push (pycell--md-cloak spare (1- from)) parts)
            (setq spare nil))
          (let ((ov (make-overlay from to nil t))
                (piece (string-join chunk "\n")))
            (overlay-put ov 'evaporate t)
            (overlay-put ov 'keymap pycell-md-map)
            (overlay-put ov 'help-echo (get-text-property 0 'help-echo text))
            (if (pycell--image piece)
                (progn (overlay-put ov 'display "")
                       (overlay-put ov 'after-string piece))
              (overlay-put ov 'display piece))
            (push ov parts)))))
    (when spare
      (push (pycell--md-cloak spare (1- end)) parts))
    (nreverse parts)))

(defun pycell--md-show (beg end &optional html)
  "Show the markdown cell body BEG..END rendered, in place.
With HTML, the cell is not sent to the converter again: it was
converted with the rest of the buffer.
The rendering hangs on the source lines themselves, a piece to a
line \(see `pycell--md-parts'), so the cell scrolls like ordinary
text and stands as tall as its source, unless the rendering is
shorter and the lines left over go under a cloak.  A cell that
renders to nothing is the exception: it falls back to the single
string a result block uses, and hides its source as one invisible
run.  That run must start at the end of a visible line —
`scroll-down' fails with a beginning-of-buffer error when it has to
move the window start over a run that begins at a line start — which
is why the =# %%= line stays visible.

Only the word =markdown= of the boundary line carries the header, so
=# %%= keeps the look of every other cell boundary and
`outline-minor-mode' still finds a heading line where it expects one."
  (let* ((start (1- beg))
         (_ (pycell-remove-overlays start end 'pycell-md))
         (help "RET/mouse-2: edit this markdown cell, mouse-1: show source")
         (text (pycell--fill-prop
                (pycell--fill-prop
                 (pycell--faced
                  (pycell--md-rendered
                   (pycell--md-uncomment
                    (buffer-substring-no-properties beg end))
                   html)
                  'default)
                 'keymap pycell-md-map)
                'help-echo help))
         (head (pycell--bar
                "markdown"
                (pycell--icons
                 (pycell--button
                  (pycell--glyph "↗" "⇗" "^")
                  "Edit this markdown cell in its own buffer"
                  #'pycell-md-edit)
                 (pycell--button (pycell--glyph "✕" "×" "x")
                                    "Show the plain source"
                                    #'pycell-md-raw))))
         (ov (pycell--make-overlay start end))
         (bov (overlay-get ov 'pycell-body))
         ;; The header covers the =markdown= word alone.  Its overlay
         ;; stops before the newline: the invisible run starts there,
         ;; and it would hide the bar with it.
         (hov (make-overlay (save-excursion
                              (goto-char (pycell--md-head beg))
                              (if (re-search-forward "\\[markdown\\]"
                                                     (pos-eol) t)
                                  (match-beginning 0)
                                (pos-eol)))
                            start)))
    ;; The property carries the body bounds: the overlay starts
    ;; above the cell and ends before the final newline.
    (overlay-put ov 'pycell-md
                 (cons (copy-marker beg) (copy-marker end t)))
    (overlay-put ov 'keymap pycell-md-map)
    (when bov
      (overlay-put bov 'keymap pycell-md-map)
      (overlay-put bov 'help-echo help))
    (overlay-put ov 'pycell-head hov)
    (overlay-put hov 'evaporate t)
    (overlay-put hov 'keymap pycell-md-map)
    ;; A click on the header resolves to this overlay, so it must
    ;; answer for the block: mark it, and point back at the main
    ;; overlay, which holds the cell bounds.
    (overlay-put hov 'pycell-md t)
    (overlay-put hov 'pycell-main ov)
    ;; A zero-width display property hides the word, and the bar draws
    ;; in its place as a string.  It has to be a string: a display
    ;; string ignores `(space :align-to (- right ...))', and the icons
    ;; then sit next to the label instead of at the window edge.
    (overlay-put hov 'display "")
    (overlay-put hov 'before-string head)
    (if-let* ((parts (pycell--md-parts beg end text)))
        (overlay-put ov 'pycell-parts parts)
      ;; A cell that renders to nothing has no pieces to hang anywhere.
      ;; It takes the single string a result block uses, with the source
      ;; hidden as one run.
      (overlay-put ov 'invisible t)
      (pycell--attach ov "" text))))

;;;###autoload
(defun pycell-md-render-all ()
  "Render every markdown cell in the buffer.
A markdown cell is one whose boundary line reads \"# %% [markdown]\"."
  (interactive)
  (let ((program (pycell--md-program))
        cells missed)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward (concat "^" pycell--md-boundary) nil t)
        (forward-line 1)
        (let ((beg (point))
              (end (if (re-search-forward code-cells-boundary-regexp nil t)
                       (pos-bol)
                     (point-max))))
          (when (< beg end)
            (if program (push (cons beg end) cells) (setq missed t)))
          (goto-char end))))
    (setq cells (nreverse cells))
    ;; One converter process for the buffer rather than one per cell.
    ;; It answers nil where the marker between cells did not survive,
    ;; and then each cell goes on its own, as before.
    (let ((htmls (and (cdr cells)
                      (pycell--md-htmls
                       (mapcar (lambda (cell)
                                 ;; The verbatim wrap belongs before the
                                 ;; converter, and this path converts
                                 ;; here rather than in the renderer.
                                 (pycell--md-verbatim-math
                                  (pycell--md-uncomment
                                   (buffer-substring-no-properties
                                    (car cell) (cdr cell)))))
                               cells)))))
      (dolist (cell cells)
        (pycell--md-show (car cell) (cdr cell) (pop htmls))))
    (when missed
      (message "pycell: %s, cells stay plain"
               (if (fboundp 'libxml-parse-html-region)
                   (format "no markdown converter found (%s)"
                           (string-join (ensure-list pycell-markdown-command)
                                        ", "))
                 "this Emacs was built without libxml, which shr reads \
the converter\'s HTML with")))))

(defun pycell-md-unrender ()
  "Show all markdown cells as their plain source again."
  (interactive)
  (pycell-remove-overlays (point-min) (point-max) 'pycell-md))

(defun pycell--md-at (event)
  "Return the markdown block at point, or at the click in EVENT.
Always the main overlay: a click on the header bar lands on the
small overlay that draws it."
  (let ((ov (or (pycell--at-point event 'pycell-md)
                (user-error "No rendered markdown cell here"))))
    (or (overlay-get ov 'pycell-main) ov)))

(defun pycell-md-raw (&optional event)
  "Show the markdown cell at point, or the one in EVENT, as plain source.
The cell is then editable in place; evaluate it, or run
`pycell-md-render-all', to render it again."
  (interactive (list last-input-event))
  (pycell--delete (pycell--md-at event)))

(defvar-local pycell--md-source nil
  "Markdown cell (BUFFER BEG END) that this edit buffer feeds.")

(define-minor-mode pycell-md-edit-mode
  "Edit a markdown cell, as `org-edit-special' edits a source block."
  ;; The :lighter also keeps the body out of the deprecated
  ;; positional INIT-VALUE argument.
  :lighter " cell-edit"
  :keymap (define-keymap
            "C-c C-c" #'pycell-md-commit
            "C-c C-k" #'pycell-md-abort))

(defun pycell-md-edit (&optional event)
  "Edit the markdown cell at point, or the one clicked in EVENT.
The body opens in its own buffer, without the comment prefixes, in
`markdown-mode' when that is installed.
\\<pycell-md-edit-mode-map>\\[pycell-md-commit] puts it back \
and renders it; \\[pycell-md-abort] discards the edit."
  (interactive (list last-input-event))
  (pcase-let* ((ov (pycell--md-at event))
               (`(,beg . ,end) (overlay-get ov 'pycell-md))
               (src (current-buffer))
               (md (pycell--md-uncomment
                    (buffer-substring-no-properties beg end)))
               ;; A buffer per cell, as `pycell-pop-output' does with
               ;; results: one buffer for the whole file would put the
               ;; text of the cell opened second over the text of the
               ;; cell opened first, and an hour of writing with it.
               (buf (get-buffer-create
                     (pycell--cell-buffer-name "md" beg))))
    (with-current-buffer buf
      ;; An edit of this very cell that is already under way is the
      ;; edit the reader wants back, not a fresh copy of what the file
      ;; still says.  Org answers the same, by asking.
      (unless (and pycell-md-edit-mode (buffer-modified-p))
        (erase-buffer)
        (insert md)
        (if (fboundp 'markdown-mode) (markdown-mode) (text-mode))
        (pycell-md-edit-mode)
        ;; The keys come from the keymap, so the hint stays true when
        ;; the bindings or the prefix change.
        (setq header-line-format
              (substitute-command-keys
               " Markdown cell — \\[pycell-md-commit] applies, \
  \\[pycell-md-abort] discards"))
        (set-buffer-modified-p nil))
      (setq pycell--md-source (list src beg end)))
    (pop-to-buffer buf)))

(defun pycell-md-commit ()
  "Put the edited markdown back into its cell and render it."
  (interactive)
  (pcase-let ((`(,src ,beg ,end) pycell--md-source)
              (md (string-trim-right (buffer-string))))
    (unless (and src (buffer-live-p src))
      (user-error "The cell's buffer is gone"))
    (with-current-buffer src
      (save-excursion
        ;; The cell reaches to the next boundary line, so it holds the
        ;; blank line that jupytext writes between cells.  Put back
        ;; what stood after the body rather than one newline, or
        ;; committing an edit that changed nothing would still close
        ;; the gap.
        (let ((tail (buffer-substring-no-properties
                     (save-excursion
                       (goto-char end)
                       (skip-chars-backward " \t\n" beg)
                       (point))
                     end)))
          (goto-char beg)
          (delete-region beg end)
          ;; An empty cell has no line to comment: `pycell--md-comment'
          ;; would write a bare # where the author left nothing, and a
          ;; commit that changed nothing would change the file.
          (insert (if (string-empty-p md) "" (pycell--md-comment md))
                  tail)))
      (pycell--md-show beg end))
    (quit-window t)))

(defun pycell-md-abort ()
  "Discard the markdown edit."
  (interactive)
  (quit-window t))

;;;; Running cells

(defvar pycell--queue nil
  "Start markers of the cells that `pycell-restart-and-run-all' still runs.")

(defvar-local pycell--run nil
  "State of the cell that runs in this Python shell, or nil.
A list (FROM BEG END TAIL START TIMER HEAD COUNT): FROM marks where
the output starts here, BEG and END delimit the cell in its own
buffer, TAIL accumulates recent output for the prompt detection,
START is the `float-time' of the send and TIMER the ticker.  The last
two belong to the live mirror: HEAD is the part of the output the
block shows, once it can no longer change, and COUNT is (POSITION
. LINES) counted up to POSITION, so a tick reads only what arrived
since the one before it.")

(defvar-local pycell--cold-cell nil
  "Cell (BEG . END markers) that waits for this shell's first prompt.")

(defun pycell--output-so-far (from)
  "Return the running cell's output after FROM, cleaned.
An incomplete escape sequence at the end is dropped: comint-mime
renders it only when it is complete."
  (pycell--clean
   (replace-regexp-in-string
    "\e\\][^\e]*\\'" "" (buffer-substring from (point-max)))))

(defun pycell--head (from)
  "Return as much of the output after FROM as the block can show.
`pycell--body-lines' takes the first `pycell-max-lines' lines and
stops, so a tick has no reason to read — or clean — everything the
cell has printed.  Once those lines are all in, the text cannot
change anymore and is kept, and the ticks after that read nothing.
Nothing is kept while the head is empty: an escape sequence that has
not arrived in full swallows everything after it until it does, and a
cell whose first lines are still on their way has more to come."
  (or (nth 6 pycell--run)
      (let* ((limit (save-excursion
                      (goto-char from)
                      (forward-line (+ pycell-max-lines 4))
                      (point)))
             (text (pycell--clean
                    (replace-regexp-in-string
                     "\e\\][^\e]*\\'" "" (buffer-substring from limit)))))
        (when (and (< limit (point-max))
                   (not (string-empty-p text)))
          (setf (nth 6 pycell--run) text))
        text)))

(defun pycell--total (from)
  "Return the number of lines the running cell has printed after FROM.
Counted where they arrive: reading the whole output again is a pass
over everything printed so far, and a cell that prints a lot pays
that pass five times a second.  Leading blank lines go, as
`pycell--clean' drops them, so the count agrees with the one the
finished cell shows."
  (let* ((state (or (nth 7 pycell--run)
                    (cons (save-excursion
                            (goto-char from)
                            (skip-chars-forward " \t\n")
                            (point-marker))
                          0)))
         (count (cdr state)))
    (save-excursion
      (goto-char (car state))
      (while (search-forward "\n" nil t) (setq count (1+ count)))
      (setf (nth 7 pycell--run) (cons (point-marker) count)))
    ;; A line that has not ended yet is a line all the same.
    (if (and (> (point-max) (marker-position from))
             (not (eq (char-before (point-max)) ?\n)))
        (1+ count)
      count)))

(defun pycell--end (text &optional died)
  "End the running cell and show TEXT as its final result.
The one exit for every way a cell ends; DIED marks abnormal ends.
Call this in the Python shell buffer."
  (pcase-let ((`(,_ ,beg ,fin ,_ ,start ,timer) pycell--run))
    (setq pycell--run nil)
    (cancel-timer timer)
    (when died (setq pycell--queue nil))
    (when (buffer-live-p (marker-buffer beg))
      (with-current-buffer (marker-buffer beg)
        (pycell--show beg fin text (- (float-time) start)
                         (and died 'died))))
    ;; Keep `pycell-restart-and-run-all' going, or stop on error.
    (when pycell--queue
      (if (string-match-p "Traceback (most recent call last)" text)
          (progn (setq pycell--queue nil)
                 (message "pycell: stopped at error"))
        (pycell--run-next)))))

(defun pycell--abort ()
  "End the running cell abnormally — its prompt will never return.
A death notice, with the exit status when one is available, follows
the output received so far.  This covers a dead interpreter (the
ticker finds it), a killed shell buffer and a shell restart, which
reinitializes the major mode — hence also on `kill-buffer-hook' and
`change-major-mode-hook' in the Python shell."
  (when pycell--run
    (let* ((proc (get-buffer-process (current-buffer)))
           (out (pycell--output-so-far (car pycell--run)))
           (msg (propertize
                 (format "Process unexpectedly died%s"
                         (if proc
                             (format " (%s %s)" (process-status proc)
                                     (process-exit-status proc))
                           ""))
                 'face 'error)))
      (pycell--end (if (string-empty-p out) msg (concat out "\n" msg))
                      t))))

(defun pycell--tick (buf timer)
  "Mirror the running cell's output and stopwatch into its overlay.
TIMER runs this every 0.2s for the Python shell BUF.  It cancels
itself when nothing runs there anymore."
  (if (not (and (buffer-live-p buf)
                (buffer-local-value 'pycell--run buf)))
      (cancel-timer timer)
    (with-current-buffer buf
      (if (not (process-live-p (get-buffer-process buf)))
          (pycell--abort)
        (pcase-let ((`(,from ,beg ,fin ,_ ,start ,_) pycell--run))
          (let* ((text (pycell--head from))
                 (total (if (string-empty-p text) 0 (pycell--total from))))
            (when (buffer-live-p (marker-buffer beg))
              (with-current-buffer (marker-buffer beg)
                (pycell--show beg fin text (- (float-time) start)
                                 t total)))))))))

(defun pycell--filter (output)
  "Watch OUTPUT for the closing prompt, then end the running cell.
The filter stays on `comint-output-filter-functions' and idles while
no cell runs; the live mirroring is the ticker's job."
  (when pycell--run
    ;; A chunk boundary can split the prompt, so match a capped tail;
    ;; `ansi-color-filter-apply' drops the escape sequences.
    (let ((tail (concat (nth 3 pycell--run)
                        (ansi-color-filter-apply output))))
      (setf (nth 3 pycell--run)
            (substring tail (max 0 (- (length tail) 256))))
      (when (python-shell-comint-end-of-output-p tail)
        ;; Copy to the end of the buffer and let `pycell--clean' take
        ;; the prompt off.  `comint-last-prompt' cannot serve as the
        ;; end: comint calls the last line without a newline a prompt,
        ;; so a chunk that arrives split leaves the marker inside the
        ;; output, and everything after it would be dropped without a
        ;; word.
        (pycell--end
         (pycell--clean
          (buffer-substring (car pycell--run) (point-max))))))))

(defun pycell--ipython-syntax-p (beg end)
  "Return non-nil when BEG..END holds syntax that only IPython reads.
A magic, a shell escape or a help request: a line that begins with %
or !, or one that ends in ?.

Only where the character means that, which is why this reads the
buffer instead of the text.  A continuation line inside brackets may
begin with a modulo, a comment may ask a question, and a docstring
may do either; there the character is Python's own and the cell has
to keep to the ordinary road.  A shell without IPython would answer
the other one with a NameError, so a cell of plain Python must never
be sent down it."
  (save-excursion
    (goto-char beg)
    (catch 'found
      (while (< (point) end)
        (let ((state (syntax-ppss (point)))
              (eol (min end (pos-eol))))
          ;; the line starts as code, not inside a string, a comment
          ;; or a bracket left open above
          (when (and (not (nth 3 state)) (not (nth 4 state))
                     (zerop (nth 0 state)))
            (when (looking-at-p "[ \t]*[%!]")
              (throw 'found t))
            (let ((last (save-excursion
                          (goto-char eol)
                          (skip-chars-backward " \t" (point))
                          (point))))
              (when (and (eq (char-before last) ??)
                         (let ((s (syntax-ppss (1- last))))
                           (and (not (nth 3 s)) (not (nth 4 s)))))
                (throw 'found t)))))
        (forward-line 1))
      nil)))

(defun pycell--send-to-ipython (proc code)
  "Send CODE to PROC the way typing it would.
`python-shell-send-region' wraps the cell in a compile call, so the
interpreter reads it as plain Python and IPython's own reader, which
is what turns %, ! and ? into calls, never sees it.  Handing the
source to `run_cell' puts the reader back in.  It travels base64
encoded, which keeps the cell's own quotes and newlines out of the
way, and the trailing None stops the result object from showing up as
the value of the cell.

Tracebacks then count lines from the top of the cell rather than the
top of the file, and a shell without IPython answers that it does not
know `get_ipython'."
  (python-shell-send-string
   (format "get_ipython().run_cell(__import__(\"base64\")\
.b64decode(\"%s\").decode(\"utf-8\"))\nNone\n"
           (base64-encode-string (encode-coding-string code 'utf-8) t))
   proc))

(defun pycell--send (proc start end)
  "Send START..END to PROC as the running cell and track it.
Call this with the cell's buffer current."
  (let ((beg (copy-marker start))
        (fin (copy-marker end t)))
    (with-current-buffer (process-buffer proc)
      (when pycell--run
        (user-error "The Python shell is still busy with another cell"))
      ;; All idempotent: the filter idles while no cell runs, the
      ;; other two catch the shell going away under a running cell.
      ;; comint-mime renders from the same hook; because our filter
      ;; appends, the copied region already carries the images.
      (add-hook 'comint-output-filter-functions #'pycell--filter t t)
      (add-hook 'kill-buffer-hook #'pycell--abort nil t)
      (add-hook 'change-major-mode-hook #'pycell--abort nil t)
      ;; The ticker receives itself, so it can always self-cancel.
      (let ((timer (run-with-timer 0.2 0.2 #'ignore)))
        (timer-set-function timer #'pycell--tick
                            (list (current-buffer) timer))
        (setq pycell--run (list (copy-marker (process-mark proc))
                                   beg fin "" (float-time) timer nil nil))))
    (pycell--show beg fin "" 0.0 t)
    (if (pycell--ipython-syntax-p beg fin)
        (pycell--send-to-ipython
         proc (buffer-substring-no-properties beg fin))
      ;; `python-shell-send-region' pads the code, so traceback line
      ;; numbers match the buffer.
      (python-shell-send-region beg fin))))

(defun pycell--run-cold ()
  "Evaluate the cell that waited for the interpreter.
Unlike `pycell--run-next' this does not move point: the command
that caused the cold start may have moved it on already."
  (remove-hook 'python-shell-first-prompt-hook #'pycell--run-cold t)
  (pcase-let ((`(,beg . ,end) pycell--cold-cell)
              (proc (get-buffer-process (current-buffer))))
    (setq pycell--cold-cell nil)
    (when (and beg (buffer-live-p (marker-buffer beg)))
      (with-current-buffer (marker-buffer beg)
        (pycell--send proc beg end)))))

(defun pycell-eval-region (start end)
  "Evaluate START..END as a cell and mirror the output below it.
This matches the calling convention of
`code-cells-eval-region-commands'.  A markdown cell renders instead.
Without an interpreter, one starts and the cell follows on its
first prompt."
  ;; ponytail: one cell at a time; add a queue if that ever chafes.
  (if (pycell--md-head start)
      ;; Keep a running restart-and-run-all chain going — no prompt
      ;; will arrive to do it.
      (progn
        (pycell--md-show start end)
        ;; Redisplay pushes a point that the block just made
        ;; invisible out of it, and upwards; put it below instead.
        (when (<= (1- start) (point) end)
          (goto-char end))
        (when pycell--queue (pycell--run-next)))
    (if-let* ((proc (python-shell-get-process)))
        (pycell--send proc start end)
      ;; Mark the cell here, while its buffer is still current:
      ;; `copy-marker' on a number answers for whatever buffer that
      ;; is, and below it is the shell's.  Markers into the shell
      ;; would send its start-up banner as the cell.
      (let ((cell (cons (copy-marker start) (copy-marker end t))))
        (run-python)
        (with-current-buffer
            (process-buffer (python-shell-get-process-or-error))
          (setq pycell--cold-cell cell)
          (add-hook 'python-shell-first-prompt-hook
                    #'pycell--run-cold 90 t)))
      (message "pycell: starting the interpreter…"))))

(defun pycell-interrupt ()
  "Send a KeyboardInterrupt to the cell's Python process.
The interrupted cell ends normally: IPython prints the traceback
and prompts again."
  (interactive)
  (interrupt-process (python-shell-get-process-or-error)))

(defun pycell-restart ()
  "Restart the Python interpreter and discard all results."
  (interactive)
  (pycell-remove-overlays)
  (setq pycell--queue nil)
  (if-let* ((proc (python-shell-get-process)))
      (progn
        ;; Drop the run silently: this command discards results, it
        ;; does not mark the running cell as aborted.  The orphaned
        ;; ticker cancels itself on its next tick.
        (with-current-buffer (process-buffer proc)
          (setq pycell--run nil))
        (python-shell-restart))
    (run-python)))

(defun pycell--run-next ()
  "Evaluate the cell at the head of `pycell--queue'.
Point follows, so the run-all pass is visible."
  (remove-hook 'python-shell-first-prompt-hook #'pycell--run-next t)
  (when-let* ((m (pop pycell--queue)))
    (if (not (buffer-live-p (marker-buffer m)))
        (setq pycell--queue nil)
      (with-current-buffer (marker-buffer m)
        (goto-char m)
        (pcase-let ((`(,beg ,end) (code-cells--bounds nil nil t)))
          (pycell-eval-region beg end))))))

(defun pycell-stop ()
  "Stop `pycell-restart-and-run-all' after the current cell."
  (interactive)
  (setq pycell--queue nil)
  (message "pycell: run all stopped"))

(defvar-keymap pycell-stop-map
  :doc "Transient keymap, active while all cells run."
  "<escape>" #'pycell-stop)

(defun pycell-restart-and-run-all ()
  "Restart the Python interpreter, then evaluate every cell in order.
The pass stops at the first error, or on \
\\<pycell-stop-map>\\[pycell-stop]."
  (interactive)
  (pycell-restart)
  (setq pycell--queue
        (save-excursion
          (goto-char (point-min))
          (let ((cells (unless (looking-at-p code-cells-boundary-regexp)
                         (list (point-min-marker)))))
            (while (re-search-forward code-cells-boundary-regexp nil t)
              (push (copy-marker (pos-bol)) cells))
            (nreverse cells))))
  ;; Evaluation may only start once the fresh interpreter prompted —
  ;; and after comint-mime's setup, which runs off the same hook;
  ;; hence the depth.  `pycell--end' chains the remaining cells.
  (with-current-buffer (process-buffer (python-shell-get-process-or-error))
    (add-hook 'python-shell-first-prompt-hook #'pycell--run-next 90 t))
  (set-transient-map pycell-stop-map (lambda () pycell--queue) nil
                     "Evaluating all cells, %k to stop"))

;;;###autoload
(define-minor-mode pycell-mode
  "Show Python cell results, and markdown cells, inline.
While the mode is on, cell evaluation goes through
`pycell-eval-region'.  Turn it off to remove all blocks and to
get plain `python-shell-send-region' back.  The bindings live in
the code-cells maps."
  ;; The :lighter also keeps the body out of the deprecated
  ;; positional INIT-VALUE argument.
  :lighter " pycell"
  (if pycell-mode
      (pycell-md-render-all)
    (pycell-remove-overlays)
    (pycell-md-unrender)))

;;;###autoload
(defun pycell-mode-maybe ()
  "Enable `pycell-mode' in Python cell buffers.
Made for `code-cells-mode-hook', where your configuration adds it:

  (add-hook \='code-cells-mode-hook #\='pycell-mode-maybe)

The package installs no hook itself: installing it must not change
how Emacs behaves."
  (when (derived-mode-p 'python-base-mode)
    (pycell-mode)))

;; Keyed on the minor mode: with it off, code-cells falls through to its
;; stock python entry, `python-shell-send-region'.
(setf (alist-get 'pycell-mode code-cells-eval-region-commands)
      #'pycell-eval-region)

(provide 'pycell)
;;; pycell.el ends here
