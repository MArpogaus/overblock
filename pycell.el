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
;; What draws a block on the screen is not here: `overblock' puts text
;; over a region of a buffer with a header above it,
;; `overblock-md' turns markdown into a string it can show,
;; and `overblock-repl' cuts the output of a shell loose from that
;; shell.  What is here is the part that knows about Python: the cells,
;; the process, and the commands.
;;
;; A result block is a display string on a single buffer line, and
;; Emacs cannot place point inside one.  The mouse wheel scrolls
;; through it a pixel at a time, but `next-line' and `previous-line'
;; cross it in one step, because a window can only start at a buffer
;; position.  A rendered markdown cell has lines of its own and moves
;; like ordinary text; it stands as tall as its source unless the
;; rendering is shorter, when the lines left over are hidden.

;;; Code:

(require 'overblock)
(require 'overblock-md)
(require 'overblock-repl)
(require 'code-cells)
(require 'outline)
(require 'comint-mime)
(require 'python)
(require 'ansi-color)
(require 'map)
(require 'seq)
(require 'subr-x)

(defgroup pycell nil "Inline results for Python code cells." :group 'python)

(defface pycell-header '((t :inherit code-cells-header-line))
  "Face for the header bar above a result.
It inherits the cell boundary face, so results match the cells.")

(defface pycell-output '((t :inherit shadow :extend t))
  "Face for the body of a result.")

(defun pycell--set-buttons (symbol value)
  "Set SYMBOL to VALUE, and draw the headers of every notebook again.
The `:set\\=' of the button options.  A change to one of them showed up
only when something else drew a bar again — a window changing width, or
the file opened afresh — so customizing the buttons of a notebook that
was already open appeared to do nothing at all."
  (set-default symbol value)
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (when (bound-and-true-p pycell-mode)
        (mapc #'pycell--bar-redraw (overblock-bars))
        (dolist (block (overblock-in (point-min) (point-max) 'result))
          (pycell--update block))))))

(defcustom pycell-result-buttons
  '((move-up ("󰅃" "⌃" "u") "Move this cell up" pycell-move-cell-up t)
    (move-down ("󰅀" "⌄" "d") "Move this cell down" pycell-move-cell-down t)
    (save-image ("󰮏" "↧" "↓") "Save the result's image to a file"
                pycell-save-image image)
    (copy ("󰄷" "◫" "≡") "Copy this result" pycell-copy-output lines)
    (pop ("󱦴" "↗" "^") "Show this result in its own buffer"
         pycell-pop-output lines)
    (discard ("󰅖" "✕" "x") "Discard this result" pycell-discard-output t))
  "The buttons on the header of a result, left to right.
Each entry is (KEY GLYPHS HELP COMMAND WHEN):

- KEY names the button for you, and nothing else reads it.
- GLYPHS are the candidates for its label.  The first one the frame
  can draw wins, and the last one always answers, so keep a plain
  character at the end.
- HELP is the tooltip.
- COMMAND runs on a click.
- WHEN says when the button shows: t always, `image' only with an
  image in the result, `lines' only with output.

Drop an entry you never press, reorder them, or give one a glyph your
font draws better.  The fold arrow and the spinner are not buttons of
this list: they say what the result is doing."
  :type overblock-button-type
  :set #'pycell--set-buttons)

(defcustom pycell-markdown-buttons
  '((move-up ("󰅃" "⌃" "u") "Move this cell up" pycell-move-cell-up t)
    (move-down ("󰅀" "⌄" "d") "Move this cell down" pycell-move-cell-down t)
    (edit ("󰲶" "✎" "e") "Edit this markdown cell in its own buffer"
          pycell-md-edit t)
    (source ("󰕍" "⟲" "s") "Show the plain source" pycell-md-raw t))
  "The buttons on the header of a rendered markdown cell.
The entries read as in `pycell-result-buttons'.  A markdown cell has
no output, so `lines' and `image' say nothing here."
  :type overblock-button-type
  :set #'pycell--set-buttons)

(defcustom pycell-source-buttons
  '((move-up ("󰅃" "⌃" "u") "Move this cell up" pycell-move-cell-up t)
    (move-down ("󰅀" "⌄" "d") "Move this cell down"
               pycell-move-cell-down t)
    (render ("󰑐" "⟳" "m") "Render this markdown cell"
            pycell-md-render-cell t))
  "The buttons on the bar of a markdown cell that shows its source.
The entries read as in `pycell-result-buttons\\='.  Such a cell is one
just written, or one taken back to its source with `pycell-md-raw\\=';
the third button renders it.

No glyph of a bar is the glyph of another button of that bar, or of a
button that means something else on another kind of bar — in any of the
three rows of candidates.  A frame draws whichever row it can, and a
frame with a font but no nerd glyphs draws the second one."
  :type overblock-button-type
  :set #'pycell--set-buttons)

(defcustom pycell-cell-buttons
  '((move-up ("󰅃" "⌃" "u") "Move this cell up" pycell-move-cell-up t)
    (move-down ("󰅀" "⌄" "d") "Move this cell down"
               pycell-move-cell-down t)
    (run-above ("󱏦" "⏫" "a") "Run every cell above this one"
               pycell-run-above t)
    (run ("󰼛" "▷" "r") "Run this cell" pycell-run-cell t))
  "The buttons on the bar of a code cell, left to right.
The entries read as in `pycell-result-buttons'.  A cell bar is drawn
before the cell has run, so `lines' and `image' say nothing here.

The two move buttons lead, as they lead on the other two bars: the
three kinds of bar stand above one another in a notebook, and one order
for all of them is one order to learn.  Not one column: the buttons are
held at the window edge, so a bar with five of them starts a slot
further left than a bar with four."
  :type overblock-button-type
  :set #'pycell--set-buttons)

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

;;;; Blocks of every kind

(defun pycell-remove-blocks ()
  "Remove the blocks of the buffer.
This is the command a reader binds, and `overblock-clear' is the same
thing under it.  Results and rendered markdown cells go; the text of
the buffer is not touched."
  (interactive)
  (overblock-clear))

(defvar pycell--moving nil
  "Non-nil while `pycell-move-cell-down' is moving a cell.
`pycell--stale-hook' stands down while it is: a move relocates whole
cells rather than editing the text of one, and the command takes the
blocks of both cells off and puts them back itself.  The text the move
inserts lands at the first character of the cell below, which is where
that cell's anchor begins, so its `insert-in-front-hooks' ran and its
result went with the insertion — measured, a third cell that had
nothing to do with the move lost its result on every move down.")

(defun pycell--stale-hook (block after beg end &optional _length)
  "Take BLOCK down when the text it covers changes, BEG..END.
AFTER marks the call that follows the change; see
`pycell--stale-when-edited'.

An insertion is judged on that call and no earlier: the call before the
change has nothing to read, and acting on it took the block down before
the text it was to be judged by existed.  A deletion is judged before,
because the anchor evaporates with the text it covers and no call would
follow.
What it reads for is one insertion the block must survive: a single
newline at the end of the buffer.  A block\'s anchor stops one character
short of the last newline of its cell, so a cell at the end of a file
that has none has an anchor ending at `point-max' — and then the
newline `require-final-newline' adds is an insertion at that end.  The
rendering came off as the reader saved the file.  A newline there
changes nothing the cell renders, typed by hand or added by a save, so
the block stays either way; a second character reaches the anchor\'s
interior and takes it down.

The whole buffer, not the accessible part: under a narrowing
`point-max' is the end of that, and an insertion there is in the middle
of the buffer like any other."
  (unless pycell--moving
    (cond
     (after
      (unless (and (equal (buffer-substring-no-properties beg end) "\n")
                   (= end (without-restriction (point-max))))
        (overblock-delete block)))
     ;; Before the change, an insertion has nothing to read: BEG and END
     ;; are the one position it will go to.  A deletion is judged here
     ;; all the same, because the anchor evaporates with the text it
     ;; covers and the call after the change would never come — and then
     ;; the bar above a rendered cell, which the anchor does not cover,
     ;; stayed behind.
     ((/= beg end) (overblock-delete block)))))

(defvar-local pycell--width nil
  "The width the bars of this buffer were built for, in pixels.
`overblock-bar\\=' cuts the label to the room the icons leave, and the cut
is in the string: a window made narrower afterwards — a split, a side
window, a frame resized — was left with a label too long for it, and
the header took two rows.  A bar is remade when this changes.")

(defun pycell--rewidth ()
  "Draw the bars again where the window has changed width.
On `window-configuration-change-hook\\=', where it runs for the buffer of
every window that changed.

Only the bars: a result is drawn again from the record it already
holds, which is what a tick does five times a second, and a rendered
markdown cell keeps its rendering and takes a new bar.  Only on a
change of width, because the hook runs for every other kind of change
as well."
  (when-let* ((width (overblock-window-width)))
    (unless (eql width pycell--width)
      (setq pycell--width width)
      (dolist (block (overblock-in (point-min) (point-max) 'result))
        (pycell--update block))
      (mapc #'pycell--bar-redraw (overblock-bars)))))

(defun pycell--stale-when-edited (block)
  "Take BLOCK down on the next edit of the text it covers.
Three hooks and not one: `modification-hooks' runs for a change inside
an overlay, `insert-in-front-hooks' for one at its first character and
`insert-behind-hooks' for one at its end.  A block's anchor stops one
character short of the cell's last newline, so the blank line that ends
a cell — where `C-e' on the last line puts point — is an insertion at
the end: with `modification-hooks' alone the block stayed behind,
showing the result of text that had changed under it."
  (let ((drop (list #'pycell--stale-hook)))
    (overlay-put block 'modification-hooks drop)
    (overlay-put block 'insert-in-front-hooks drop)
    (overlay-put block 'insert-behind-hooks drop)))

;;;; Result blocks

(defun pycell--strip-prompts (text)
  "Return TEXT without the shell\'s prompts and its Out[N] labels.
The prompt before the output goes, the prompt after it goes, and so does
the one that ends up on the same line as output which stopped without a
newline — `comint-prompt-regexp' anchors to a line start and cannot see
that one.  An `Out[N]:' label goes where it begins a line, which is
where the shell writes one; see the comment below for the one that does
not, and why it stays.  Call this in the shell buffer, where that
variable has its value."
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
    ;; A plain python3 shell leaves a prompt on the same line after a
    ;; `sys.stdout.write' without a newline.  Take that one off.
    (when (string-match (concat "\\(?:" (string-remove-prefix
                                         "^" comint-prompt-regexp)
                                "\\)[ \t]*\\'")
                        text)
      (setq text (substring text 0 (match-beginning 0)))))
  ;; The search costs 0.009 milliseconds and the replacement 3.08 over a
  ;; hundred thousand characters, measured: `replace-regexp-in-string'
  ;; copies its argument twice even when nothing matches, and a plain
  ;; python3 shell never writes a label at all.
  ;;
  ;; Anchored to a line start, which is where the shell writes one.  A
  ;; label the shell wrote after output that stopped without a newline
  ;; sits on that same line and stays: unanchored, this took `Out[1]: '
  ;; out of the middle of a value that held those characters itself —
  ;; `'a value that says Out[1]: inside it'' came out as `'a value that
  ;; says inside it'', and `pycell--text' hands that to
  ;; `pycell-copy-output', so the reader yanked the hole as well.  The
  ;; two cannot be told apart: a trailing prompt takes the newline after
  ;; a `print' with it, so "ends the text" says nothing either.  A label
  ;; left on the screen is the cheaper fault of the two.
  (if (string-search "Out[" text)
      (replace-regexp-in-string "^Out\\[[0-9]+\\]: " "" text)
    text))

(defun pycell--clean (text)
  "Return TEXT as a result block can show it.
The prompts and the Out[N] labels go, and the copy is cut loose from
the shell; see `pycell--strip-prompts' and `overblock-repl-detach' for what
each of those means.  Call this in the shell buffer, where
`comint-prompt-regexp' has its value."
  (overblock-repl-detach (pycell--strip-prompts text)))

(defun pycell--shorten (line)
  "Return LINE cut to `pycell-max-line-length' characters.
The cut is marked with an ellipsis.  See the option for what a line
left whole costs the scroller."
  (if (or (not (natnump pycell-max-line-length))
          (zerop pycell-max-line-length)
          (<= (length line) pycell-max-line-length))
      line
    (concat (substring line 0 pycell-max-line-length)
            (overblock-glyph "…" "..."))))

(defun pycell--body-lines (lines)
  "Return the leading LINES that show inline.
At most `pycell-max-lines', each cut to `pycell-max-line-length', and
nothing after the first line that carries an image it can draw: more
inline figures would grow the block, and the scroll jump with it,
without bound.  A display that shows no images has nothing to stop
for, and names them instead.  A line with an image on it is not cut,
since the image may sit past the cut; its images are capped to
`overblock-image-height' instead."
  (let (shown stop)
    (while (and lines (not stop) (< (length shown) pycell-max-lines))
      (let* ((l (pop lines))
             (imagep (overblock-image-in l))
             ;; Only where an image can be drawn.  A terminal shows
             ;; the space it rides on and nothing else, so stopping
             ;; there would cost the rest of the output and buy no
             ;; height back.
             (drawp (and imagep (display-images-p))))
        (push (cond (drawp (overblock-image-cap l))
                    ;; A blank row said nothing about the figure that
                    ;; could not be drawn there.
                    (imagep (pycell--shorten (overblock-image-label l)))
                    (t (pycell--shorten l)))
              shown)
        (when drawp (setq stop t))))
    (nreverse shown)))

(defun pycell--header (folded total shown runtime state imagep)
  "Return the header bar of a result.
FOLDED is non-nil when only the header shows.
TOTAL and SHOWN count the lines and the inline subset.  RUNTIME is the
time in seconds since the cell started.  STATE is `running' while the
cell runs, `died' where the interpreter went away before the cell
ended, and nil where the cell finished.  IMAGEP marks a result with an image."
  (let* ((icons (overblock-buttons pycell-result-buttons imagep total))
         ;; The stopwatch drives the spinner: one frame for each tick.
         (mark (cond ((eq state 'running)
                      (let ((frames (overblock-glyph "⠋⠙⠹⠸⠼⠴⠦⠧⠇⠏" "|/-\\")))
                        (string ?\s (aref frames (mod (truncate runtime 0.2)
                                                      (length frames))))))
                     ((eq state 'died) (overblock-glyph " 󰀪" " ⚠" " !"))
                     ;; A single line can still be tall: one image is
                     ;; one line, and that is the block worth folding.
                     ((> total 0)
                      (overblock-button (if folded
                                            (overblock-glyph " 󰍟" " ▸" " >")
                                          (overblock-glyph " 󰍝" " ▾" " v"))
                                        "Fold or unfold this result"
                                        #'pycell-toggle-output))
                     ;; nothing printed: every other case is above
                     (t (overblock-glyph " 󰄬" " ✓" " ."))))
         (label (cond ((> total 0)
                       (format "%d line%s%s" total (if (= total 1) "" "s")
                               (if (< shown total)
                                   (format ", showing %d" shown) "")))
                      ((not state) "no output")))
         (time (format "%.1fs" runtime)))
    (overblock-bar
     (concat mark " " (string-join (delq nil (list label time)) " · "))
     icons 'pycell-header)))

(defun pycell--update (block)
  "Make the header and the body of the result BLOCK again, and show them.
The lines are counted once for both: the header says how many there
are and how many of them show, and the body is those that show."
  (let* ((data (overblock-get block :data))
         (folded (plist-get data :folded))
         (text (plist-get data :text))
         (total (plist-get data :total)))
    (let* ((empty (string-empty-p text))
           (lines (unless empty (overblock-repl-first-lines text pycell-max-lines)))
           (shown (pycell--body-lines lines))
           ;; The count is asked for once and kept: a finished result
           ;; carries none, and a fold would otherwise scan the whole
           ;; output again on every keypress.
           (count (cond (empty 0)
                        (total)
                        (t (let ((n (overblock-repl-count-lines text)))
                             (overblock-set block :data
                                            (plist-put data :total n))
                             n)))))
      (overblock-set block :header
                     (pycell--header folded count (length shown)
                                     (plist-get data :runtime)
                                     (plist-get data :state)
                                     (and lines (overblock-image-in text))))
      (overblock-set block :body
                     (when (and shown (not folded))
                       (overblock-faced (string-join shown "\n")
                                        'pycell-output)))
      (overblock-refresh block))))

(defun pycell--tab-filter (cmd)
  "Return CMD when point sits at the very end of a cell with a result."
  (and (eolp)
       (seq-some (lambda (o) (eq (point) (overlay-end o)))
                 (overblock-in (max (1- (point)) (point-min)) (point)
                               'result))
       cmd))

(defvar-keymap pycell-result-map
  :doc "Keymap inside a cell that shows a result."
  "TAB" '(menu-item "" pycell-toggle-output :filter pycell--tab-filter))

(defun pycell--show (beg end text runtime &optional state total)
  "Show TEXT as the result of the cell BEG..END.
RUNTIME is the time in seconds since the cell started.  STATE is
`running' while the cell runs, `died' where the interpreter went away
before the cell ended, and nil where the cell finished.

Empty TEXT gets a header that says \"no output\", so the cell is
recognizable as evaluated, and the fold state of a replaced result is
kept.  TOTAL is how many lines the cell has printed, for a running cell
whose TEXT is only the part that shows; without it the lines of TEXT are
counted."
  (let* ((old (car (overblock-in beg end 'result)))
         (data (list :folded (and old (plist-get (overblock-get old :data)
                                                 :folded))
                     :text text :runtime runtime :state state :total total)))
    (if (and old (= (overlay-start old) beg))
        ;; The ticker of a running cell comes here five times a second
        ;; with nothing new but its data.  Keeping the block it has saves
        ;; two overlays and a scan of the region on every tick, and it
        ;; leaves redisplay alone.
        (progn (overblock-set old :data data)
               (pycell--update old)
               old)
      ;; The newline that ends the cell carries the result; give the
      ;; last cell of the buffer one.  The whole buffer: under a
      ;; narrowing `point-max' is the end of the accessible part, and
      ;; the newline went into the middle of the buffer — measured, it
      ;; cut a `print(2)' in two.
      (without-restriction
        (when (and (= end (point-max)) (not (eq (char-before end) ?\n)))
          (save-excursion (goto-char end) (insert "\n"))))
      (let ((block (overblock-show beg end
                                   :kind 'result
                                   :data data
                                   :keymap pycell-result-map)))
        ;; An edit of the cell makes the result stale; it goes.
        (pycell--stale-when-edited block)
        (pycell--update block)
        block))))

(defun pycell--goto-event (event)
  "Select the window of EVENT and move point to the click.
Anything that is not a click leaves point where it is: the commands read
EVENT from `last-input-event', so it can be any event at all, and a
`switch-frame' is a cons whose start is a frame rather than a place.

A click on a bar leaves point on the line below it rather than on the
line itself.  That line still belongs to the same cell, so every command
finds what it is for, and the bar stays on the screen: point on a
boundary line gives the bar way to the text under it, which took the
button out from under the reader between one press and the next.  A
move carries the offset within the cell, so the cell can be moved twice
running."
  (when-let* (((consp event))
              (posn (event-start event))
              ((consp posn))
              (pos (posn-point posn)))
    (select-window (posn-window posn))
    (goto-char pos)
    (when (and (overblock-bar-on-line)
               (< (pos-eol) (point-max)))
      (forward-line 1))))

(defun pycell--result-at (event)
  "Return the result block at point, or at the click in EVENT.
Point first, then anywhere in the cell around it.  Signals a
`user-error' where the cell has no result, which is the answer the
commands that call it give their reader."
  (pycell--goto-event event)
  (or (overblock-at 'result)
      (car (apply #'overblock-in (append (code-cells--bounds) '(result))))
      (user-error "No result here")))

(defun pycell-toggle-output (&optional event)
  "Fold or unfold the result at point, or the one clicked in EVENT."
  (interactive (list last-input-event))
  (let* ((block (pycell--result-at event))
         (data (overblock-get block :data)))
    (overblock-set block :data
                   (plist-put data :folded (not (plist-get data :folded))))
    (pycell--update block)))

(defun pycell-discard-output (&optional event)
  "Discard the result at point, or the one clicked in EVENT."
  (interactive (list last-input-event))
  (overblock-delete (pycell--result-at event)))

;;;; Moving a cell

(defun pycell--cell-state (beg end)
  "Return what the cell BEG..END shows, to put back after a move.
The car is the record of its result, or nil, and the cdr says whether
its markdown was rendered."
  (cons (when-let* ((block (car (overblock-in beg end 'result))))
          (copy-sequence (overblock-get block :data)))
        (and (overblock-in beg end 'markdown) t)))

(defun pycell--restore-cell (beg end state)
  "Show STATE on the cell BEG..END again.
STATE comes from `pycell--cell-state'.  A markdown cell is rendered by
the caller, which does the whole buffer at once."
  ;; The record goes back whole: the region was cleared, so the block
  ;; `pycell--show' builds has no state of its own worth keeping.
  (when-let* ((record (car state))
              (block (pycell--show beg end "" 0.0)))
    (overblock-set block :data record)
    (pycell--update block)))

(defun pycell--running-in-p (beg end)
  "Return non-nil where the cell the shell is running lies in BEG..END.
Asked of this buffer alone: another notebook on the same shell may be
the one running, and its cells are not moving."
  (when-let* ((proc (python-shell-get-process))
              (run (buffer-local-value 'pycell--run (process-buffer proc)))
              (mark (plist-get run :beg))
              ((eq (marker-buffer mark) (current-buffer))))
    (<= beg mark end)))

;;;###autoload
(defun pycell-move-cell-down (&optional arg event)
  "Move the cell at point down ARG cells, with what it shows.
A negative ARG moves it up, which is all `pycell-move-cell-up' does.
EVENT is the click that asked for the move, where a button asked.

An outline move, because `code-cells-mode' makes every boundary line an
outline heading and a cell is therefore a subtree.  From the boundary
line, since that mode takes the major mode's own headings into
`outline-regexp' too.  Outline cuts the
text and puts it back; `code-cells-move-cell-down' transposed the two
regions, and `transpose-regions' leaves an overlay where the text used
to be — the result of one cell ended up under the other.  It also glued
the file together where the last cell had no final newline, writing
\"# omega# %%\" and leaving one cell where there were two; outline
writes that newline itself.

The blocks of both cells come off after the move — the moved cell's go
with the text it was cut from, parts and all, so what is left of them
is swept — and go back on the cells they belong to.  Point travels with
the cell, so a click on the button of a header keeps moving the same
cell."
  (interactive (list (prefix-numeric-value current-prefix-arg)
                     last-input-event))
  (setq arg (or arg 1))
  ;; The click first, so the cell that moves is the one whose button was
  ;; pressed.  A header answers for its own cell wherever point is: with
  ;; point left where it was, clicking the arrow of one cell moved
  ;; another.
  (pycell--goto-event event)
  (pcase-let* ((`(,beg ,end) (code-cells--bounds))
               (`(,nbeg ,nend) (code-cells--neighbor-bounds arg))
               (offset (- (point) beg))
               (mine (pycell--cell-state beg end))
               (theirs (pycell--cell-state nbeg nend)))
    ;; A cell the shell is still writing into cannot move: the run holds
    ;; markers into its text, and the move cuts that text out — the
    ;; markers collapsed, the block came back frozen at whatever the
    ;; last tick had shown, and the rest of the output went nowhere.
    (when (pycell--running-in-p (min beg nbeg) (max end nend))
      (user-error "Wait for the cell to finish, or M-x pycell-interrupt"))
    ;; From the cell's own boundary line: `code-cells-mode' takes the
    ;; major mode's headings into `outline-regexp' as well, so
    ;; `outline-back-to-heading' from inside a cell that holds a `def'
    ;; finds the def and would move that instead — measured, it
    ;; refused with "Cannot move past superior level" and the cell
    ;; stayed where it was.
    (goto-char beg)
    ;; This signals when there is nowhere to move, before anything is
    ;; taken off.
    (let ((pycell--moving t))
      (condition-case nil
          (outline-move-subtree-down arg)
        ;; The text before the first boundary line is a cell to
        ;; code-cells and no subtree at all to outline.
        (outline-before-first-heading
         (user-error "Can't move the text above the first cell"))
        ;; Outline says "Cannot move past superior level", which is
        ;; about headings and levels: neither is a word this package
        ;; uses, and the reader pressed an arrow on a cell.
        (user-error
         (user-error "No cell to swap this one with"))))
    ;; Point is where the cell that moved now begins, so the buffer is
    ;; asked for the two ranges rather than counting them out.
    (overblock-clear (min beg nbeg) (max end nend))
    ;; The text of the moved cell was cut out, so its parts outlived
    ;; the anchor that owned them.
    (overblock-sweep-orphans)
    (pcase-let* ((`(,mbeg ,mend) (code-cells--bounds))
                 (`(,tbeg ,tend) (code-cells--neighbor-bounds (- arg))))
      (pycell--restore-cell mbeg mend mine)
      (pycell--restore-cell tbeg tend theirs)
      (when (or (cdr mine) (cdr theirs))
        ;; The two cells that moved, not every cell in the file.
        (pycell-md-render-all (min mbeg tbeg) (max mend tend)))
      (goto-char (+ mbeg (min offset (- mend mbeg)))))))

;;;###autoload
(defun pycell-move-cell-up (&optional arg event)
  "Move the cell at point up ARG cells, with what it shows.
EVENT is the click that asked for the move, where a button asked."
  (interactive (list (prefix-numeric-value current-prefix-arg)
                     last-input-event))
  (pycell-move-cell-down (- (or arg 1)) event))

(defun pycell--text (block)
  "Return the text of the result BLOCK.
While the cell runs, that is only the part that shows — the head the
tick reads, some sixteen lines — so copying, popping out or saving says
as much rather than handing over a fraction in silence."
  (let ((data (overblock-get block :data)))
    (when (eq (plist-get data :state) 'running)
      (message "pycell: the cell is still running, so this is only \
what shows"))
    (plist-get data :text)))

(defun pycell--cell-buffer-name (kind position)
  "Return the name of the KIND buffer for the cell at POSITION.
KIND is the word after `pycell' in the name, or nil for a result.
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
  (kill-new (pycell--text (pycell--result-at event)))
  (message "pycell: result copied"))

(defun pycell-save-image (&optional event)
  "Save the first image of the result at point, or of the one in EVENT.
The file type comes from the image descriptor; `create-image' read
it from the data's magic bytes."
  (interactive (list last-input-event))
  (let* ((text (pycell--text (pycell--result-at event)))
         (img (or (overblock-image-in text)
                  (user-error "No image in this result")))
         (data (or (plist-get (cdr img) :data)
                   (user-error "This image carries no data")))
         (type (plist-get (cdr img) :type))
         (file (read-file-name
                "Save image to: " nil nil nil
                (format "figure.%s" (if (eq type 'jpeg) "jpg" type)))))
    (let ((coding-system-for-write 'no-conversion))
      ;; Asks before it overwrites: the name comes from
      ;; `read-file-name', which does not.
      (write-region data nil file nil nil nil t))
    (message "pycell: image saved to %s" file)))

(defun pycell--follow-done (follow text)
  "Put TEXT, the whole of what the cell printed, into FOLLOW's buffer.
The tail the run wrote there is raw: it carries the shell's prompts,
and the last of it arrives after the closing one.  The finished buffer
holds what a result popped out after the fact would hold."
  (when-let* ((buffer (car-safe follow))
              ((buffer-live-p buffer)))
    (with-current-buffer buffer
      (let* ((inhibit-read-only t)
             (end (point-max))
             (at-end (= (point) end))
             ;; Every window, not the buffer's point alone: `erase-buffer'
             ;; puts them all at 1, and a reader watching in a window of
             ;; its own was scrolled back to the top at the very moment
             ;; the last of the output arrived.
             (following (seq-filter (lambda (window)
                                      (= (window-point window) end))
                                    (get-buffer-window-list buffer nil t))))
        (erase-buffer)
        (pycell--insert-result text)
        (goto-char (if at-end (point-max) (point-min)))
        (dolist (window following)
          (set-window-point window (point-max)))))))

(defvar-local pycell--cell nil
  "Where the cell a popped-out result shows begins, as a marker.
`pycell-interrupt\\=' asks whether that is still the cell the shell is
running: a buffer showing a result that has ended, or one whose cell is
long finished, must not stop somebody else\\='s run.")

(defvar-local pycell--shell nil
  "The Python shell a popped-out result came from.
A pop-out is not a Python buffer, so `python-shell-get-process\\=' would
answer with whatever shell the settings point at — the wrong one where
the notebook has a shell of its own.  `pycell-interrupt\\=' asks this
first.")

(defvar-keymap pycell-pop-map
  :doc "Keymap in a buffer showing one result of its own.
The buffer is read-only, so a plain key is free and is what answers
wherever the reader has bound the `C-c\\=' prefix: a minor mode that owns
it shadows a major mode's map, and `C-c C-c\\=' then reaches nothing at
all.  Both are bound, and `i\\=' is the one to rely on."
  :parent special-mode-map
  "i" #'pycell-interrupt
  "C-c C-c" #'pycell-interrupt)

(defun pycell--insert-result (text)
  "Insert TEXT as a popped-out result, in the current buffer.
A table goes in live: every binding of vtable works here, and vtable
aligns the columns for this window itself.  It goes in as a copy,
because the table of the result belongs to the shell buffer that drew
it.

Only the table did, once, and the rest of the cell\\='s output went
missing — the six lines a cell printed before its DataFrame, and the
lines a follower had already seen.  This buffer is the one that holds
more than the block, so what is around a table goes in with it."
  (let ((pos 0)
        (len (length text))
        (drawn nil))
    (while (< pos len)
      (let ((table (get-text-property pos 'overblock-repl-table text))
            (next (or (next-single-property-change
                       pos 'overblock-repl-table text)
                      len)))
        (cond
         ;; `overblock-repl-detach' leaves the table on the text it laid
         ;; out; the padding between its runs carries no property, so
         ;; one table can arrive in several pieces and is drawn once.
         ((and table (not (eq table drawn)))
          (vtable-insert (overblock-repl-table-copy table))
          ;; `vtable--insert' ends with point back at the row after the
          ;; header, so the next chunk went in between the header and
          ;; the rows: the table's body was pushed to the end of the
          ;; buffer and what followed the table was glued into it.
          (goto-char (point-max))
          (setq drawn table))
         (table nil)
         (t
          (let ((part (substring text pos next)))
            ;; A figure is one space carrying an image: on a display
            ;; that draws none, this buffer held that space and nothing
            ;; else.
            (insert (if (display-images-p) part
                      (overblock-image-label part))))))
        (setq pos next)))))

(defun pycell-pop-output (&optional event)
  "Show the result at point, or the one clicked in EVENT, in a buffer.
Each cell gets one buffer, so results are comparable side by side.

A cell that is still running keeps writing there: the whole of what it
prints, where the block itself shows `pycell-max-lines' of it, so a
long run can be followed in a window of its own.  Point at the end of
that buffer follows the output; anywhere else it stays where it is.
The buffer is written once more when the cell ends, with the prompts
taken off and a table laid out live."
  (interactive (list last-input-event))
  (let* ((ov (pycell--result-at event))
         (runningp (eq (plist-get (overblock-get ov :data) :state) 'running))
         ;; Not `pycell--text': that answers with the head the tick
         ;; reads and says so.  A buffer that is about to follow the
         ;; cell wants everything printed so far instead.
         (text (if runningp "" (pycell--text ov)))
         (name (pycell--cell-buffer-name nil (overlay-start ov)))
         (buffer (get-buffer-create name)))
    (with-current-buffer buffer
      (special-mode)
      (use-local-map pycell-pop-map)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (pycell--insert-result text))
      (goto-char (point-max)))
    (when-let* ((proc (python-shell-get-process))
                (shell (process-buffer proc)))
      (with-current-buffer buffer
        (setq pycell--shell shell
              pycell--cell (and runningp
                                (plist-get (buffer-local-value 'pycell--run
                                                               shell)
                                           :beg)))))
    (when runningp (pycell--follow buffer))
    (pop-to-buffer buffer)))

;;;; Markdown cells

(defconst pycell--md-boundary
  "#+[[:blank:]]*%%+[[:blank:]]*\\[markdown\\]"
  "What marks a cell boundary line as a markdown cell.
Loose where `code-cells-boundary-regexp' is loose: any number of
comment characters, with or without a space, since VS Code and Spyder
write =#%% [markdown]= where jupytext writes =# %% [markdown]=.  A
tag list or a title may follow, as they may on a code cell.

The comment character is spelled out rather than asked of the syntax
table: this package reads Python and nothing else, and a caller with
another table current — a test, or a buffer whose mode has not been
set yet — would otherwise get a different answer.")

(defun pycell--md-cell-start (pos)
  "Return the start of the =# %% [markdown]= line above POS, or nil.
A non-nil value marks POS as the body of a markdown cell."
  (save-excursion
    (goto-char pos)
    (forward-line -1)
    (and (looking-at-p pycell--md-boundary) (point))))

(defun pycell--keep-result-newline (from to)
  "Keep the newline a result block hangs on out of a fold over FROM..TO.
A fold that reaches the end of the buffer covers that newline, where a
fold in the middle of one stops short of it.  The block would go with
the fold, and the reader would lose the bar that folds the result
itself, so the invisible run is shrunk back off the newline."
  (dolist (block (overblock-in from to 'result))
    ;; A live overlay, tested by its buffer: this runs from advice on
    ;; `outline-flag-region', and a deleted overlay in the slot answers
    ;; nil to `overlay-end' — which raised on every fold in the buffer.
    (when-let* ((nl (overblock-get block :newline))
                ((overlay-buffer nl))
                ((<= (overlay-end nl) to)))
      (dolist (ov (overlays-in (overlay-start nl) (overlay-end nl)))
        (when (and (eq (overlay-get ov 'invisible) 'outline)
                   (> (overlay-end ov) (overlay-start nl)))
          (move-overlay ov (overlay-start ov)
                        (max (overlay-start ov) (overlay-start nl))))))))

(defun pycell--outline-flag-blocks (from to flag)
  "Hide or show the blocks in FROM..TO to match an outline fold.
FLAG is non-nil where `outline-flag-region' hid the region, and this
follows that call rather than guessing which command made it.

A markdown cell is the content of its cell, so it goes under the fold:
`:hidden' takes it off the screen and a refresh puts it back, since a
block makes what it shows anew.

A result block stays.  The fold hides the code and the block keeps its
own fold button, so the two fold apart; `pycell--keep-result-newline'
is what leaves it room.

The advice is global, so this runs on every fold in every outline buffer
of the session.  It filters on the block properties rather than on the
mode: a buffer can carry blocks with the mode off — the tests do it, and
so does a mode turned off while a result is on the screen — and the two
scans below cost two interval-tree queries where there is nothing to
find."
  (when flag (pycell--keep-result-newline from to))
  (dolist (block (overblock-in from to 'markdown))
    (overblock-set block :hidden flag)
    (overblock-refresh block)))

;; At load, not at activation: by the time this file loads, the user
;; has turned the mode on.  The advice is inert in buffers without
;; blocks, because it filters on the block properties.
(advice-add 'outline-flag-region :after #'pycell--outline-flag-blocks)

(defun pycell--md-uncomment (text)
  "Strip the comment prefixes from the markdown cell TEXT."
  (replace-regexp-in-string "^# ?" "" text))

(defun pycell--md-comment (text)
  "Prefix each line of TEXT as a jupytext markdown comment."
  (mapconcat (lambda (l) (if (string-empty-p l) "#" (concat "# " l)))
             (split-string text "\n") "\n"))

(defvar-keymap pycell-md-map
  :doc "Keymap on rendered markdown cells."
  "RET" #'pycell-md-edit
  "C-c C-o" #'pycell-md-follow-link
  "<mouse-2>" #'pycell-md-edit
  "<mouse-1>" #'pycell-md-raw)

(defun pycell--md-links (block)
  "Return the links of BLOCK's rendering, in the order they are shown.
Each is a cons of the text the reader sees and the URL under it.

The rendering itself, not the pieces it was dealt into: a piece that
holds an image hides its line with a display string of nothing and
shows its row on the before-string instead, and an empty string is not
nil — reading the display property first took that empty string for the
row.  Every link of a cell with an image in it was lost, the badge a
notebook opens with included."
  (let ((shown (overblock-get block :over))
        (pos 0)
        links)
    (when (stringp shown)
      (let ((len (length shown)))
        (while (< pos len)
          (let ((url (get-text-property pos 'shr-url shown))
                (next (or (next-single-property-change pos 'shr-url shown)
                          len)))
            (when (stringp url)
              (push (cons (string-trim (substring-no-properties
                                        shown pos next))
                          url)
                    links))
            (setq pos next)))))
    (nreverse links)))

;;;###autoload
(defun pycell-md-follow-link ()
  "Follow a link of the rendered markdown cell at point.
Clicking a link follows it already: a click is answered by the string
it lands on, and the rendering carries shr's own keymap there.  Point
cannot be put on a link at all — it never enters a display string, and
the row under it belongs to the source, not to the rendering — so this
asks the cell for its links instead.  With one, it is followed; with
several, the reader chooses."
  (interactive)
  (let* ((block (pycell--md-at nil))
         (links (pycell--md-links block)))
    (cond
     ((null links) (user-error "No link in this cell"))
     ((null (cdr links)) (browse-url (cdar links)))
     (t (browse-url
         (cdr (assoc (completing-read "Follow link: " (mapcar #'car links)
                                      nil t)
                     links)))))))

(defun pycell--md-show (beg end &optional html)
  "Show the markdown cell body BEG..END rendered, in place.
With HTML, the cell is not sent to the converter again: it was
converted with the rest of the buffer.
The rendering hangs on the source lines themselves, a piece to a
line \(see `overblock--pieces'), so the cell scrolls like ordinary
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
  (when-let* ((rendered (overblock-md-rendered
                         (pycell--md-uncomment
                          (buffer-substring-no-properties beg end))
                         html)))
    (pycell--md-block beg end rendered)))

(defun pycell--md-bar (hov)
  "Draw the bar HOV of a rendered markdown cell, or draw it again.
The bar is an overlay on the boundary line above the cell, which the
block keeps under `:bar\\='.  It is remade rather than the cell rendered
again when the window changes width: the rendering does not depend on
the width, and the label of the bar does."
  (when (overlay-buffer hov)
    (overblock-bar-draw hov 'markdown
                        (concat (overblock-glyph "󰽛" "◇" "M") " "
                                (or (pycell--cell-title (overlay-start hov)
                                                        (overlay-end hov))
                                    "markdown"))
                        (overblock-buttons pycell-markdown-buttons)
                        'pycell-header)))

(defun pycell--md-block (beg end rendered)
  "Show RENDERED over the markdown cell BEG..END, with a bar above it.
See `pycell--md-show', which renders and calls this."
  (let* ((start (1- beg))
         (help "RET/mouse-2: edit this markdown cell, mouse-1: show source")
         (text (overblock-fill-props
                (overblock-faced rendered 'default)
                'keymap pycell-md-map 'help-echo help))
         ;; The bar covers the boundary line and stops before the
         ;; newline where the cell begins, so the line reads as the
         ;; header of the cell and not as a comment with a bar after it.
         ;; Whatever bar the line carries goes first: the cell may have
         ;; been showing its source, which is barred too.
         (hov (let ((from (pycell--md-cell-start beg)))
                (pycell--sole-bar from start nil)
                (overblock-bar-over from start)))
         ;; The block covers the source of the cell.  The pieces hang
         ;; on those lines, and the bar above them is not part of it.
         (block (overblock-show beg end
                                :kind 'markdown
                                ;; the bounds of the source, which
                                ;; the block itself does not cover
                                :data (cons (copy-marker beg)
                                            (copy-marker end t))
                                :over text
                                :keymap pycell-md-map
                                :help-echo help
                                :attached (list hov))))
    (overlay-put hov 'keymap pycell-md-map)
    ;; A click on the bar lands on this overlay, so it points back at
    ;; the block, which knows the bounds of the cell.
    (overlay-put hov 'pycell-main block)
    (overblock-set block :bar hov)
    (pycell--md-bar hov)
    ;; An edit of the source takes the rendering with it, the bar
    ;; included.  The block itself evaporates with the text it covers,
    ;; and the bar sits on the boundary line above, where no edit of the
    ;; cell reaches it: it would be left behind, and `pycell-md-commit'
    ;; would draw a second bar beside it.
    (pycell--stale-when-edited block)
    block))

(defun pycell--md-cells (beg end)
  "Return the body of every markdown cell between BEG and END, in order.
Each is a cons of where the body starts and where it ends, which is the
next boundary line or the end of the buffer.  A cell with nothing in it
is left out: there is nothing to render."
  (save-excursion
    (goto-char beg)
    (let (cells)
      (while (re-search-forward (concat "^" pycell--md-boundary) end t)
        (forward-line 1)
        (let ((from (point))
              (to (if (re-search-forward code-cells-boundary-regexp nil t)
                      (pos-bol)
                    (point-max))))
          (when (< from to) (push (cons from to) cells))
          ;; Never past the bound: `re-search-forward\\=' signals on a
          ;; bound behind point, whatever its NOERROR says, and a cell
          ;; that reaches past END would leave point there.
          (goto-char (min to end))))
      (nreverse cells))))

;;;###autoload
(defun pycell-md-render-all (&optional beg end)
  "Render the markdown cells between BEG and END, the whole buffer by default.
A markdown cell is one whose boundary line reads \"# %% [markdown]\".
A caller that knows which cells changed says so: measured, one moved
cell in a file of two hundred rendered every one of them, 436
milliseconds against 17.7 for the two that moved."
  (interactive)
  (let* ((found (pycell--md-cells (or beg (point-min))
                                  (or end (point-max))))
         ;; Without a converter there is nothing to render, and the
         ;; reader is told once rather than once a cell.
         (cells (and (overblock-md-program) found)))
    ;; One converter process for the buffer rather than one per cell.
    ;; It answers nil where the marker between cells did not survive,
    ;; and then each cell goes on its own, as before.
    (let ((htmls (and (cdr cells)
                      (overblock-md-html-batch
                       (mapcar (lambda (cell)
                                 (pycell--md-uncomment
                                  (buffer-substring-no-properties
                                   (car cell) (cdr cell))))
                               cells)))))
      (dolist (cell cells)
        (pycell--md-show (car cell) (cdr cell) (pop htmls))))
    (when (and found (not cells))
      (message "pycell: %s, cells stay plain"
               (if (fboundp 'libxml-parse-html-region)
                   (format "no markdown converter found (%s)"
                           (string-join (ensure-list overblock-md-command)
                                        ", "))
                 "this Emacs was built without libxml, which shr reads \
the converter's HTML with")))))

(defun pycell-md-unrender ()
  "Show all markdown cells as their plain source again."
  (interactive)
  (overblock-clear (point-min) (point-max) 'markdown)
  ;; And whatever lost its anchor: a block whose anchor evaporated with
  ;; the line it hung on leaves its parts behind, and a clear that names
  ;; a kind cannot sweep them — an orphan says nothing about the kind it
  ;; belonged to.  This is the command a reader reaches for when a
  ;; rendering looks wrong, so it takes them too.  The results stay: a
  ;; bare `overblock-clear' here took every one of them with it.
  (overblock-sweep-orphans))

(defun pycell--md-at (event)
  "Return the markdown block at point, or at the click in EVENT.
A click on the bar lands on the small overlay that draws it, which
points back at the block.  Signals a `user-error' where there is no
rendered cell, which is the answer the commands that call it give."
  (pycell--goto-event event)
  (or (overblock-at 'markdown)
      ;; A click on the bar lands beside the block, so the overlay that
      ;; drew it is asked next.
      (seq-some (lambda (ov) (overlay-get ov 'pycell-main))
                (overlays-in (max (1- (point)) (point-min))
                             (min (1+ (point)) (point-max))))
      (user-error "No rendered markdown cell here")))

;;;###autoload
(defun pycell-md-render-cell (&optional event)
  "Render the markdown cell at point, or the one whose button EVENT clicked.
`pycell-md-render-all\\=' does the whole buffer; this is the button on the
bar of a cell that is showing its source."
  (interactive (list last-input-event))
  (pycell--goto-event event)
  (pcase-let ((`(,beg ,end) (code-cells--bounds nil nil t)))
    (unless (pycell--md-cell-start beg)
      (user-error "This is not a markdown cell"))
    (pycell--md-show beg end)))

(defun pycell-md-raw (&optional event)
  "Show the markdown cell at point, or the one in EVENT, as plain source.
The cell is then editable in place; evaluate it, or run
`pycell-md-render-all', to render it again."
  (interactive (list last-input-event))
  (overblock-delete (pycell--md-at event)))

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
  (pcase-let* ((block (pycell--md-at event))
               (`(,beg . ,end) (overblock-get block :data))
               (src (current-buffer))
               ;; Trimmed on the right: the cell reaches to the next
               ;; boundary line, so it holds the blank line jupytext
               ;; writes between cells.  With that line in the edit
               ;; buffer a paragraph typed at the end landed after it,
               ;; and `pycell-md-commit' put the gap back below —
               ;; three comment lines a round, compounding.
               (md (string-trim-right
                    (pycell--md-uncomment
                     (buffer-substring-no-properties beg end))))
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
      ;; The same cell, not merely the same name: the name carries a
      ;; line number, and two cells can stand on that line at different
      ;; times.  Keeping a stranger's pending edit committed one cell's
      ;; text into another; throwing it away without a word would lose an
      ;; hour of writing just as quietly, so the reader is asked.
      (let ((pending (and pycell-md-edit-mode (buffer-modified-p)))
            (mine (equal pycell--md-source (list src beg end))))
        (when (and pending (not mine)
                   (not (yes-or-no-p
                         "Discard the unsaved edit of another cell? ")))
          (user-error "Kept the unsaved edit"))
        (unless (and pending mine)
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
          (set-buffer-modified-p nil)))
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

;;;; The bar over a boundary line

(defun pycell--cell-title (bol eol)
  "Return the title written on the boundary line BOL..EOL, or nil.
What follows the =%%= marker is the title, as jupytext writes it, less
the tag list of a =# %% [markdown]= line.  A cell without one is named
after what it holds."
  (save-excursion
    (goto-char bol)
    (when (looking-at code-cells-boundary-regexp)
      ;; Trimmed before the tags are taken off as well as after: the
      ;; marker is followed by a space, and an anchored search for the
      ;; tag list then found nothing to take off — every rendered
      ;; markdown cell was labelled "[markdown]".
      (let ((title (string-trim
                    (replace-regexp-in-string
                     "\\`\\(\\[[^]]*\\][[:blank:]]*\\)+" ""
                     (string-trim
                      (buffer-substring-no-properties (match-end 0) eol))))))
        (unless (string-empty-p title) title)))))

(defun pycell--bar-redraw (ov)
  "Draw the bar OV again, of whichever kind of cell it belongs to."
  (pcase (overblock-bar-kind ov)
    ('code (pycell--code-bar (overlay-start ov) (overlay-end ov)))
    ('source (pycell--source-bar (overlay-start ov) (overlay-end ov)))
    ('markdown (pycell--md-bar ov))))

(defun pycell--source-bar (bol eol)
  "Draw the bar of the markdown cell BOL..EOL that is showing its source.
A rendered markdown cell is barred by its rendering; a cell that has
none — one just written, or one taken back to its source — had no bar at
all, and the line read as one the package had lost track of."
  (let ((there (overblock-bar-in bol (min (point-max) (1+ eol))))
        ov)
    (setq ov (if (eq (overblock-bar-kind there) 'source)
                 there
               (overblock-bar-over bol eol)))
    (move-overlay ov bol eol)
    (overblock-bar-draw ov 'source
                        (concat (overblock-glyph "󰽛" "◇" "M") " "
                                (or (pycell--cell-title bol eol) "markdown"))
                        (overblock-buttons pycell-source-buttons)
                        'pycell-header)))

(defun pycell--code-bar (bol eol)
  "Draw the bar of the code cell whose boundary line is BOL..EOL.
A bar of another kind on that line is not taken over: the bar of a
rendered markdown cell belongs to its block, and drawing a code bar on
it left the block holding an overlay that was no longer its own."
  (let ((there (overblock-bar-in bol (min (point-max) (1+ eol))))
        ov)
    (setq ov (if (eq (overblock-bar-kind there) 'code)
                 there
               (overblock-bar-over bol eol)))
    ;; Text typed at the end of the line is outside the overlay, and the
    ;; bar then covered a boundary line only as far as it reached when
    ;; the line was shorter.
    (move-overlay ov bol eol)
    (overblock-bar-draw ov 'code
                        (concat (overblock-glyph "󰌠" "◆" "%") " "
                                (or (pycell--cell-title bol eol) "python"))
                        (overblock-buttons pycell-cell-buttons)
                        'pycell-header)))

(defun pycell--drop-bar (bar)
  "Take BAR down, and the rendering it belongs to where it has one.
A markdown bar is a block\\='s own overlay: the block goes with it, and
the source of the cell comes back.  Its boundary line no longer says
=[markdown]=, so the rendering below it is a rendering of nothing."
  (if-let* ((block (overlay-get bar 'pycell-main)))
      (overblock-delete block)
    (delete-overlay bar)))

(defun pycell--cell-bars (start end)
  "Draw the bar of every code cell whose boundary line START..END touches.
Whole lines, whatever START and END are: this is called with the bounds
of a change, and a change reaches the middle of a line.

A markdown cell has a bar of its own where it is rendered, and none
where it shows its source: its boundary line is left out here, and a
bar left over from before the line said =[markdown]= goes."
  (save-excursion
    (goto-char (min start end))
    (forward-line 0)
    (setq end (save-excursion (goto-char (max start end)) (pos-eol)))
    (while (< (point) (min end (point-max)))
      (pycell--bar-this-line)
      (forward-line 1))))

(defun pycell--sole-bar (bol eol kinds)
  "Return the one bar to keep on the line BOL..EOL, and drop the others.
KINDS names the kinds worth keeping, best first; every bar of another
kind goes, and so does a second bar of the same kind.  Nil keeps none of
them.

A line carries one bar, and two things want to put one there: the pass
that bars every boundary line, and the rendering of a markdown cell,
which brings its own.  Measured in a graphical frame before this: three
bars on the boundary line of one rendered cell."
  (let ((bars (seq-filter #'overblock-bar-kind
                          (overlays-in bol (min (point-max) (1+ eol)))))
        keep)
    (dolist (kind kinds)
      (unless keep
        (setq keep (seq-find (lambda (bar) (eq (overblock-bar-kind bar) kind))
                             bars))))
    (dolist (bar bars)
      (unless (eq bar keep) (pycell--drop-bar bar)))
    keep))

(defun pycell--bar-this-line ()
  "Give the line point is on the bar it should have, or take one away.
Four lines to tell apart: one that is no boundary, a markdown boundary
whose cell is rendered, a markdown boundary whose cell shows its source,
and a code boundary."
  (let ((bol (pos-bol))
        (eol (pos-eol)))
    (cond
     ;; Not a boundary line any more — a space typed before the comment,
     ;; a marker half deleted.  Whatever bar it carries goes: its
     ;; buttons would act on the cell that now encloses the line.
     ((not (looking-at-p code-cells-boundary-regexp))
      (pycell--sole-bar bol eol nil))
     ;; A rendered markdown cell is barred by its rendering, which
     ;; brings its own bar; one showing its source is barred here, with
     ;; the button that renders it.
     ((looking-at-p pycell--md-boundary)
      (unless (eq (overblock-bar-kind
                   (pycell--sole-bar bol eol '(markdown source)))
                  'markdown)
        (pycell--source-bar bol eol)))
     (t
      ;; A rendering whose line stopped saying =[markdown]= is a
      ;; rendering of nothing, and the bar of a source that is no longer
      ;; markdown is a bar for a cell that has gone: both come down, and
      ;; the line takes a code bar like any other.
      (pycell--sole-bar bol eol '(code))
      (pycell--code-bar bol eol)))))

(defun pycell--bars-after-change (beg end _length)
  "Draw the bars of the lines the change BEG..END touched.
On `after-change-functions\\=', and not on `jit-lock-register\\=': one error
in any other jit-lock function skips the rest of them, and a
`python-ts-mode\\=' buffer whose grammar does not match the mode signals
from redisplay — not one bar was drawn in such a buffer."
  (pycell--cell-bars beg end))


;;;; Running cells

(defvar-local pycell--queue nil
  "Start markers of the cells that `pycell-restart-and-run-all\' still runs.
Kept in the Python shell\'s buffer, beside `pycell--run\': a notebook with
a shell of its own — which `python-shell-dedicated\' gives it — has a
queue of its own.  One global list let a run-all in one notebook discard
another\'s cells and then feed its own down that notebook\'s interpreter.
`pycell--queue-buffer\' is how to reach it.")

(defun pycell--queue-buffer ()
  "Return the buffer that holds the run-all queue for this one.
That is the Python shell: this buffer where it is one, and the shell this
notebook sends to otherwise.  Nil where there is no shell, and then there
is nothing queued either."
  (if (derived-mode-p 'inferior-python-mode)
      (current-buffer)
    (when-let* ((proc (python-shell-get-process)))
      (process-buffer proc))))

(defvar-local pycell--queue-home nil
  "Where point goes in the notebook when this shell\\='s queue runs out.
`pycell--run-next\\=' walks point down the notebook, which is what makes a
pass over the whole buffer visible.  A pass asked for from one cell
gives point back instead: the reader pressed a button there.")

(defun pycell--queued ()
  "Return the cells a run-all still has to run, in order."
  (when-let* ((shell (pycell--queue-buffer)))
    (buffer-local-value 'pycell--queue shell)))

(defun pycell--go-home ()
  "Put point back where the pass that has just ended was asked for."
  (when-let* ((shell (pycell--queue-buffer))
              (home (buffer-local-value 'pycell--queue-home shell))
              ((buffer-live-p (marker-buffer home))))
    (with-current-buffer shell (setq pycell--queue-home nil))
    (with-current-buffer (marker-buffer home)
      (goto-char home)
      (when (bound-and-true-p pycell-mode) (overblock-bar-reveal)))))

(defun pycell--queue-set (cells)
  "Give the shell CELLS to run, and answer them."
  (when-let* ((shell (pycell--queue-buffer)))
    (with-current-buffer shell (setq pycell--queue cells))))

(defvar-local pycell--run nil
  "State of the cell that runs in this Python shell, or nil.
A plist:

  :from   where the output of the cell starts in this buffer
  :beg    :end  the cell in its own buffer
  :tail   the recent output, for the prompt detection
  :start  the `float-time' of the send
  :timer  the ticker
  :head   the part of the output the block shows, once it can no
          longer change
  :count  (POSITION . LINES) counted up to POSITION, so a tick reads
          only what arrived since the one before it

The last two belong to the live mirror.")

(defvar-local pycell--cold-cell nil
  "Cell (BEG . END markers) that waits for this shell's first prompt.")

(defun pycell--whole-escapes (text)
  "Return TEXT without an escape sequence that has not arrived in full.
comint-mime sends an image as one escape sequence, and half of one
swallows everything after it until the rest comes.  The search is what
makes this cheap: `replace-regexp-in-string' copies its argument twice
whether it matches or not, which measured 3.19 milliseconds over a
hundred thousand characters of propertized text against 0.040 for the
search that stands in front of it now."
  (if (string-search "\e]" text)
      (replace-regexp-in-string "\e\\][^\e]*\\'" "" text)
    text))

(defun pycell--output-so-far (from)
  "Return the running cell's output after FROM, cleaned.
An incomplete escape sequence at the end is dropped: comint-mime
renders it only when it is complete."
  (pycell--clean (pycell--whole-escapes (buffer-substring from (point-max)))))

(defun pycell--output-head (from)
  "Return as much of the output after FROM as the block can show.
`pycell--body-lines' takes the first `pycell-max-lines' lines and
stops, so a tick has no reason to read — or clean — everything the
cell has printed.  Once those lines are all in, the text cannot
change anymore and is kept, and the ticks after that read nothing.

A cell that prints much on few lines never reaches that line, so the
read is bounded in characters as well; the comment below says why that
bound holds only where no escape sequence begins inside it.
Nothing is kept while the head is empty: an escape sequence that has
not arrived in full swallows everything after it until it does, and a
cell whose first lines are still on their way has more to come."
  (or (plist-get pycell--run :head)
      (let* ((budget (and (natnump pycell-max-line-length)
                          (> pycell-max-line-length 0)
                          ;; what `pycell--body-lines' can show, and no
                          ;; more: the lines it keeps, each cut to the
                          ;; length it cuts them to
                          (* pycell-max-lines (1+ pycell-max-line-length))))
             (limit (save-excursion
                      (goto-char from)
                      (forward-line (+ pycell-max-lines 4))
                      (point)))
             ;; A cell that prints much on few lines never reaches that
             ;; line, so its text is never kept and every tick reads and
             ;; cleans everything printed so far: measured, 68
             ;; milliseconds a tick over a hundred thousand characters on
             ;; one line, five times a second, for the two thousand
             ;; characters that show.  The body cuts each line to
             ;; `pycell-max-line-length' anyway, so a bound in characters
             ;; loses nothing that shows — except where it would cut an
             ;; escape sequence in two.  comint-mime sends an image as
             ;; one, and a cut inside it drops the figure: measured, a
             ;; result of no characters at all.  So the bound holds only
             ;; where no escape begins inside it.
             (limit (if (and budget
                             (> (- limit from) budget)
                             (not (save-excursion
                                    (goto-char from)
                                    (search-forward
                                     "\e]" (min (point-max) (+ from budget))
                                     t))))
                        (+ from budget)
                      limit))
             (text (pycell--clean
                    (pycell--whole-escapes (buffer-substring from limit)))))
        (when (and (< limit (point-max))
                   (not (string-empty-p text)))
          (setq pycell--run (plist-put pycell--run :head text)))
        text)))

(defun pycell--total (from)
  "Return the number of lines the running cell has printed after FROM.
Counted where they arrive: reading the whole output again is a pass
over everything printed so far, and a cell that prints a lot pays
that pass five times a second.  Leading blank lines go, as
`pycell--clean' drops them, so the count agrees with the one the
finished cell shows."
  (let* ((state (or (plist-get pycell--run :count)
                    (cons (save-excursion
                            (goto-char from)
                            (skip-chars-forward " \t\n")
                            (point-marker))
                          0)))
         (count (cdr state)))
    (save-excursion
      (goto-char (car state))
      (while (search-forward "\n" nil t) (setq count (1+ count)))
      ;; The marker is moved rather than made again.  Every marker left
      ;; behind stays in the buffer's chain until a garbage collection,
      ;; and comint adjusts the whole chain on every insertion: 2000
      ;; ticks over 60000 inserted lines measured 0.144 seconds with a
      ;; fresh marker each time and 0.036 with this one.
      (setq pycell--run
            (plist-put pycell--run :count
                       (cons (set-marker (car state) (point)) count))))
    ;; A line that has not ended yet is a line all the same.
    (if (and (> (point-max) (marker-position from))
             (not (eq (char-before (point-max)) ?\n)))
        (1+ count)
      count)))

(defun pycell--end (text &optional died)
  "End the running cell and show TEXT as its final result.
The one exit for every way a cell ends; DIED marks abnormal ends.
Call this in the Python shell buffer.

Nothing happens where no cell is running: a failing send can end its
cell through the filter and then signal, and the handler would call this
a second time — `cancel-timer' of nil raised, which masked the error it
was reporting.  `pycell--abort' asks the same question."
  (when pycell--run
    (pcase-let (((map :beg (:end fin) :start :timer :follow) pycell--run))
      ;; The last of the output, and then the whole of it cleaned: the
      ;; tail a follower wrote is raw, and its final lines arrive with
      ;; the closing prompt.
      (pycell--follow-tick)
      (setq pycell--run nil)
      (cancel-timer timer)
      (when died (setq pycell--queue nil))
      (when (buffer-live-p (marker-buffer beg))
        (with-current-buffer (marker-buffer beg)
          (pycell--show beg fin text (- (float-time) start)
                        (and died 'died))))
      (pycell--follow-done follow text)
      ;; Keep `pycell-restart-and-run-all' going, or stop on error.
      (when pycell--queue
        (if (string-match-p "Traceback (most recent call last)" text)
            (progn (setq pycell--queue nil)
                   (message "pycell: stopped at error"))
          (pycell--run-next))))))

(defun pycell--abort (&optional reason)
  "End the running cell abnormally — its prompt will never return.
A death notice, with the exit status when one is available, follows
the output received so far.  This covers a dead interpreter (the
ticker finds it), a killed shell buffer and a shell restart, which
reinitializes the major mode — hence also on `kill-buffer-hook' and
`change-major-mode-hook' in the Python shell.

REASON says what happened, for a caller that knows: a restart is not
an unexpected death."
  (when pycell--run
    (let* ((proc (get-buffer-process (current-buffer)))
           (out (pycell--output-so-far (plist-get pycell--run :from)))
           (msg (propertize
                 (or reason
                     (format "Process unexpectedly died%s"
                             (if proc
                                 (format " (%s %s)" (process-status proc)
                                         (process-exit-status proc))
                               "")))
                 'face 'error)))
      (pycell--end (if (string-empty-p out) msg (concat out "\n" msg))
                   t))))

(defun pycell--follow (buffer)
  "Have the running cell copy what it prints into BUFFER as it prints it.
Call this in the notebook.  Nothing happens where no cell is running.

The shell is where the output lands, so the marker that says how much
of it has been copied lives there, in the record of the run."
  (when-let* ((proc (python-shell-get-process))
              (shell (process-buffer proc)))
    (with-current-buffer shell
      (when pycell--run
        (setq pycell--run
              (plist-put pycell--run :follow
                         (cons buffer
                               (copy-marker (plist-get pycell--run :from)))))
        ;; What the cell has printed already, rather than an empty
        ;; buffer until the next tick.
        (pycell--follow-tick)))))

(defun pycell--follow-tick ()
  "Copy what the cell has printed since the last look into its buffer.
Call this in the Python shell buffer.

Only what is new: the whole output is what the block's own head is
bounded away from reading five times a second, and a cell that prints a
hundred thousand characters would cost that on every tick here as well.

Point at the end of the buffer follows the output, in the buffer and in
every window showing it; point anywhere else stays where the reader put
it."
  (when-let* ((follow (plist-get pycell--run :follow))
              (buffer (car follow))
              ((buffer-live-p buffer))
              (copied (cdr follow))
              ((< (marker-position copied) (point-max)))
              (new (buffer-substring copied (point-max))))
    (set-marker copied (point-max))
    (with-current-buffer buffer
      (let ((inhibit-read-only t)
            (end (point-max))
            (windows (get-buffer-window-list buffer nil t)))
        (save-excursion
          (goto-char (point-max))
          (insert new))
        (when (= (point) end) (goto-char (point-max)))
        (dolist (window windows)
          (when (= (window-point window) end)
            (set-window-point window (point-max))))))))

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
        (pcase-let (((map (:from from) :beg (:end fin) :start) pycell--run))
          (let* ((text (pycell--output-head from))
                 (total (if (string-empty-p text) 0 (pycell--total from))))
            (pycell--follow-tick)
            (when (buffer-live-p (marker-buffer beg))
              (with-current-buffer (marker-buffer beg)
                (pycell--show beg fin text (- (float-time) start)
                              'running total)))))))))

(defun pycell--filter (output)
  "Watch OUTPUT for the closing prompt, then end the running cell.
The filter stays on `comint-output-filter-functions' and idles while
no cell runs; the live mirroring is the ticker's job."
  (when pycell--run
    ;; A chunk boundary can split the prompt, so match a capped tail;
    ;; `ansi-color-filter-apply' drops the escape sequences.
    (let ((tail (concat (plist-get pycell--run :tail)
                        (ansi-color-filter-apply output))))
      (setq pycell--run
            (plist-put pycell--run :tail
                       (string-limit tail 256 t)))
      (when (python-shell-comint-end-of-output-p tail)
        ;; Copy to the end of the buffer and let `pycell--clean' take
        ;; the prompt off.  `comint-last-prompt' cannot serve as the
        ;; end: comint calls the last line without a newline a prompt,
        ;; so a chunk that arrives split leaves the marker inside the
        ;; output, and everything after it would be dropped without a
        ;; word.
        (pycell--end
         (pycell--clean
          (buffer-substring (plist-get pycell--run :from) (point-max))))))))

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
          (when (and (not (python-syntax-comment-or-string-p state))
                     (zerop (nth 0 state)))
            (when (looking-at-p "[ \t]*[%!]")
              (throw 'found t))
            (let ((last (save-excursion
                          (goto-char eol)
                          ;; To the start of the line, but never past
                          ;; the start of the region: with point as the
                          ;; limit this could not move at all, so `df?  '
                          ;; took the plain Python road and IPython
                          ;; answered with a syntax error — and with the
                          ;; line's start alone it read a `?' from text
                          ;; the reader had not marked.
                          (skip-chars-backward " \t" (max (pos-bol) beg))
                          (point))))
              (when (and (eq (char-before last) ??)
                         (not (python-syntax-comment-or-string-p
                               (syntax-ppss (1- last)))))
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
        ;; The process mark, and not the end of the buffer.  A render
        ;; comint-mime finishes after the closing prompt sits past the
        ;; mark, and a cell that started from the end of the buffer
        ;; would have had its own output — which comint inserts AT the
        ;; mark, before that render — fall outside its own region.  So
        ;; a late render is still swept into the next cell's result;
        ;; that is a fault of its own and not one to cure here.
        (setq pycell--run (list :from (copy-marker (process-mark proc))
                                :beg beg :end fin :tail ""
                                :start (float-time) :timer timer
                                :head nil :count nil))))
    (pycell--show beg fin "" 0.0 'running)
    ;; The bookkeeping above says a cell is running, and the send below
    ;; can fail — a signal from the shell, or `C-g' while the region is
    ;; written to its temporary file.  Without this the shell stays busy
    ;; for the rest of the session: the ticker counts up, and every later
    ;; cell is refused.  So a failed send ends the cell as a death, which
    ;; also empties the queue of a run-all.
    (condition-case error
        (if (pycell--ipython-syntax-p beg fin)
            (pycell--send-to-ipython
             proc (buffer-substring-no-properties beg fin))
          ;; `python-shell-send-region' pads the code, so traceback line
          ;; numbers match the buffer.
          (python-shell-send-region beg fin))
      ((error quit)
       (with-current-buffer (process-buffer proc)
         (pycell--end (propertize (error-message-string error) 'face 'error)
                      t))
       (signal (car error) (cdr error))))))

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

(defun pycell--dedicated ()
  "Return what a new shell is dedicated to, as the reader asked.
`python-shell-dedicated' says it.  Its `project' value makes
`run-python' ask which project, where the file belongs to none, and
that is no question to put in front of a reader who evaluated a cell —
one of a whole run, at that.  `python-shell-get-process-name' names
such a shell the shared one anyway, so that is what this answers."
  (unless (and (eq python-shell-dedicated 'project)
               (not (project-current)))
    python-shell-dedicated))

(defun pycell-eval-region (start end)
  "Evaluate START..END as a cell and mirror the output below it.
This matches the calling convention of
`code-cells-eval-region-commands'.  A markdown cell renders instead.
Without an interpreter, one starts and the cell follows on its first
prompt.  A cell sent while another one runs is refused, with a
`user-error' from `pycell--send'."
  ;; ponytail: a second cell sent by hand is refused rather than
  ;; queued; `pycell--queue' serves `pycell-restart-and-run-all' alone.
  (if (pycell--md-cell-start start)
      ;; Keep a running restart-and-run-all chain going — no prompt
      ;; will arrive to do it.
      (progn
        (pycell--md-show start end)
        ;; Redisplay pushes a point that the block just made
        ;; invisible out of it, and upwards; put it below instead.
        (when (<= (1- start) (point) end)
          (goto-char end)))
    (if-let* ((proc (python-shell-get-process)))
        (pycell--send proc start end)
      ;; Mark the cell here, while its buffer is still current:
      ;; `copy-marker' on a number answers for whatever buffer that
      ;; is, and below it is the shell's.  Markers into the shell
      ;; would send its start-up banner as the cell.
      (let ((cell (cons (copy-marker start) (copy-marker end t))))
        (run-python nil (pycell--dedicated))
        (with-current-buffer
            (process-buffer (python-shell-get-process-or-error))
          (setq pycell--cold-cell cell)
          (add-hook 'python-shell-first-prompt-hook
                    #'pycell--run-cold 90 t)))
      (message "pycell: starting the interpreter…"))))

(defun pycell-interrupt ()
  "Send a KeyboardInterrupt to the cell's Python process.
The interrupted cell ends normally: IPython prints the traceback
and prompts again.

Works in a popped-out result as well as in the notebook, which is where
a reader watching a long run has their point: such a buffer is not a
Python buffer, so it remembers the shell it came from rather than
letting `python-shell-get-process' answer with whatever the settings
point at.

In such a buffer it interrupts the cell that buffer shows, and nothing
else.  It asked the shell for whatever was running: a pop-out of a
result that had finished, or one whose own shell was gone, then killed
another notebook\\='s run at a keystroke, with no message and nothing to
undo it."
  (interactive)
  (if (not pycell--shell)
      (interrupt-process (python-shell-get-process-or-error))
    (unless (buffer-live-p pycell--shell)
      (user-error "The shell this result came from is gone"))
    (let* ((run (buffer-local-value 'pycell--run pycell--shell))
           (beg (plist-get run :beg)))
      ;; Both have to point somewhere.  A killed notebook leaves the
      ;; run's marker and this buffer's — the same object — pointing
      ;; nowhere, and `eq' on two nil buffers passed the test while `='
      ;; signalled "Marker does not point anywhere".
      (unless (and beg pycell--cell
                   (marker-buffer beg)
                   (eq (marker-buffer beg) (marker-buffer pycell--cell))
                   (= beg pycell--cell))
        (user-error "The cell this buffer shows is not running"))
      (interrupt-process
       (or (get-buffer-process pycell--shell)
           ;; `interrupt-process' of nil takes the current buffer's
           ;; process, which is not this buffer's business.
           (user-error "The shell this result came from has no process"))))))

(defun pycell--clear-results ()
  "Take the results of the buffer down, and sweep what lost its anchor.
The renderings stay.  A clear that names a kind cannot sweep an orphan —
an orphan says nothing about the kind it belonged to — so the sweep is
asked for by name here: taking the results down with
`overblock-clear' alone left a cloak of a lost block keeping lines of
the buffer invisible, with nothing able to remove it."
  (overblock-clear nil nil 'result)
  (overblock-sweep-orphans))

(defun pycell-restart ()
  "Restart the Python interpreter and remove every result.
The rendered markdown cells stay.  They were taken down with the
results, on the grounds that a rendering is a block like any other, and
that cost a whole notebook its renderings: `pycell-restart-and-run-all'
puts them back one cell at a time as the pass reaches them, so a pass
that stops — at an error, or on `pycell-stop' — leaves every cell after
that point plain.  A rendering has nothing to do with the interpreter."
  (interactive)
  (if-let* ((proc (python-shell-get-process)))
      (progn
        ;; End the running cell first, and as a death: the interpreter
        ;; it waits for is about to go.  Its cell can belong to another
        ;; notebook on the same shell, whose block would otherwise keep
        ;; a running header — spinner and stopwatch frozen where the
        ;; ticker stopped — for the rest of the session.  The block of
        ;; this buffer goes with every other one, below.
        (with-current-buffer (process-buffer proc)
          (pycell--abort "The interpreter was restarted"))
        (pycell--queue-set nil)
        (pycell--clear-results)
        (python-shell-restart))
    (pycell--queue-set nil)
    (pycell--clear-results)
    (run-python nil (pycell--dedicated))))

(defun pycell--run-next ()
  "Evaluate the cells of the shell\'s queue until one has to wait.
Point follows, so the run-all pass is visible.  Called from the shell on
its first prompt and from `pycell--end\' when a cell finishes, so the
queue is reached through `pycell--queue-buffer\' either way.

A markdown cell needs no prompt, so the walk goes on to the cell after
it here; a code cell is sent and the walk stops, to be taken up again
when its prompt comes back.  A loop and not a call back into
`pycell-eval-region\': that built a frame for every markdown cell in a
row, a hundred of them reached `max-lisp-eval-depth\', and — worse —
every frame ran its own tail on the way out, so the second one sent a
code cell while the first was still running.  `pycell--send\' refused it
from inside the process filter and that cell, already off the queue,
never ran at all."
  (remove-hook 'python-shell-first-prompt-hook #'pycell--run-next t)
  (catch 'waiting
    (while t
      (let* ((cells (pycell--queued))
             (m (car cells)))
        (unless m
          (pycell--go-home)
          (throw 'waiting nil))
        (pycell--queue-set (cdr cells))
        (unless (buffer-live-p (marker-buffer m))
          (pycell--queue-set nil)
          (throw 'waiting nil))
        (with-current-buffer (marker-buffer m)
          (goto-char m)
          ;; Point moved from a process filter, where no command ends and
          ;; `post-command-hook' never runs: without this the line the
          ;; pass left point on kept showing its text, and the line it
          ;; had left kept showing none.
          (when (bound-and-true-p pycell-mode) (overblock-bar-reveal))
          (pcase-let ((`(,beg ,end) (code-cells--bounds nil nil t)))
            ;; A markdown cell that already shows its rendering needs
            ;; no second one: the restart leaves the renderings alone
            ;; now, and rendering them again is a converter process a
            ;; cell — measured over thirty cells, 198 milliseconds
            ;; against the 32 the batch costs.
            (unless (and (pycell--md-cell-start beg)
                         (overblock-in beg end 'markdown))
              (pycell-eval-region beg end))
            (unless (pycell--md-cell-start beg)
              (throw 'waiting nil))))))))

(defun pycell-stop ()
  "Stop the run of the cells after the current one.
Both passes go through the same queue: `pycell-restart-and-run-all\\=' and
`pycell-run-above\\='."
  (interactive)
  (pycell--queue-set nil)
  (message "pycell: run all stopped"))

(defvar-keymap pycell-stop-map
  :doc "Transient keymap, active while all cells run."
  "<escape>" #'pycell-stop)

(defun pycell--cell-starts ()
  "Return a marker on the first line of every cell of the buffer, in order.
The text above the first boundary line is a cell too, and the first
marker where there is any."
  (save-excursion
    (goto-char (point-min))
    (let ((cells (unless (looking-at-p code-cells-boundary-regexp)
                   (list (point-min-marker)))))
      (while (re-search-forward code-cells-boundary-regexp nil t)
        (push (copy-marker (pos-bol)) cells))
      (nreverse cells))))

(defun pycell--run-cells (cells message)
  "Run CELLS in order, and say MESSAGE while they run.
Each cell goes on the prompt of the one before it, so the queue is left
with the shell and `pycell--run-next\\=' takes the next one off it.  The
interpreter starts where there is none, and the pass begins on its
first prompt."
  (let ((here (point-marker)))
    (when-let* ((shell (pycell--queue-buffer)))
      (with-current-buffer shell (setq pycell--queue-home here))))
  (if (python-shell-get-process)
      ;; A cell that will not start takes the whole pass with it: the
      ;; queue was left armed by a refusal, and the cells ran later
      ;; without being asked for, less the one the refusal had already
      ;; taken off it.
      (progn (pycell--queue-set cells)
             (condition-case err
                 (pycell--run-next)
               (error (pycell--queue-set nil)
                      (signal (car err) (cdr err)))))
    (run-python nil (pycell--dedicated))
    (with-current-buffer (process-buffer (python-shell-get-process-or-error))
      (add-hook 'python-shell-first-prompt-hook #'pycell--run-next 90 t))
    (pycell--queue-set cells)
    (message "pycell: starting the interpreter…"))
  (set-transient-map pycell-stop-map (lambda () (pycell--queued)) nil
                     message))

;;;###autoload
(defun pycell-run-cell (&optional event)
  "Run the cell at point, or the one whose button EVENT clicked.
The same as `code-cells-eval\\=' on that cell, which is what the reader
presses \\[code-cells-eval] for."
  (interactive (list last-input-event))
  (pycell--goto-event event)
  (apply #'code-cells-eval (code-cells--bounds nil nil t)))

;;;###autoload
(defun pycell-run-above (&optional event)
  "Run every cell above the one at point, or above the one EVENT clicked.
The cells run in order and the pass stops at the first error, or on \\<pycell-stop-map>\\[pycell-stop].
The interpreter keeps what it has: `pycell-restart-and-run-all\\=' is the
one that starts from nothing."
  (interactive (list last-input-event))
  (pycell--goto-event event)
  (let* ((beg (car (code-cells--bounds)))
         (cells (seq-take-while (lambda (m) (< m beg)) (pycell--cell-starts))))
    (unless cells (user-error "No cell above this one"))
    (pycell--run-cells cells "Evaluating the cells above, %k to stop")))

(defun pycell-restart-and-run-all ()
  "Restart the Python interpreter, then evaluate every cell in order.
The pass stops at the first error, or on \
\\<pycell-stop-map>\\[pycell-stop]."
  (interactive)
  (pycell-restart)
  (pycell--queue-set (pycell--cell-starts))
  ;; Evaluation may only start once the fresh interpreter prompted —
  ;; and after comint-mime's setup, which runs off the same hook;
  ;; hence the depth.  `pycell--end' chains the remaining cells.
  (with-current-buffer (process-buffer (python-shell-get-process-or-error))
    (add-hook 'python-shell-first-prompt-hook #'pycell--run-next 90 t))
  (set-transient-map pycell-stop-map (lambda () (pycell--queued)) nil
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
      (progn
        ;; A bar is cut to the width of the window it was built for, so
        ;; a window made narrower afterwards wants it drawn again.
        (add-hook 'window-configuration-change-hook #'pycell--rewidth nil t)
        ;; A bar is drawn in place of its boundary line, so the line
        ;; under point shows what is written on it.
        (add-hook 'post-command-hook #'overblock-bar-reveal nil t)
        (add-hook 'after-change-functions #'pycell--bars-after-change nil t)
        ;; The whole buffer, narrowed or not: a mode turned on under a
        ;; narrowing would otherwise bar the visible cells alone, and
        ;; the rest only when something edited them.
        (without-restriction
          (pycell--cell-bars (point-min) (point-max)))
        ;; The reveal is on `post-command-hook', and no command has run
        ;; yet: the line point is on would otherwise draw its bar, with
        ;; the cursor at the far right of it, until the first keystroke.
        (overblock-bar-reveal)
        (pycell-md-render-all))
    (remove-hook 'window-configuration-change-hook #'pycell--rewidth t)
    (remove-hook 'post-command-hook #'overblock-bar-reveal t)
    (remove-hook 'after-change-functions #'pycell--bars-after-change t)
    (setq overblock--revealed nil)
    (mapc #'delete-overlay (overblock-bars))
    ;; Every kind of block goes, rendered markdown cells included.
    (pycell-remove-blocks)))

;;;###autoload
(defun pycell-mode-maybe ()
  "Enable `pycell-mode' in Python cell buffers.
Made for `code-cells-mode-hook', where your configuration adds it:

  (add-hook \\='code-cells-mode-hook #\\='pycell-mode-maybe)

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
