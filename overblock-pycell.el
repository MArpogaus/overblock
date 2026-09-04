;;; overblock-pycell.el --- Inline results for Python code cells -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Marcel Arpogaus

;; Author: Marcel Arpogaus <znepry.necbtnhf@tznvy.pbz>
;; Assisted-by: Claude:claude-opus-5
;; Assisted-by: Claude:claude-fable-5
;; Version: 0.1.3
;; Package-Requires: ((emacs "29.1") (overblock "0.1.0") (overblock-md "0.1.0") (code-cells "0.5") (comint-mime "0.4"))
;; Keywords: convenience, languages, tools
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

;; Notebook style results for Python code cells, built from python.el
;; and comint-mime alone -- no Jupyter kernel and no zmq module.
;;
;; Add `overblock-pycell-mode-maybe' to `code-cells-mode-hook' and the mode is
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
(require 'overblock-run)
(require 'code-cells)
(require 'outline)
(require 'comint-mime)
(require 'python)
(require 'ansi-color)
(require 'map)
(require 'seq)
(require 'subr-x)

(defgroup overblock-pycell nil "Inline results for Python code cells." :group 'python)

(defface overblock-pycell-header '((t :inherit code-cells-header-line))
  "Face for the header bar above a result.
It inherits the cell boundary face, so results match the cells.")

(defface overblock-pycell-output '((t :inherit shadow :extend t))
  "Face for the body of a result.")

(defun overblock-pycell--set-buttons (symbol value)
  "Set SYMBOL to VALUE, and draw the headers of every notebook again.
The `:set' of the button options.  A change to one of them showed up
only when something else drew a bar again — a window changing width, or
the file opened afresh — so customizing the buttons of a notebook that
was already open appeared to do nothing at all."
  (set-default symbol value)
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (when (bound-and-true-p overblock-pycell-mode)
        (mapc #'overblock-pycell--bar-redraw (overblock-bars))
        (dolist (block (overblock-in (point-min) (point-max) 'result))
          (overblock-pycell--update block))))))

(defcustom overblock-pycell-result-buttons
  '((stop ("" "□" "stop") "Stop the run after this cell"
          overblock-pycell-stop running)
    (save-image ("" "↧" "save") "Save the result's image to a file"
                overblock-pycell-save-image image)
    (copy ("" "◫" "copy") "Copy this result" overblock-pycell-copy-output lines)
    (pop ("" "↗" "pop") "Show this result in its own buffer"
         overblock-pycell-pop-output lines)
    (discard ("" "✕" "drop") "Discard this result" overblock-pycell-discard-output t)
    (move-up ("" "⌃" "up") "Move this cell up" overblock-pycell-move-cell-up t)
    (move-down ("" "⌄" "down") "Move this cell down" overblock-pycell-move-cell-down t))
  "The buttons on the header of a result, left to right.
Each entry is (KEY GLYPHS HELP COMMAND WHEN):

- KEY names the button for you, and nothing else reads it.
- GLYPHS are the candidates for its label.  The first one the frame
  can draw wins, and the last one always answers, so keep something
  every display has at the end.  Three of them is the shape used here:
  a nerd glyph, a character an ordinary monospace font has, and a short
  word for the display that has neither — a word rather than a letter,
  because `u' and `d' say nothing to a reader who has not read this
  list.  Ask the font about the middle one before choosing it —
  measured, `⏫' is in none of Source Code Pro, Liberation Mono or
  FiraCode Nerd Font, so a frame without nerd glyphs fell all the way
  to the last candidate for that button and to a symbol for every
  other.

  Every nerd glyph here is a codicon, the set whose names begin
  nf-cod- and which VS Code draws its own buttons with.  One family,
  because it is the one whose shapes share a single hairline weight
  and a single visual size: sets mixed, the notebook drew a heavy
  filled arrow beside a thin outlined page.  A glyph the reader's own
  nerd font is too old to carry is skipped by `overblock-glyph', so
  the row falls to the symbol rather than drawing a box of hex
  digits.

  A terminal takes the last candidate as well, unless
  `overblock-terminal-glyphs' says its font carries the icons.
- HELP is the tooltip.
- COMMAND runs on a click.
- WHEN says when the button shows: t always, `image' only with an
  image in the result, `lines' only with output, `running' only while
  the cell runs.

Drop an entry you never press, reorder them, or give one a glyph your
font draws better.  The fold arrow and the spinner are not buttons of
this list: they say what the result is doing."
  :type overblock-button-type
  :set #'overblock-pycell--set-buttons)

(defcustom overblock-pycell-markdown-buttons
  '((edit ("" "✎" "edit") "Edit this markdown cell in its own buffer"
          overblock-pycell-md-edit t)
    (move-up ("" "⌃" "up") "Move this cell up" overblock-pycell-move-cell-up t)
    (move-down ("" "⌄" "down") "Move this cell down"
               overblock-pycell-move-cell-down t))
  "The buttons on the header of a rendered markdown cell.
The entries read as in `overblock-pycell-result-buttons'.  A markdown cell has
no output, so `lines' and `image' say nothing here.

No button for the source: a click on the rendering shows it, which is
what the cell's own tooltip says, and a second way of saying it is one
more icon to read."
  :type overblock-button-type
  :set #'overblock-pycell--set-buttons)

(defcustom overblock-pycell-source-buttons
  '((render ("" "⟳" "render") "Render this markdown cell"
            overblock-pycell-md-render-cell t)
    (move-up ("" "⌃" "up") "Move this cell up" overblock-pycell-move-cell-up t)
    (move-down ("" "⌄" "down") "Move this cell down"
               overblock-pycell-move-cell-down t))
  "The buttons on the bar of a markdown cell that shows its source.
The entries read as in `overblock-pycell-result-buttons'.  Such a cell is one
just written, or one taken back to its source with `overblock-pycell-md-raw';
the third button renders it.

No candidate of a bar is the candidate of another button of that bar,
or of a button that means something else on another kind of bar — in
any of the three rows.  A frame draws whichever row it can, and a
frame with a font but no nerd glyphs draws the second one."
  :type overblock-button-type
  :set #'overblock-pycell--set-buttons)

(defcustom overblock-pycell-cell-buttons
  '((run-above ("" "⇈" "above") "Run every cell above this one"
               overblock-pycell-run-above t)
    (run ("" "▷" "run") "Run this cell" overblock-pycell-run-cell t)
    (move-up ("" "⌃" "up") "Move this cell up" overblock-pycell-move-cell-up t)
    (move-down ("" "⌄" "down") "Move this cell down"
               overblock-pycell-move-cell-down t))
  "The buttons on the bar of a code cell, left to right.
The entries read as in `overblock-pycell-result-buttons'.  A cell bar is drawn
before the cell has run, so `lines' and `image' say nothing here.

The two move buttons come last, as they do on every other bar.  The
buttons are held against the right edge, so the trailing slots are the
ones that fall in the same place whatever else a bar carries: measured,
the pair leading sat at x=996 on a bar of four buttons and x=959 on one
of five, and trailing it stands in one column down the window."
  :type overblock-button-type
  :set #'overblock-pycell--set-buttons)

(defcustom overblock-pycell-max-lines 12
  "Number of result lines that show inline.
Zero shows all of them.
A result block is one buffer line however tall it is, so a long
result makes one long step for `next-line' and for the wheel.  Use
`overblock-pycell-pop-output' to see the whole of it.

Length is not what costs redisplay its time.  Measured in a 1000x700
window, forty lines of plain output scroll as cheaply as none, while
twelve lines full of face changes cost three times as much: the work
follows the number of face runs the text carries, not its size.

Width is another matter: see `overblock-pycell-max-line-length'."
  :type 'natnum)

(defcustom overblock-pycell-max-line-length 2000
  "Number of characters of a result line that show inline.
Zero shows all of them.  A line longer than this is cut, and the cut
is marked with an ellipsis; `overblock-pycell-pop-output' has the whole of it.

One long line is one line, so `overblock-pycell-max-lines' does not bound it,
and a block laid out on every redisplay costs what it holds.
Measured in a 1200x800 window: a thousand characters on one line cost
1.4 milliseconds a wheel event, five thousand 2.4, twenty thousand
12.8, and a hundred thousand 226 — a fifth of a second an event, with
the wheel sending them by the dozen.  A `print' of a wide row, a long
list or a base64 blob is one such line."
  :type 'natnum)

;;;; Blocks of every kind

;;;###autoload
(defun overblock-pycell-remove-blocks ()
  "Remove the blocks of the buffer.
This is the command a reader binds, and `overblock-clear' is the same
thing under it.  Results and rendered markdown cells go; the text of
the buffer is not touched."
  (interactive)
  (overblock-clear))

(defun overblock-pycell--drop-rendering (block)
  "Take BLOCK down, and bar the boundary line a rendering leaves behind.
The bar of a rendered markdown cell is the block's own overlay, so it
goes with the block — and no text on that line changed, so nothing else
would put one back.  Measured in a graphical frame: the line was left
with no bar at all, and the button that renders the cell again sits on
that bar."
  (let ((markdown (eq (overblock-get block :kind) 'markdown))
        (start (overlay-start block)))
    (overblock-delete block)
    (when markdown
      (when-let* ((from (overblock-pycell--md-cell-start start)))
        (overblock-pycell--cell-bars from start)))))

(defvar overblock-pycell--moving nil
  "Non-nil while `overblock-pycell-move-cell-down' is moving a cell.
`overblock-pycell--stale-when-edited' stands down while it is: a move relocates
whole
cells rather than editing the text of one, and the command takes the
blocks of both cells off and puts them back itself.  The text the move
inserts lands at the first character of the cell below, which is where
that cell's anchor begins, so its `insert-in-front-hooks' ran and its
result went with the insertion — measured, a third cell that had
nothing to do with the move lost its result on every move down.")

(defun overblock-pycell--redraw ()
  "Draw what this notebook builds for a window width again.
Only the bars and the frames the results are drawn in: a result is
drawn again from the record it already holds, which is what a tick does
five times a second, and a rendered markdown cell keeps its rendering
and takes a new bar."
  (dolist (block (overblock-in (point-min) (point-max) 'result))
    (overblock-pycell--update block))
  (mapc #'overblock-pycell--bar-redraw (overblock-bars))
  ;; A rendered markdown cell is filled to the width it is shown at.
  (overblock-width-follow 'markdown))

(defun overblock-pycell--rescale ()
  "Draw the bars again after the text scale changed.
`overblock-bar-rescale\' says why."
  (overblock-bar-rescale #'overblock-pycell--redraw))

(defun overblock-pycell--rewidth ()
  "Draw the bars again where the window has changed width.
`overblock-bar-width-follow\' says why, and it is what compares."
  (overblock-bar-width-follow #'overblock-pycell--redraw))

(defun overblock-pycell--stale-when-edited (block)
  "Take BLOCK down on the next edit of the text it covers.
A move stands the taking down: `overblock-pycell--moving' says the text is being
relocated rather than edited, and the command puts the blocks of both
cells back itself.  What comes down is the rendering and the bar a
rendered cell carries above it, which is what `overblock-pycell--drop-rendering'
knows and a plain delete does not."
  (overblock-stale-when-edited
   block (lambda (block)
           (unless overblock-pycell--moving (overblock-pycell--drop-rendering block)))))

;;;; Result blocks

(defun overblock-pycell--strip-prompts (text)
  "Return TEXT without the shell's prompts and its Out[N] labels.
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
  ;; says inside it'', and `overblock-pycell--text' hands that to
  ;; `overblock-pycell-copy-output', so the reader yanked the hole as well.  The
  ;; two cannot be told apart: a trailing prompt takes the newline after
  ;; a `print' with it, so "ends the text" says nothing either.  A label
  ;; left on the screen is the cheaper fault of the two.
  (if (string-search "Out[" text)
      (replace-regexp-in-string "^Out\\[[0-9]+\\]: " "" text)
    text))

(defun overblock-pycell--drop-prompt-face (text)
  "Return TEXT with the face comint paints a prompt with taken off.
comint calls a chunk of output that ends without a newline a prompt,
and paints it `comint-highlight-prompt'.  A cell that prints a single
line arrives as one such chunk, so the commonest result of all showed
in the colour of a prompt: measured against a real IPython, a cell that
printed one line came back in that face, where the same cell printing
three lines came back plain.

Only that face.  Every other one rides the same property and says
something about the output: ansi-color writes the colours of a terminal
there, and comint-mime the faces of whatever it renders.

A run left with no face at all loses the property rather than carrying
a nil: what a block costs redisplay follows the number of face runs its
text has, and a property set to nil is a run of its own.

TEXT is written on in place.  It is the copy `overblock-pycell--clean' was
handed, which comes from `buffer-substring'."
  (let ((pos 0)
        (len (length text)))
    (while (< pos len)
      (let* ((next (or (next-single-property-change pos 'font-lock-face text)
                       len))
             (face (ensure-list (get-text-property pos 'font-lock-face text)))
             (kept (remq 'comint-highlight-prompt face)))
        (unless (= (length kept) (length face))
          (if kept
              (put-text-property pos next 'font-lock-face
                                 (if (cdr kept) kept (car kept))
                                 text)
            (remove-text-properties pos next '(font-lock-face nil) text)))
        (setq pos next))))
  text)

(defun overblock-pycell--clean (text)
  "Return TEXT as a result block can show it.
The prompts and the Out[N] labels go, the face of a prompt goes with
them, and the copy is cut loose from the shell; see
`overblock-pycell--strip-prompts', `overblock-pycell--drop-prompt-face' and
`overblock-repl-detach' for what each of those means.  Call this in the
shell buffer, where `comint-prompt-regexp' has its value."
  (overblock-repl-detach
   (overblock-pycell--drop-prompt-face (overblock-pycell--strip-prompts text))))

(defun overblock-pycell-tab-filter (cmd)
  "Return CMD when point sits at the very end of a cell with a result.
A `menu-item' filter for a key in `overblock-pycell-result-map': it keeps a key
that means something in the rest of the cell — TAB indents — out of the
way everywhere but on the one spot where the reader faces the result."
  (and (eolp)
       (seq-some (lambda (o) (eq (point) (overlay-end o)))
                 (overblock-in (max (1- (point)) (point-min)) (point)
                               'result))
       cmd))

(defvar-keymap overblock-pycell-result-map
  :doc "Keymap inside a cell that shows a result, empty on purpose.
overblock-pycell binds no keys; put your own here.  is the
`overblock-pycell-toggle-output'
natural candidate.  Guard a key the rest of the cell needs with
`overblock-pycell-tab-filter', which answers only at the very end of the cell:

  (keymap-set overblock-pycell-result-map \"TAB\"
              \\='(menu-item \"\" overblock-pycell-toggle-output
                          :filter overblock-pycell-tab-filter))")

(defvar overblock-pycell--style
  (list :keymap overblock-pycell-result-map
        :buttons (lambda () overblock-pycell-result-buttons)
        :fold #'overblock-pycell-toggle-output
        :header-face 'overblock-pycell-header
        :output-face 'overblock-pycell-output
        :stale #'overblock-pycell--stale-when-edited
        :lines (lambda () overblock-pycell-max-lines)
        :chars (lambda () overblock-pycell-max-line-length))
  "How a result of this notebook looks, for `overblock-run-show'.
The commentary of `overblock-run' lists the slots.  A plain variable
and not a buffer-local one: a block can be drawn with no mode on, and
the options it reads are looked up when it is drawn.")

(defun overblock-pycell--update (block)
  "Make the header and the body of the result BLOCK again, and show them."
  (overblock-run-update overblock-pycell--style block))

(defun overblock-pycell--show (beg end text runtime &optional state total)
  "Show TEXT as the result of the cell BEG..END.
RUNTIME, STATE and TOTAL are what `overblock-run-show' takes."
  (overblock-run-show overblock-pycell--style beg end text runtime state total))

(defun overblock-pycell--output-head (from)
  "Return as much of the running cell's output after FROM as shows."
  (overblock-run-output-head from overblock-pycell-max-lines
                             overblock-pycell-max-line-length
                             #'overblock-pycell--clean))

(defun overblock-pycell--result-at (event)
  "Return the result block at point, or at the click in EVENT.
Point first, then anywhere in the cell around it.  Signals a
`user-error' where the cell has no result, which is the answer the
commands that call it give their reader."
  (overblock-goto-event event)
  (or (overblock-at 'result)
      (car (apply #'overblock-in (append (code-cells--bounds) '(result))))
      (user-error "No result here")))

;;;###autoload
(defun overblock-pycell-toggle-output (&optional event)
  "Fold or unfold the result at point, or the one clicked in EVENT."
  (interactive (list last-input-event))
  (let* ((block (overblock-pycell--result-at event))
         (data (overblock-get block :data)))
    (overblock-set block :data
                   (plist-put data :folded (not (plist-get data :folded))))
    (overblock-pycell--update block)))

;;;###autoload
(defun overblock-pycell-discard-output (&optional event)
  "Discard the result at point, or the one clicked in EVENT."
  (interactive (list last-input-event))
  (overblock-delete (overblock-pycell--result-at event)))

;;;; Moving a cell

(defun overblock-pycell--cell-state (beg end)
  "Return what the cell BEG..END shows, to put back after a move.
The car is the record of its result, or nil, and the cdr says whether
its markdown was rendered."
  (cons (when-let* ((block (car (overblock-in beg end 'result))))
          (copy-sequence (overblock-get block :data)))
        (and (overblock-in beg end 'markdown) t)))

(defun overblock-pycell--restore-cell (beg end state)
  "Show STATE on the cell BEG..END again.
STATE comes from `overblock-pycell--cell-state'.  A markdown cell is rendered by
the caller, which does the whole buffer at once."
  ;; The record goes back whole: the region was cleared, so the block
  ;; `overblock-pycell--show' builds has no state of its own worth keeping.
  (when-let* ((record (car state))
              (block (overblock-pycell--show beg end "" 0.0)))
    (overblock-set block :data record)
    (overblock-pycell--update block)))

(defun overblock-pycell--running-in-p (beg end)
  "Return non-nil where the cell the shell is running lies in BEG..END.
Asked of this buffer alone: another notebook on the same shell may be
the one running, and its cells are not moving."
  (when-let* ((proc (python-shell-get-process))
              (run (buffer-local-value 'overblock-run--state (process-buffer proc)))
              (mark (plist-get run :beg))
              ((eq (marker-buffer mark) (current-buffer))))
    (<= beg mark end)))

;;;###autoload
(defun overblock-pycell-move-cell-down (&optional arg event)
  "Move the cell at point down ARG cells, with what it shows.
A negative ARG moves it up, which is all `overblock-pycell-move-cell-up' does.
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
  (overblock-goto-event event)
  (pcase-let* ((`(,beg ,end) (code-cells--bounds))
               (`(,nbeg ,nend) (code-cells--neighbor-bounds arg))
               (offset (- (point) beg))
               (mine (overblock-pycell--cell-state beg end))
               (theirs (overblock-pycell--cell-state nbeg nend)))
    ;; A cell the shell is still writing into cannot move: the run holds
    ;; markers into its text, and the move cuts that text out — the
    ;; markers collapsed, the block came back frozen at whatever the
    ;; last tick had shown, and the rest of the output went nowhere.
    (when (overblock-pycell--running-in-p (min beg nbeg) (max end nend))
      (user-error "Wait for the cell to finish, or M-x overblock-pycell-interrupt"))
    ;; From the cell's own boundary line: `code-cells-mode' takes the
    ;; major mode's headings into `outline-regexp' as well, so
    ;; `outline-back-to-heading' from inside a cell that holds a `def'
    ;; finds the def and would move that instead — measured, it
    ;; refused with "Cannot move past superior level" and the cell
    ;; stayed where it was.
    (goto-char beg)
    ;; This signals when there is nowhere to move, before anything is
    ;; taken off.  Any error puts point back where the reader had it:
    ;; outline walks point before it refuses, and an error of a kind
    ;; not named here — one from a mode whose headings are in
    ;; `outline-regexp' beside the cells — left the reader inside
    ;; another cell with nothing said.
    (let ((overblock-pycell--moving t)
          (here (point-marker)))
      (condition-case error
          (outline-move-subtree-down arg)
        ;; The text before the first boundary line is a cell to
        ;; code-cells and no subtree at all to outline.
        (outline-before-first-heading
         (goto-char here)
         (user-error "Can't move the text above the first cell"))
        ;; Outline says "Cannot move past superior level", which is
        ;; about headings and levels: neither is a word this package
        ;; uses, and the reader pressed an arrow on a cell.
        (user-error
         (goto-char here)
         (user-error "No cell to swap this one with"))
        (error
         (goto-char here)
         (signal (car error) (cdr error)))))
    ;; Point is where the cell that moved now begins, so the buffer is
    ;; asked for the two ranges rather than counting them out.
    (overblock-clear (min beg nbeg) (max end nend))
    ;; The text of the moved cell was cut out, so its parts outlived
    ;; the anchor that owned them.
    (overblock-sweep-orphans)
    (pcase-let* ((`(,mbeg ,mend) (code-cells--bounds))
                 (`(,tbeg ,tend) (code-cells--neighbor-bounds (- arg))))
      (overblock-pycell--restore-cell mbeg mend mine)
      (overblock-pycell--restore-cell tbeg tend theirs)
      (when (or (cdr mine) (cdr theirs))
        ;; The two cells that moved, not every cell in the file.
        (overblock-pycell-md-render-all (min mbeg tbeg) (max mend tend)))
      (goto-char (+ mbeg (min offset (- mend mbeg)))))))

;;;###autoload
(defun overblock-pycell-move-cell-up (&optional arg event)
  "Move the cell at point up ARG cells, with what it shows.
EVENT is the click that asked for the move, where a button asked."
  (interactive (list (prefix-numeric-value current-prefix-arg)
                     last-input-event))
  (overblock-pycell-move-cell-down (- (or arg 1)) event))

(defun overblock-pycell--text (block)
  "Return the text of the result BLOCK.
While the cell runs, that is only the part that shows — the head the
tick reads, some sixteen lines — so copying, popping out or saving says
as much rather than handing over a fraction in silence."
  (let ((data (overblock-get block :data)))
    (when (eq (plist-get data :state) 'running)
      (message "overblock-pycell: the cell is still running, so this is only \
what shows"))
    (plist-get data :text)))

(defun overblock-pycell--cell-buffer-name (kind position)
  "Return the name of the KIND buffer for the cell at POSITION.
KIND is the word after `overblock-pycell' in the name, or nil for a result.
The name carries the line of the cell, so each cell has a buffer of
its own and the buffers of two cells cannot collide."
  (format "*overblock-pycell%s: %s:%d*"
          (if kind (concat " " kind) "")
          (buffer-name)
          (line-number-at-pos position)))

;;;###autoload
(defun overblock-pycell-copy-output (&optional event)
  "Copy the result at point, or the one clicked in EVENT.
The copy keeps its text properties, so images survive a yank."
  (interactive (list last-input-event))
  (kill-new (overblock-pycell--text (overblock-pycell--result-at event)))
  (message "overblock-pycell: result copied"))

;;;###autoload
(defun overblock-pycell-save-image (&optional event)
  "Save the first image of the result at point, or of the one in EVENT.
The file type comes from the image descriptor; `create-image' read
it from the data's magic bytes."
  (interactive (list last-input-event))
  (let* ((text (overblock-pycell--text (overblock-pycell--result-at event)))
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
    (message "overblock-pycell: image saved to %s" file)))

(defun overblock-pycell--follow-done (buffer text)
  "Put TEXT, the whole of what the cell printed, into BUFFER.
The backend's `:done'.  The tail the run wrote there is raw: it carries
the shell's prompts, and the last of it arrives after the closing one.
The finished buffer holds what a result popped out after the fact would
hold."
  (progn
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
        (overblock-pycell--insert-result text)
        (goto-char (if at-end (point-max) (point-min)))
        (dolist (window following)
          (set-window-point window (point-max)))))))

(defvar-local overblock-pycell--cell nil
  "Where the cell a popped-out result shows begins, as a marker.
`overblock-pycell-interrupt' asks whether that is still the cell the shell is
running: a buffer showing a result that has ended, or one whose cell is
long finished, must not stop somebody else's run.")

(defvar-local overblock-pycell--shell nil
  "The Python shell a popped-out result came from.
A pop-out is not a Python buffer, so `python-shell-get-process' would
answer with whatever shell the settings point at — the wrong one where
the notebook has a shell of its own.  `overblock-pycell-interrupt' asks this
first.")

(defvar-keymap overblock-pycell-pop-map
  :doc "Keymap in a buffer showing one result of its own, empty on purpose.
overblock-pycell binds no keys; put your own here.  `overblock-pycell-interrupt'
and
`overblock-pycell-stop' are the natural candidates: both resolve the shell the
result came from, not the buffer they are pressed in.  The buffer is
read-only, so a plain key is free:

  (keymap-set overblock-pycell-pop-map \"i\" #\\='overblock-pycell-interrupt)"
  :parent special-mode-map)

(defun overblock-pycell--insert-result (text)
  "Insert TEXT as a popped-out result, in the current buffer.
A table goes in live: every binding of vtable works here, and vtable
aligns the columns for this window itself.  It goes in as a copy,
because the table of the result belongs to the shell buffer that drew
it.

Only the table did, once, and the rest of the cell's output went
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

;;;###autoload
(defun overblock-pycell-pop-output (&optional event)
  "Show the result at point, or the one clicked in EVENT, in a buffer.
Each cell gets one buffer, so results are comparable side by side.

A cell that is still running keeps writing there: the whole of what it
prints, where the block itself shows `overblock-pycell-max-lines' of it, so a
long run can be followed in a window of its own.  Point at the end of
that buffer follows the output; anywhere else it stays where it is.
The buffer is written once more when the cell ends, with the prompts
taken off and a table laid out live."
  (interactive (list last-input-event))
  (let* ((ov (overblock-pycell--result-at event))
         (runningp (eq (plist-get (overblock-get ov :data) :state) 'running))
         ;; Not `overblock-pycell--text': that answers with the head the tick
         ;; reads and says so.  A buffer that is about to follow the
         ;; cell wants everything printed so far instead.
         (text (if runningp "" (overblock-pycell--text ov)))
         (name (overblock-pycell--cell-buffer-name nil (overlay-start ov)))
         (buffer (get-buffer-create name)))
    (with-current-buffer buffer
      (special-mode)
      (use-local-map overblock-pycell-pop-map)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (overblock-pycell--insert-result text))
      (goto-char (point-max)))
    ;; Set whether or not a shell answers: a pop-out whose shell is
    ;; gone is still not a notebook, and `overblock-pycell-interrupt' there must
    ;; not fall through to whatever shell the settings point at — that
    ;; killed another notebook's running cell at a keystroke.
    (let* ((proc (python-shell-get-process))
           (shell (and proc (process-buffer proc))))
      (with-current-buffer buffer
        (setq overblock-pycell--shell shell
              overblock-pycell--cell (and runningp shell
                                (plist-get (buffer-local-value 'overblock-run--state
                                                               shell)
                                           :beg)))))
    (when runningp (overblock-run-follow buffer))
    (pop-to-buffer buffer)))

;;;; Markdown cells

(defconst overblock-pycell--md-boundary
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

(defun overblock-pycell--md-cell-start (pos)
  "Return the start of the =# %% [markdown]= line above POS, or nil.
A non-nil value marks POS as the body of a markdown cell."
  (save-excursion
    (goto-char pos)
    (forward-line -1)
    (and (looking-at-p overblock-pycell--md-boundary) (point))))

(defun overblock-pycell--keep-result-newline (from to)
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

(defun overblock-pycell--outline-flag-blocks (from to flag)
  "Hide or show the blocks in FROM..TO to match an outline fold.
FLAG is non-nil where `outline-flag-region' hid the region, and this
follows that call rather than guessing which command made it.

A markdown cell is the content of its cell, so it goes under the fold:
`:hidden' takes it off the screen and a refresh puts it back, since a
block makes what it shows anew.

A result block stays.  The fold hides the code and the block keeps its
own fold button, so the two fold apart; `overblock-pycell--keep-result-newline'
is what leaves it room.

The advice is global, so this runs on every fold in every outline
buffer while any notebook has the mode on.  It filters on the block
properties rather than on the mode: a buffer can carry blocks with the
mode off — the tests do it, and so does a mode turned off while a
result is on the screen — and the two scans below cost two
interval-tree queries where there is nothing to find."
  (when flag (overblock-pycell--keep-result-newline from to))
  (dolist (block (overblock-in from to 'markdown))
    (overblock-set block :hidden flag)
    (overblock-refresh block)))

(defun overblock-pycell--md-uncomment (text)
  "Strip the comment prefixes from the markdown cell TEXT."
  (replace-regexp-in-string "^# ?" "" text))

(defun overblock-pycell--md-comment (text)
  "Prefix each line of TEXT as a jupytext markdown comment."
  (mapconcat (lambda (l) (if (string-empty-p l) "#" (concat "# " l)))
             (split-string text "\n") "\n"))

(defvar-keymap overblock-pycell-md-map
  :doc "Keymap on rendered markdown cells.
Only the mouse is bound: overblock-pycell binds no keys.  Put your own here;
`overblock-pycell-md-edit' and `overblock-pycell-md-follow-link' are the natural
candidates.  Point never enters the rendering, so a key pressed on the
cell is answered by this map through the overlays that carry it."
  "<mouse-2>" #'overblock-pycell-md-edit
  "<mouse-1>" #'overblock-pycell-md-raw)

(defun overblock-pycell--md-links (block)
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
(defun overblock-pycell-md-follow-link ()
  "Follow a link of the rendered markdown cell at point.
Clicking a link follows it already: a click is answered by the string
it lands on, and the rendering carries shr's own keymap there.  Point
cannot be put on a link at all — it never enters a display string, and
the row under it belongs to the source, not to the rendering — so this
asks the cell for its links instead.  With one, it is followed; with
several, the reader chooses."
  (interactive)
  (let* ((block (overblock-pycell--md-at nil))
         (links (overblock-pycell--md-links block)))
    (cond
     ((null links) (user-error "No link in this cell"))
     ((null (cdr links)) (browse-url (cdar links)))
     (t (browse-url
         (cdr (assoc (completing-read "Follow link: " (mapcar #'car links)
                                      nil t)
                     links)))))))

(defun overblock-pycell--md-show (beg end &optional html)
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
  (when-let* (;; Still a markdown cell: the line above BEG is the
              ;; boundary that says so, and an edit of that line drops
              ;; the cell's block.  `overblock-edit-commit' rewrites the
              ;; body and renders it again, and a reader who changed
              ;; the boundary in the notebook meanwhile reached
              ;; `overblock-pycell--md-block' with no start for its bar.
              ((overblock-pycell--md-cell-start beg))
              (rendered (let ((overblock-md-width (overblock-md-columns)))
                          (overblock-md-rendered
                           (overblock-pycell--md-uncomment
                            (buffer-substring-no-properties beg end))
                           html))))
    (overblock-pycell--md-block beg end rendered)))

(defun overblock-pycell--md-bar (hov)
  "Draw the bar HOV of a rendered markdown cell, or draw it again.
The bar is an overlay on the boundary line above the cell, which the
block keeps under `:bar'.  It is remade rather than the cell rendered
again when the window changes width: the rendering does not depend on
the width, and the label of the bar does."
  (when (overlay-buffer hov)
    ;; The overlay does not grow at its end, so a title typed at the end
    ;; of the boundary line fell outside it: the label was read from the
    ;; stale region and the text beyond it drew after the bar, which
    ;; made the row two rows.  The code and source bars move theirs to
    ;; the line first; this one now does too.
    (save-excursion
      (goto-char (overlay-start hov))
      (move-overlay hov (pos-bol) (pos-eol)))
    (overblock-bar-draw hov 'markdown
                        (concat (overblock-glyph "" "◇" "M") " "
                                (or (overblock-pycell--cell-title (overlay-start hov)
                                                        (overlay-end hov))
                                    "markdown"))
                        (overblock-buttons overblock-pycell-markdown-buttons)
                        'overblock-pycell-header)))

(defun overblock-pycell--md-block (beg end rendered)
  "Show RENDERED over the markdown cell BEG..END, with a bar above it.
See `overblock-pycell--md-show', which renders and calls this."
  (let* ((start (1- beg))
         (help "RET/mouse-2: edit this markdown cell, mouse-1: show source")
         (text (overblock-fill-props
                (overblock-faced rendered 'default)
                'keymap overblock-pycell-md-map 'help-echo help))
         ;; The bar covers the boundary line and stops before the
         ;; newline where the cell begins, so the line reads as the
         ;; header of the cell and not as a comment with a bar after it.
         ;; Whatever bar the line carries goes first: the cell may have
         ;; been showing its source, which is barred too.
         (hov (let ((from (overblock-pycell--md-cell-start beg)))
                (overblock-pycell--sole-bar from start nil)
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
                                :keymap overblock-pycell-md-map
                                :help-echo help
                                :attached (list hov))))
    (overlay-put hov 'keymap overblock-pycell-md-map)
    ;; A click on the bar lands on this overlay, so it points back at
    ;; the block, which knows the bounds of the cell.
    (overlay-put hov 'overblock-pycell-main block)
    (overblock-set block :bar hov)
    (overblock-pycell--md-bar hov)
    ;; An edit of the source takes the rendering with it, the bar
    ;; included.  The block itself evaporates with the text it covers,
    ;; and the bar sits on the boundary line above, where no edit of the
    ;; cell reaches it: it would be left behind, and `overblock-edit-commit'
    ;; would draw a second bar beside it.
    (overblock-pycell--stale-when-edited block)
    block))

(defun overblock-pycell--md-cells (beg end)
  "Return the body of every markdown cell between BEG and END, in order.
Each is a cons of where the body starts and where it ends, which is the
next boundary line or the end of the buffer.  A cell with nothing in it
is left out: there is nothing to render."
  (save-excursion
    (goto-char beg)
    (let (cells)
      (while (re-search-forward (concat "^" overblock-pycell--md-boundary) end t)
        (forward-line 1)
        (let ((from (point))
              (to (if (re-search-forward code-cells-boundary-regexp nil t)
                      (pos-bol)
                    (point-max))))
          (when (< from to) (push (cons from to) cells))
          ;; Never past the bound: `re-search-forward' signals on a
          ;; bound behind point, whatever its NOERROR says, and a cell
          ;; that reaches past END would leave point there.
          (goto-char (min to end))))
      (nreverse cells))))

;;;###autoload
(defun overblock-pycell-md-render-all (&optional beg end)
  "Render the markdown cells between BEG and END, the whole buffer by default.
A markdown cell is one whose boundary line reads \"# %% [markdown]\".
A caller that knows which cells changed says so: measured, one moved
cell in a file of two hundred rendered every one of them, 436
milliseconds against 17.7 for the two that moved."
  (interactive)
  (let* ((found (overblock-pycell--md-cells (or beg (point-min))
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
                                 (overblock-pycell--md-uncomment
                                  (buffer-substring-no-properties
                                   (car cell) (cdr cell))))
                               cells)))))
      (dolist (cell cells)
        (overblock-pycell--md-show (car cell) (cdr cell) (pop htmls))))
    (when (and found (not cells))
      (message "overblock-pycell: %s, cells stay plain"
               (if (fboundp 'libxml-parse-html-region)
                   (format "no markdown converter found (%s)"
                           (string-join (ensure-list overblock-md-command)
                                        ", "))
                 "this Emacs was built without libxml, which shr reads \
the converter's HTML with")))))

;;;###autoload
(defun overblock-pycell-md-unrender ()
  "Show all markdown cells as their plain source again."
  (interactive)
  (overblock-clear (point-min) (point-max) 'markdown)
  ;; The bar of a rendering goes with it, and no text changed, so
  ;; nothing else would draw the bar those lines want now.
  (without-restriction
    (overblock-pycell--cell-bars (point-min) (point-max)))
  ;; And whatever lost its anchor: a block whose anchor evaporated with
  ;; the line it hung on leaves its parts behind, and a clear that names
  ;; a kind cannot sweep them — an orphan says nothing about the kind it
  ;; belonged to.  This is the command a reader reaches for when a
  ;; rendering looks wrong, so it takes them too.  The results stay: a
  ;; bare `overblock-clear' here took every one of them with it.
  (overblock-sweep-orphans))

(defun overblock-pycell--md-at (event)
  "Return the markdown block at point, or at the click in EVENT.
A click on the bar lands on the small overlay that draws it, which
points back at the block.  Signals a `user-error' where there is no
rendered cell, which is the answer the commands that call it give."
  (overblock-goto-event event)
  (or (overblock-at 'markdown)
      ;; A click on the bar lands beside the block, so the overlay that
      ;; drew it is asked next.
      (seq-some (lambda (ov) (overlay-get ov 'overblock-pycell-main))
                (overlays-in (max (1- (point)) (point-min))
                             (min (1+ (point)) (point-max))))
      (user-error "No rendered markdown cell here")))

;;;###autoload
(defun overblock-pycell-md-render-cell (&optional event)
  "Render the markdown cell at point, or the one whose button EVENT clicked.
`overblock-pycell-md-render-all' does the whole buffer; this is the button on
the
bar of a cell that is showing its source."
  (interactive (list last-input-event))
  (overblock-goto-event event)
  (pcase-let ((`(,beg ,end) (code-cells--bounds nil nil t)))
    (unless (overblock-pycell--md-cell-start beg)
      (user-error "This is not a markdown cell"))
    (overblock-pycell--md-show beg end)))

;;;###autoload
(defun overblock-pycell-md-raw (&optional event)
  "Show the markdown cell at point, or the one in EVENT, as plain source.
The cell is then editable in place; press the button on its bar, or run
`overblock-pycell-md-render-all', to render it again."
  (interactive (list last-input-event))
  (overblock-pycell--drop-rendering (overblock-pycell--md-at event)))

(defun overblock-pycell--md-put (beg end md)
  "Write the edited MD back into the markdown cell BEG..END and render it.
The cell reaches to the next boundary line, so it holds the blank line
jupytext writes between cells: what stood after the body goes back
rather than one newline, or committing an edit that changed nothing
would close the gap.

An empty cell has no line to comment — `overblock-pycell--md-comment' would
write
a bare # where the author left nothing, and a commit that changed
nothing would change the file."
  (let ((tail (buffer-substring-no-properties
               (save-excursion
                 (goto-char end)
                 (skip-chars-backward " \t\n" beg)
                 (point))
               end)))
    (goto-char beg)
    (delete-region beg end)
    (insert (if (string-empty-p md) "" (overblock-pycell--md-comment md)) tail))
  (overblock-pycell--md-show beg end))

;;;###autoload
(defun overblock-pycell-md-edit (&optional event)
  "Edit the markdown cell at point, or the one clicked in EVENT.
The body opens in its own buffer, without the comment prefixes, in
`markdown-mode' when that is installed.  `overblock-edit-commit' puts
it back and renders it; `overblock-edit-abort' discards the edit."
  (interactive (list last-input-event))
  (pcase-let* ((block (overblock-pycell--md-at event))
               (`(,beg . ,end) (overblock-get block :data)))
    (overblock-edit-in-buffer
     beg end
     (list :name (overblock-pycell--cell-buffer-name "md" beg)
           :label "markdown cell"
           :mode (if (fboundp 'markdown-mode) #'markdown-mode #'text-mode)
           ;; Trimmed on the right: the cell reaches to the next
           ;; boundary line, so it holds the blank line jupytext writes
           ;; between cells.  With that line in the edit buffer a
           ;; paragraph typed at the end landed after it, and the
           ;; commit put the gap back below — three comment lines a
           ;; round, compounding.
           :text (lambda (from to)
                   (string-trim-right
                    (overblock-pycell--md-uncomment
                     (buffer-substring-no-properties from to))))
           :put #'overblock-pycell--md-put))))

;;;; The bar over a boundary line

(defun overblock-pycell--cell-title (bol eol)
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

(defun overblock-pycell--bar-redraw (ov)
  "Draw the bar OV again, of whichever kind of cell it belongs to."
  (pcase (overblock-bar-kind ov)
    ('code (overblock-pycell--code-bar (overlay-start ov) (overlay-end ov)))
    ('source (overblock-pycell--source-bar (overlay-start ov) (overlay-end ov)))
    ('markdown (overblock-pycell--md-bar ov))))

(defun overblock-pycell--bar-line (bol eol kind glyph plain buttons)
  "Draw the bar of KIND over the boundary line BOL..EOL.
GLYPH is what stands in front of the label, PLAIN the label of a cell
with no title of its own, and BUTTONS the buttons of the bar.  A bar of
another kind on that line is not taken over: the bar of a rendered
markdown cell belongs to its block, and drawing a code bar on it left
the block holding an overlay that was no longer its own."
  (let* ((there (overblock-bar-in bol (min (point-max) (1+ eol))))
         (ov (if (eq (overblock-bar-kind there) kind)
                 there
               (overblock-bar-over bol eol))))
    ;; Text typed at the end of the line is outside the overlay, and the
    ;; bar then covered a boundary line only as far as it reached when
    ;; the line was shorter.
    (move-overlay ov bol eol)
    (overblock-bar-draw ov kind
                        (concat glyph " " (or (overblock-pycell--cell-title bol eol)
                                              plain))
                        (overblock-buttons buttons)
                        'overblock-pycell-header)))

(defun overblock-pycell--source-bar (bol eol)
  "Draw the bar of the markdown cell BOL..EOL that is showing its source.
A rendered markdown cell is barred by its rendering; a cell that has
none — one just written, or one taken back to its source — had no bar at
all, and the line read as one the package had lost track of."
  ;; "source" and not "markdown": the glyph says markdown, and a
  ;; rendered cell and one showing its source read alike otherwise —
  ;; captured in a terminal, two bars saying `M markdown' that differed
  ;; only in their buttons.
  (overblock-pycell--bar-line bol eol 'source
                    (overblock-glyph "" "◇" "M") "source"
                    overblock-pycell-source-buttons))

(defun overblock-pycell--code-bar (bol eol)
  "Draw the bar of the code cell whose boundary line is BOL..EOL."
  (overblock-pycell--bar-line bol eol 'code
                    (overblock-glyph "" "◆" "%") "python"
                    overblock-pycell-cell-buttons))

(defun overblock-pycell--drop-bar (bar)
  "Take BAR down, and the rendering it belongs to where it has one.
A markdown bar is a block's own overlay: the block goes with it, and
the source of the cell comes back.  Its boundary line no longer says
=[markdown]=, so the rendering below it is a rendering of nothing."
  (if-let* ((block (overlay-get bar 'overblock-pycell-main)))
      (overblock-delete block)
    (delete-overlay bar)))

(defun overblock-pycell--cell-bars (start end)
  "Draw the bar of every code cell whose boundary line START..END touches.
Whole lines, whatever START and END are: this is called with the bounds
of a change, and a change reaches the middle of a line.

A markdown cell has a bar of its own where it is rendered, and none
where it shows its source: its boundary line is left out here, and a
bar left over from before the line said =[markdown]= goes."
  (save-excursion
    (let ((from (progn (goto-char (min start end)) (pos-bol)))
          (to (progn (goto-char (max start end)) (pos-eol))))
      ;; The lines that carry a bar already: one of them may have
      ;; stopped being a boundary line, and its bar has to come down.
      ;; A question about the few lines with bars, not about every line
      ;; of the range.
      (dolist (bar (seq-filter #'overblock-bar-kind
                               (overlays-in from (min (point-max) (1+ to)))))
        (when-let* ((pos (overlay-start bar)))
          (goto-char pos)
          (forward-line 0)
          (overblock-pycell--bar-this-line)))
      ;; And the boundary lines themselves, searched for rather than
      ;; walked to: a `revert-buffer' or a jupytext round trip reports
      ;; one change over the whole buffer, and a line-by-line walk then
      ;; asked `looking-at-p' and `overlays-in' of every line of it.
      (goto-char from)
      ;; Point first, then the search: `forward-line' below can carry
      ;; point past TO, and a bound behind point is an error rather
      ;; than an empty answer.
      (while (and (< (point) to)
                  (re-search-forward code-cells-boundary-regexp to t))
        (forward-line 0)
        (overblock-pycell--bar-this-line)
        (forward-line 1)))))

(defun overblock-pycell--sole-bar (bol eol kinds)
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
      (unless (eq bar keep) (overblock-pycell--drop-bar bar)))
    keep))

(defun overblock-pycell--bar-this-line ()
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
      (overblock-pycell--sole-bar bol eol nil))
     ;; A rendered markdown cell is barred by its rendering, which
     ;; brings its own bar; one showing its source is barred here, with
     ;; the button that renders it.
     ((looking-at-p overblock-pycell--md-boundary)
      (let ((bar (overblock-pycell--sole-bar bol eol '(markdown source))))
        ;; The rendering's own bar is drawn again, not merely left
        ;; alone: its label is the cell's title, and a title edited on
        ;; the line stayed on the bar until a width change or the next
        ;; rendering.
        (if (eq (overblock-bar-kind bar) 'markdown)
            (overblock-pycell--md-bar bar)
          (overblock-pycell--source-bar bol eol))))
     (t
      ;; A rendering whose line stopped saying =[markdown]= is a
      ;; rendering of nothing, and the bar of a source that is no longer
      ;; markdown is a bar for a cell that has gone: both come down, and
      ;; the line takes a code bar like any other.
      (overblock-pycell--sole-bar bol eol '(code))
      (overblock-pycell--code-bar bol eol)))))

(defun overblock-pycell--bars-after-change (beg end _length)
  "Draw the bars of the lines the change BEG..END touched.
On `after-change-functions', and not on `jit-lock-register': one error
in any other jit-lock function skips the rest of them, and a
`python-ts-mode' buffer whose grammar does not match the mode signals
from redisplay — not one bar was drawn in such a buffer.

The match data is the caller's: a change hook runs between a search and
what the searcher does with it, and `replace-match' after a
`search-forward' signalled here."
  (save-match-data (overblock-pycell--cell-bars beg end)))


;;;; Running cells

(defconst overblock-pycell--error-tail
  (concat "\\`"
          "\\(?:[a-z][a-zA-Z0-9_]*\\.\\)*"       ; a module path, if any
          "[A-Z][a-zA-Z0-9_]*"                   ; the exception's name
          "\\(?:Error\\|Exception\\|Exit\\|Interrupt\\|Iteration\\)"
          ":")
  "What the last line of failed output looks like.
The name of an exception, and nothing before it.

The colon is required: a cell whose own output ends with the name of an
exception — `print(type(err).__name__)' after catching one — stopped a
whole pass.  The two names IPython does print alone are
`overblock-pycell--error-alone'.")

(defconst overblock-pycell--error-alone
  "\\`\\(?:KeyboardInterrupt\\|SystemExit\\)\\'"
  "The exceptions IPython reports with nothing after the name.
An interrupted cell ends with a bare `KeyboardInterrupt', and
`sys.exit()' with a bare `SystemExit'; every other report carries a
colon and a message.")

(defun overblock-pycell--error-p (text)
  "Return non-nil when TEXT is the output of a cell that failed.
A traceback says so in its first line, but not every failure has one:
`SyntaxError' and `SystemExit' print the name of the exception and
nothing else, and a pass ran happily past a cell holding `x = = 1'.
So the last line of the output answers as well — that is where the name
of the exception stands, whether a traceback led to it or not."
  (or (string-match-p "Traceback (most recent call last)" text)
      (when-let* ((lines (split-string (string-trim-right text) "\n" t "[ \t\r]+"))
                  (last (car (last lines))))
        (or (string-match-p overblock-pycell--error-tail last)
            (string-match-p overblock-pycell--error-alone last)))))



(defun overblock-pycell--ipython-syntax-p (beg end)
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

(defun overblock-pycell--send-to-ipython (proc code)
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

(defun overblock-pycell--send-region (proc beg end)
  "Send BEG..END to PROC, as the backend's `:send'.
A cell of IPython syntax goes down IPython's own reader; every other
one is padded by `python-shell-send-region', so traceback line numbers
match the buffer."
  (if (overblock-pycell--ipython-syntax-p beg end)
      (overblock-pycell--send-to-ipython
       proc (buffer-substring-no-properties beg end))
    (python-shell-send-region beg end)))

(defun overblock-pycell--start ()
  "Start an inferior Python, and answer nil: it will only prompt later.
The backend's `:start'.  Nil is what tells the runner to arm whatever
was asked for on that first prompt rather than send it now."
  (run-python nil (overblock-pycell--dedicated))
  nil)

(defun overblock-pycell--arm (thunk)
  "Call THUNK on the first prompt of this notebook's Python shell.
The backend's `:arm'.  Evaluation may only start once the fresh
interpreter has prompted — and after the setup of comint-mime, which runs
off the same hook, hence the depth.  A shell that answers with an error
here leaves nothing armed, which is what the caller relies on.

The hook is local and the function takes itself off it again, so a
second pass arms a second thunk and no stale one is left behind."
  (with-current-buffer (process-buffer (python-shell-get-process-or-error))
    (letrec ((once (lambda ()
                     (remove-hook 'python-shell-first-prompt-hook once t)
                     (funcall thunk))))
      (add-hook 'python-shell-first-prompt-hook once 90 t))))

(defun overblock-pycell--step ()
  "Run the cell at point, and say whether a prompt has to come back first.
The backend's `:step', which is how `overblock-run-next' walks a pass
down the notebook.  A markdown cell is rendered here and there is no
prompt to wait for, so the walk goes straight on to the cell after it.

A markdown cell that already shows its rendering needs no second one: a
restart leaves the renderings alone, and rendering them again is a
converter process a cell — measured over thirty cells, 198
milliseconds against the 32 the batch costs."
  (pcase-let ((`(,beg ,end) (code-cells--bounds nil nil t)))
    (unless (and (overblock-pycell--md-cell-start beg)
                 (overblock-in beg end 'markdown))
      (overblock-pycell-eval-region beg end))
    (not (overblock-pycell--md-cell-start beg))))

(defun overblock-pycell--backend ()
  "Return what `overblock-run' needs to drive an inferior Python.
The commentary of `overblock-run' lists the slots."
  (list :name "overblock-pycell"
        :process #'python-shell-get-process
        :start #'overblock-pycell--start
        :arm #'overblock-pycell--arm
        :send #'overblock-pycell--send-region
        :prompt-p #'python-shell-comint-end-of-output-p
        :clean #'overblock-pycell--clean
        :head #'overblock-pycell--output-head
        :show #'overblock-pycell--show
        :error-p #'overblock-pycell--error-p
        :step #'overblock-pycell--step
        :done #'overblock-pycell--follow-done))

(defun overblock-pycell--dedicated ()
  "Return what a new shell is dedicated to, as the reader asked.
`python-shell-dedicated' says it.  Its `project' value makes
`run-python' ask which project, where the file belongs to none, and
that is no question to put in front of a reader who evaluated a cell —
one of a whole run, at that.  `python-shell-get-process-name' names
such a shell the shared one anyway, so that is what this answers."
  (unless (and (eq python-shell-dedicated 'project)
               (not (project-current)))
    python-shell-dedicated))

(defun overblock-pycell-eval-region (start end)
  "Evaluate START..END as a cell and mirror the output below it.
This matches the calling convention of
`code-cells-eval-region-commands'.  A markdown cell renders instead.
Without an interpreter, one starts and the cell follows on its first
prompt.  A cell sent while another one runs is refused, with a
`user-error' from `overblock-run-send'."
  ;; ponytail: a second cell sent by hand is refused rather than
  ;; queued; `overblock-run--queue' serves
  ;; `overblock-pycell-restart-and-run-all' alone.
  (if (overblock-pycell--md-cell-start start)
      ;; Keep a running restart-and-run-all chain going — no prompt
      ;; will arrive to do it.
      (progn
        (overblock-pycell--md-show start end)
        ;; Redisplay pushes a point that the block just made
        ;; invisible out of it, and upwards; put it below instead.
        (when (<= (1- start) (point) end)
          (goto-char end)))
    (overblock-run-region start end)))

;;;###autoload
(defun overblock-pycell-interrupt ()
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
another notebook's run at a keystroke, with no message and nothing to
undo it."
  (interactive)
  ;; Whether this buffer is a pop-out, not whether its shell is there:
  ;; a pop-out that came from a shell since gone has the variable set
  ;; to nil, and it must say so rather than interrupt a stranger.
  (if (not (local-variable-p 'overblock-pycell--shell))
      (interrupt-process (python-shell-get-process-or-error))
    (unless (buffer-live-p overblock-pycell--shell)
      (user-error "The shell this result came from is gone"))
    (let* ((run (buffer-local-value 'overblock-run--state overblock-pycell--shell))
           (beg (plist-get run :beg)))
      ;; Both have to point somewhere.  A killed notebook leaves the
      ;; run's marker and this buffer's — the same object — pointing
      ;; nowhere, and `eq' on two nil buffers passed the test while `='
      ;; signalled "Marker does not point anywhere".
      (unless (and beg overblock-pycell--cell
                   (marker-buffer beg)
                   (eq (marker-buffer beg) (marker-buffer overblock-pycell--cell))
                   (= beg overblock-pycell--cell))
        (user-error "The cell this buffer shows is not running"))
      (interrupt-process
       (or (get-buffer-process overblock-pycell--shell)
           ;; `interrupt-process' of nil takes the current buffer's
           ;; process, which is not this buffer's business.
           (user-error "The shell this result came from has no process"))))))

(defun overblock-pycell--clear-results ()
  "Take the results of the buffer down, and sweep what lost its anchor.
The renderings stay.  A clear that names a kind cannot sweep an orphan —
an orphan says nothing about the kind it belonged to — so the sweep is
asked for by name here: taking the results down with
`overblock-clear' alone left a cloak of a lost block keeping lines of
the buffer invisible, with nothing able to remove it."
  (overblock-clear nil nil 'result)
  (overblock-sweep-orphans))

;;;###autoload
(defun overblock-pycell-restart ()
  "Restart the Python interpreter and remove every result.
The rendered markdown cells stay.  They were taken down with the
results, on the grounds that a rendering is a block like any other, and
that cost a whole notebook its renderings:
`overblock-pycell-restart-and-run-all'
puts them back one cell at a time as the pass reaches them, so a pass
that stops — at an error, or on `overblock-pycell-stop' — leaves every cell
after
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
          (overblock-run-abort "The interpreter was restarted"))
        (overblock-run-queue-set nil)
        (overblock-pycell--clear-results)
        (python-shell-restart))
    (overblock-run-queue-set nil)
    (overblock-pycell--clear-results)
    (run-python nil (overblock-pycell--dedicated))))

;;;###autoload
(defun overblock-pycell-stop (&optional event)
  "Stop the run of the cells after the current one.
Both passes go through the same queue: `overblock-pycell-restart-and-run-all'
and
`overblock-pycell-run-above'.  The cell that is already running runs to its end;
`overblock-pycell-interrupt' is the harder stop.

The queue lives with the shell, and the shell is resolved as
`overblock-pycell-interrupt' resolves it: a popped-out result remembers the one
it came from, and every other buffer asks `python-shell-get-process'.
EVENT is the click on a stop button, and names the notebook to act on."
  (interactive (list last-input-event))
  (overblock-goto-event event)
  (let* ((shell (if (local-variable-p 'overblock-pycell--shell)
                    overblock-pycell--shell
                  (overblock-run-shell)))
         ;; What was queued says what to report: a stop pressed with
         ;; nothing left to run said a pass had been stopped that was
         ;; already over.
         (queued (if (buffer-live-p shell)
                     (length (buffer-local-value 'overblock-run--queue shell))
                   0)))
    (when (buffer-live-p shell)
      (with-current-buffer shell (setq overblock-run--queue nil)))
    (message (if (> queued 0)
                 "overblock-pycell: the pass is stopped, %d cells left unrun"
               "overblock-pycell: nothing was queued")
             queued)))

(defun overblock-pycell--cell-starts ()
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

;;;###autoload
(defun overblock-pycell-run-cell (&optional event)
  "Run the cell at point, or the one whose button EVENT clicked.
The same as `code-cells-eval' on that cell, which is what the reader
presses \\[code-cells-eval] for."
  (interactive (list last-input-event))
  (overblock-goto-event event)
  (apply #'code-cells-eval (code-cells--bounds nil nil t)))

;;;###autoload
(defun overblock-pycell-run-above (&optional event)
  "Run every cell above the one at point, or above the one EVENT clicked.
The cells run in order and the pass stops at the first error, or on
`overblock-pycell-stop'.
The interpreter keeps what it has: `overblock-pycell-restart-and-run-all' is the
one that starts from nothing."
  (interactive (list last-input-event))
  (overblock-goto-event event)
  (let* ((beg (car (code-cells--bounds)))
         (cells (seq-take-while (lambda (m) (< m beg)) (overblock-pycell--cell-starts))))
    (unless cells (user-error "No cell above this one"))
    (overblock-run-cells cells "overblock-pycell: evaluating the cells above")))

;;;###autoload
(defun overblock-pycell-restart-and-run-all ()
  "Restart the Python interpreter, then evaluate every cell in order.
The pass stops at the first error, or on `overblock-pycell-stop'."
  (interactive)
  (overblock-pycell-restart)
  ;; The same arming `overblock-run-cells' does for a shell that is
  ;; starting: a restarted shell has a live process that has not
  ;; prompted, so the queue waits for that prompt here too.
  (overblock-run-on-prompt (overblock-pycell--cell-starts)
                         "overblock-pycell: evaluating all cells"))

(defvar-keymap overblock-pycell-mode-map
  :doc "Keymap of `overblock-pycell-mode', empty on purpose.
overblock-pycell binds no keys; put your own here.  `overblock-pycell-interrupt'
and
`overblock-pycell-stop' are the natural candidates, beside the commands the
README lists:

  (keymap-set overblock-pycell-mode-map \"C-c C-k\" #\\='overblock-pycell-interrupt)")

;;;###autoload
(define-minor-mode overblock-pycell-mode
  "Show Python cell results, and markdown cells, inline.
While the mode is on, cell evaluation goes through
`overblock-pycell-eval-region'.  Turn it off to remove all blocks and to
get plain `python-shell-send-region' back.  The mode binds no
keys: `overblock-pycell-mode-map' is empty and yours to fill."
  ;; The :lighter also keeps the body out of the deprecated
  ;; positional INIT-VALUE argument.
  :lighter " overblock-pycell"
  (if overblock-pycell-mode
      (progn
        ;; What the runner reads to know this is a notebook it may draw
        ;; in, and how to reach its interpreter.
        (setq-local overblock-run-backend (overblock-pycell--backend))
        ;; One piece of advice for the session, put on by the first
        ;; notebook and taken off by the last.  Added while this file
        ;; loaded, it changed how `outline-flag-region' behaves in every
        ;; outline buffer of a session that had never turned the mode on
        ;; — and completing the name of one command loads the file.
        (advice-add 'outline-flag-region :after
                    #'overblock-pycell--outline-flag-blocks)
        ;; A bar is cut to the width of the window it was built for, so
        ;; a window made narrower afterwards wants it drawn again.
        (add-hook 'window-configuration-change-hook #'overblock-pycell--rewidth nil t)
        (add-hook 'text-scale-mode-hook #'overblock-pycell--rescale nil t)
        (add-hook 'after-change-functions #'overblock-pycell--bars-after-change nil t)
        ;; The whole buffer, narrowed or not: a mode turned on under a
        ;; narrowing would otherwise bar the visible cells alone, and
        ;; the rest only when something edited them.
        (without-restriction
          (overblock-pycell--cell-bars (point-min) (point-max)))
        (overblock-pycell-md-render-all))
    (kill-local-variable 'overblock-run-backend)
    (remove-hook 'window-configuration-change-hook #'overblock-pycell--rewidth t)
    (remove-hook 'text-scale-mode-hook #'overblock-pycell--rescale t)
    (remove-hook 'after-change-functions #'overblock-pycell--bars-after-change t)
    (mapc #'delete-overlay (overblock-bars))
    ;; Every kind of block goes, rendered markdown cells included.
    (overblock-pycell-remove-blocks)
    ;; The last notebook takes the advice with it.  This buffer does not
    ;; count itself: the mode's own variable is already nil here.
    (unless (seq-some (lambda (buffer)
                        (buffer-local-value 'overblock-pycell-mode buffer))
                      (buffer-list))
      (advice-remove 'outline-flag-region #'overblock-pycell--outline-flag-blocks))))

;;;###autoload
(defun overblock-pycell-mode-maybe ()
  "Enable `overblock-pycell-mode' in Python cell buffers.
Made for `code-cells-mode-hook', where your configuration adds it:

  (add-hook \\='code-cells-mode-hook #\\='overblock-pycell-mode-maybe)

The package installs no hook itself: installing it must not change
how Emacs behaves."
  (when (derived-mode-p 'python-base-mode)
    (overblock-pycell-mode)))

;; Keyed on the minor mode: with it off, code-cells falls through to its
;; stock python entry, `python-shell-send-region'.
(setf (alist-get 'overblock-pycell-mode code-cells-eval-region-commands)
      #'overblock-pycell-eval-region)

(provide 'overblock-pycell)
;;; overblock-pycell.el ends here
