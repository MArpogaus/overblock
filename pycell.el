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
(require 'comint-mime)
;; comint-mime renders a table with it, and a block lays that table
;; out again.  Optional, as it is in comint-mime: an Emacs without
;; vtable shows the text of the table as it came.
(require 'vtable nil t)
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
  :type overblock-button-type)

(defcustom pycell-markdown-buttons
  '((move-up ("󰅃" "⌃" "u") "Move this cell up" pycell-move-cell-up t)
    (move-down ("󰅀" "⌄" "d") "Move this cell down" pycell-move-cell-down t)
    (edit ("󱦴" "↗" "^") "Edit this markdown cell in its own buffer"
          pycell-md-edit t)
    (source ("󰅖" "✕" "x") "Show the plain source" pycell-md-raw t))
  "The buttons on the header of a rendered markdown cell.
The entries read as in `pycell-result-buttons'.  A markdown cell has
no output, so `lines' and `image' say nothing here."
  :type overblock-button-type)

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

(defun pycell-remove-overlays ()
  "Remove the blocks of the buffer.
This is the command a reader binds, and `overblock-clear\=' is the same
thing under it.  Results and rendered markdown cells go; the text of
the buffer is not touched."
  (interactive)
  (overblock-clear))

(defun pycell--stale-when-edited (block)
  "Take BLOCK down on the next edit of the text it covers.
Three hooks and not one: `modification-hooks\=' runs for a change inside
an overlay, `insert-in-front-hooks\=' for one at its first character and
`insert-behind-hooks\=' for one at its end.  A block's anchor stops one
character short of the cell's last newline, so the blank line that ends
a cell — where `C-e\=' on the last line puts point — is an insertion at
the end: with `modification-hooks\=' alone the block stayed behind,
showing the result of text that had changed under it."
  (let ((drop (list (lambda (ov &rest _) (overblock-delete ov)))))
    (overlay-put block 'modification-hooks drop)
    (overlay-put block 'insert-in-front-hooks drop)
    (overlay-put block 'insert-behind-hooks drop)))

;;;; Result blocks

(defun pycell--strip-prompts (text)
  "Return TEXT without the shell\'s prompts and its Out[N] labels.
The prompt before the output goes, the prompt after it goes, and so does
the one that ends up on the same line as output which stopped without a
newline — `comint-prompt-regexp\=' anchors to a line start and cannot see
that one.  An `Out[N]:\=' label goes where it begins a line, which is
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
the shell; see `pycell--strip-prompts\=' and `overblock-repl-detach\=' for what
each of those means.  Call this in the shell buffer, where
`comint-prompt-regexp\=' has its value."
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
time in seconds since the cell started.  STATE is `running\=' while the
cell runs, `died\=' where the interpreter went away before the cell
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

(defvar-keymap pycell-overlay-map
  :doc "Keymap inside a cell that shows a result."
  "TAB" '(menu-item "" pycell-toggle-output :filter pycell--tab-filter))

(defun pycell--show (beg end text runtime &optional state total)
  "Show TEXT as the result of the cell BEG..END.
RUNTIME is the time in seconds since the cell started.  STATE is
`running\=' while the cell runs, `died\=' where the interpreter went away
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
      ;; last cell of the buffer one.
      (when (and (= end (point-max)) (not (eq (char-before end) ?\n)))
        (save-excursion (goto-char end) (insert "\n")))
      (let ((block (overblock-show beg end
                                   :kind 'result
                                   :data data
                                   :keymap pycell-overlay-map)))
        ;; An edit of the cell makes the result stale; it goes.
        (pycell--stale-when-edited block)
        (pycell--update block)
        block))))

(defun pycell--goto-event (event)
  "Select the window of EVENT and move point to the click.
Anything that is not a click leaves point where it is: the commands read
EVENT from `last-input-event\=', so it can be any event at all, and a
`switch-frame\=' is a cons whose start is a frame rather than a place."
  (when-let* (((consp event))
              (posn (event-start event))
              ((consp posn))
              (pos (posn-point posn)))
    (select-window (posn-window posn))
    (goto-char pos)))

(defun pycell--result-at (event)
  "Return the result block at point, or at the click in EVENT.
Point first, then anywhere in the cell around it.  Signals a
`user-error\=' where the cell has no result, which is the answer the
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

;;;###autoload
(defun pycell-move-cell-down (&optional arg)
  "Move the cell at point down ARG cells, with what it shows.
A negative ARG moves it up, which is all `pycell-move-cell-up\=' does.
`code-cells-move-cell-down' transposes the text of the two cells, and
`transpose-regions' leaves an overlay where the text used to be: the
result of one cell would end up under the other.  So both blocks come
off, the text moves, and each block goes back on the cell it belongs
to.  Point travels with the cell, so a click on the button of a
header keeps moving the same cell."
  (interactive "p")
  (setq arg (or arg 1))
  (pcase-let* ((`(,beg ,end) (code-cells--bounds))
               (`(,nbeg ,nend) (code-cells--neighbor-bounds arg))
               (offset (- (point) beg))
               (mine (pycell--cell-state beg end))
               (theirs (pycell--cell-state nbeg nend))
               (down (> nbeg beg))
               (mine-length (- end beg))
               (their-length (- nend nbeg)))
    ;; This signals when there is nowhere to move, before anything is
    ;; taken off.
    (code-cells-move-cell-down arg)
    (overblock-clear (min beg nbeg) (max end nend))
    (let ((mine-beg (if down (- nend mine-length) nbeg))
          (their-beg (if down beg (- end their-length))))
      (pycell--restore-cell mine-beg (+ mine-beg mine-length) mine)
      (pycell--restore-cell their-beg (+ their-beg their-length) theirs)
      (when (or (cdr mine) (cdr theirs))
        ;; The two cells that moved, not every cell in the file: the
        ;; union of the two, where the sum of the larger start and the
        ;; larger length overshot it — and an end inside a later cell
        ;; made `pycell-md-render-all' search with a bound behind point.
        (pycell-md-render-all (min mine-beg their-beg)
                              (max (+ mine-beg mine-length)
                                   (+ their-beg their-length))))
      (goto-char (+ mine-beg (min offset mine-length))))))

;;;###autoload
(defun pycell-move-cell-up (&optional arg)
  "Move the cell at point up ARG cells, with what it shows."
  (interactive "p")
  (pycell-move-cell-down (- (or arg 1))))

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

(defun pycell-pop-output (&optional event)
  "Show the result at point, or the one clicked in EVENT, in a buffer.
Each cell gets one buffer, so results are comparable side by side."
  (interactive (list last-input-event))
  (let* ((ov (pycell--result-at event))
         (text (pycell--text ov))
         (name (pycell--cell-buffer-name nil (overlay-start ov))))
    (with-current-buffer (get-buffer-create name)
      (special-mode)
      (let ((inhibit-read-only t))
        (erase-buffer)
        ;; A table goes in live: every binding of vtable works here, and
        ;; vtable aligns the columns for this window itself.  It goes in
        ;; as a copy, because the table of the result belongs to the
        ;; shell buffer that drew it.
        (if-let* ((table (overblock-repl-table-in text)))
            (vtable-insert (overblock-repl-table-copy table))
          ;; A figure here is one space carrying an image: on a display
          ;; that draws none, this buffer held that space and nothing
          ;; else.
          (insert (if (display-images-p) text
                    (overblock-image-label text)))))
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
FLAG is non-nil where `outline-flag-region\=' hid the region, and this
follows that call rather than guessing which command made it.

A markdown cell is the content of its cell, so it goes under the fold:
`:hidden\=' takes it off the screen and a refresh puts it back, since a
block makes what it shows anew.

A result block stays.  The fold hides the code and the block keeps its
own fold button, so the two fold apart; `pycell--keep-result-newline\='
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
  "<mouse-2>" #'pycell-md-edit
  "<mouse-1>" #'pycell-md-raw)

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

(defun pycell--md-block (beg end rendered)
  "Show RENDERED over the markdown cell BEG..END, with a bar above it.
See `pycell--md-show', which renders and calls this."
  ;; Give the last cell of a file with no final newline one, as
  ;; `pycell--show' does.  Without it the anchor ends at `point-max',
  ;; so the newline `require-final-newline' adds on save is an
  ;; insertion at the anchor's end — and the rendering came off as the
  ;; reader saved the file.
  (when (and (= end (point-max)) (not (eq (char-before end) ?\n)))
    (save-excursion (goto-char end) (insert "\n")))
  (let* ((start (1- beg))
         (help "RET/mouse-2: edit this markdown cell, mouse-1: show source")
         (text (overblock-fill-props
                (overblock-faced rendered 'default)
                'keymap pycell-md-map 'help-echo help))
         ;; The bar covers the word =markdown= of the boundary line and
         ;; nothing else, and stops before the newline where the cell
         ;; begins.
         ;; The boundary line is a markdown boundary because it carries
         ;; the word, so the search cannot fail.
         (hov (make-overlay (save-excursion
                              (goto-char (pycell--md-cell-start beg))
                              (re-search-forward "\\[markdown\\]" (pos-eol))
                              (match-beginning 0))
                            start))
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
    (overlay-put hov 'evaporate t)
    (overlay-put hov 'keymap pycell-md-map)
    ;; A click on the bar lands on this overlay, so it points back at
    ;; the block, which knows the bounds of the cell.
    (overlay-put hov 'pycell-main block)
    ;; A zero-width display property hides the word, and the bar draws
    ;; in its place as a string.  It has to be a string: a display
    ;; string ignores `(space :align-to (- right ...))', and the icons
    ;; then sit next to the label instead of at the window edge.
    (overlay-put hov 'display "")
    (overlay-put hov 'before-string
                 (overblock-bar "markdown"
                                (overblock-buttons pycell-markdown-buttons)
                                'pycell-header))
    ;; An edit of the source takes the rendering with it, the bar
    ;; included.  The block itself evaporates with the text it covers,
    ;; and the bar sits on the boundary line above, where no edit of the
    ;; cell reaches it: it would be left behind, and `pycell-md-commit'
    ;; would draw a second bar beside it.
    (pycell--stale-when-edited block)
    block))

;;;###autoload
(defun pycell-md-render-all (&optional beg end)
  "Render the markdown cells between BEG and END, the whole buffer by default.
A markdown cell is one whose boundary line reads \"# %% [markdown]\".
A caller that knows which cells changed says so: measured, one moved
cell in a file of two hundred rendered every one of them, 436
milliseconds against 17.7 for the two that moved."
  (interactive)
  (let ((program (overblock-md-program))
        (last (or end (point-max)))
        cells missed)
    (save-excursion
      (goto-char (or beg (point-min)))
      (while (re-search-forward (concat "^" pycell--md-boundary) last t)
        (forward-line 1)
        (let ((beg (point))
              (end (if (re-search-forward code-cells-boundary-regexp nil t)
                       (pos-bol)
                     (point-max))))
          (when (< beg end)
            (if program (push (cons beg end) cells) (setq missed t)))
          ;; Never past the bound: `re-search-forward' signals on a
          ;; bound behind point, whatever its NOERROR says, and a cell
          ;; that reaches past END would leave point there.
          (goto-char (min end last)))))
    (setq cells (nreverse cells))
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
    (when missed
      (message "pycell: %s, cells stay plain"
               (if (fboundp 'libxml-parse-html-region)
                   (format "no markdown converter found (%s)"
                           (string-join (ensure-list overblock-md-command)
                                        ", "))
                 "this Emacs was built without libxml, which shr reads \
the converter\'s HTML with")))))

(defun pycell-md-unrender ()
  "Show all markdown cells as their plain source again."
  (interactive)
  (overblock-clear (point-min) (point-max) 'markdown)
  ;; And whatever lost its anchor: a block whose anchor evaporated with
  ;; the line it hung on leaves its parts behind, and a clear that names
  ;; a kind cannot sweep them — an orphan says nothing about the kind it
  ;; belonged to.  This is the command a reader reaches for when a
  ;; rendering looks wrong, so it takes them too.  The results stay: a
  ;; bare `overblock-clear\=' here took every one of them with it.
  (overblock-sweep-orphans))

(defun pycell--md-at (event)
  "Return the markdown block at point, or at the click in EVENT.
A click on the bar lands on the small overlay that draws it, which
points back at the block.  Signals a `user-error\=' where there is no
rendered cell, which is the answer the commands that call it give."
  (pycell--goto-event event)
  (or (overblock-at 'markdown)
      ;; A click on the bar lands beside the block, so the overlay that
      ;; drew it is asked next.
      (seq-some (lambda (ov) (overlay-get ov 'pycell-main))
                (overlays-in (max (1- (point)) (point-min))
                             (min (1+ (point)) (point-max))))
      (user-error "No rendered markdown cell here")))

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
               ;; and `pycell-md-commit\=' put the gap back below —
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
      (when (and pycell-md-edit-mode (buffer-modified-p)
                 (not (equal pycell--md-source (list src beg end)))
                 (not (yes-or-no-p
                       "Discard the unsaved edit of another cell? ")))
        (user-error "Kept the unsaved edit"))
      (unless (and pycell-md-edit-mode (buffer-modified-p)
                   (equal pycell--md-source (list src beg end)))
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

(defun pycell--queued ()
  "Return the cells a run-all still has to run, in order."
  (when-let* ((shell (pycell--queue-buffer)))
    (buffer-local-value 'pycell--queue shell)))

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
  :start  the `float-time\=' of the send
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
makes this cheap: `replace-regexp-in-string\=' copies its argument twice
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
a second time — `cancel-timer\=' of nil raised, which masked the error it
was reporting.  `pycell--abort\=' asks the same question."
  (when pycell--run
    (pcase-let (((map :beg (:end fin) :start :timer) pycell--run))
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
                       (substring tail (max 0 (- (length tail) 256)))))
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
          (when (and (not (nth 3 state)) (not (nth 4 state))
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
`python-shell-dedicated\=' says it.  Its `project\=' value makes
`run-python\=' ask which project, where the file belongs to none, and
that is no question to put in front of a reader who evaluated a cell —
one of a whole run, at that.  `python-shell-get-process-name\=' names
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
`user-error\=' from `pycell--send\='."
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
and prompts again."
  (interactive)
  (interrupt-process (python-shell-get-process-or-error)))

(defun pycell-restart ()
  "Restart the Python interpreter, and remove every result and rendering.
A rendered markdown cell is a block like a result, so it goes too and
shows its source again."
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
        (pycell-remove-overlays)
        (python-shell-restart))
    (pycell--queue-set nil)
    (pycell-remove-overlays)
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
        (unless m (throw 'waiting nil))
        (pycell--queue-set (cdr cells))
        (unless (buffer-live-p (marker-buffer m))
          (pycell--queue-set nil)
          (throw 'waiting nil))
        (with-current-buffer (marker-buffer m)
          (goto-char m)
          (pcase-let ((`(,beg ,end) (code-cells--bounds nil nil t)))
            (pycell-eval-region beg end)
            (unless (pycell--md-cell-start beg)
              (throw 'waiting nil))))))))

(defun pycell-stop ()
  "Stop `pycell-restart-and-run-all' after the current cell."
  (interactive)
  (pycell--queue-set nil)
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
  (pycell--queue-set
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
      (pycell-md-render-all)
    ;; Every kind of block goes, rendered markdown cells included.
    (pycell-remove-overlays)))

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
