;;; overblock-pycell.el --- Inline results for Python code cells -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Marcel Arpogaus

;; Author: Marcel Arpogaus <znepry.necbtnhf@tznvy.pbz>
;; Assisted-by: Claude:claude-opus-5
;; Assisted-by: Claude:claude-fable-5
;; Version: 1.0
;; Package-Requires: ((emacs "29.1") (overblock "1.0") (overblock-md "1.0") (code-cells "0.5"))
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

;; Notebook style results for Python code cells, built on python.el:
;; no Jupyter kernel and no zmq module.  comint-mime, where it is
;; installed, adds figures and tables.
;;
;; Add `overblock-pycell-mode-maybe' to `code-cells-mode-hook' and the mode is
;; on in every Python buffer with cells.  Evaluating a cell sends it
;; to the inferior Python process as usual, so the REPL keeps the full
;; log.  While the cell runs, the result grows below it: a header bar
;; with a spinner, a stopwatch and buttons, and the output as
;; comint-mime rendered it, images included.
;;
;; Markdown cells, the `# %% [markdown]' ones that jupytext writes, are
;; rendered in place.  An external markdown command and shr produce
;; the text, which then hangs on the source lines it replaces, a piece
;; to a line, and latex-to-svg-backend turns the formulas into preview
;; images.  A click on a rendering shows the source, and the cell
;; renders again after the edit, through the live cycle of
;; `overblock-md-preview-mode'.
;;
;; Rich output needs an IPython REPL, because comint-mime installs its
;; renderers there; a plain python3 shell yields text only.
;;
;; `overblock' draws the blocks, `overblock-md' turns markdown into a
;; string, `overblock-repl' cuts the output of a shell loose from it,
;; and `overblock-run' sends a region to a shell, shows the result and
;; holds the commands on a result.  This file knows about Python: the
;; cells, the process, and the commands of a cell.
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
(require 'python)
(require 'seq)
(require 'subr-x)

(defgroup overblock-pycell nil "Inline results for Python code cells."
  :group 'python
  :group 'overblock
  :prefix "overblock-pycell-")

(defconst overblock-pycell--move-buttons
  '((move-up ("" "⌃" "up") "Move this cell up"
             overblock-pycell-move-cell-up t)
    (move-down ("" "⌄" "down") "Move this cell down"
               overblock-pycell-move-cell-down t))
  "The pair that moves a cell, at the end of every bar of a cell.
The buttons are held against the right edge, so only the trailing
slots are in the same column on every bar.")

(defcustom overblock-pycell-result-buttons
  (append (overblock-run-result-buttons "cell" "image")
          overblock-pycell--move-buttons)
  "The buttons on the header of a result, left to right.
An entry has the shape `overblock-buttons' reads.  The five of every
result come from `overblock-run-result-buttons', shared with the Rmd
notebook; the pair that moves a cell belongs to this notebook.

Drop, reorder or change entries as you like.  The fold arrow and the
spinner are not in this list: they show the state of the result."
  :type overblock-button-type
  :set #'overblock-run-set-and-redraw)

(defcustom overblock-pycell-md-buttons
  (append '((edit ("" "✎" "edit") "Edit this markdown cell in its own buffer"
          overblock-pycell-md-edit t))
          overblock-pycell--move-buttons)
  "The buttons on the header of a rendered markdown cell.
An entry has the shape `overblock-buttons' reads.  A markdown cell has
no output, so `lines' and `image' mean nothing here.  There is no
button for the source: a click on the rendering shows it."
  :type overblock-button-type
  :set #'overblock-run-set-and-redraw)

(defcustom overblock-pycell-source-buttons
  (append '((render ("" "⟳" "render") "Render this markdown cell"
            overblock-pycell-md-render-cell t))
          overblock-pycell--move-buttons)
  "The buttons on the bar of a markdown cell that shows its source.
An entry has the shape `overblock-buttons' reads.  Such a cell is new,
or was taken back to its source with `overblock-pycell-md-raw'; the
render button renders it."
  :type overblock-button-type
  :set #'overblock-run-set-and-redraw)

(defcustom overblock-pycell-cell-buttons
  (append '((run-above ("" "⇈" "above") "Run every cell above this one"
               overblock-run-above t)
    (run ("" "▷" "run") "Run this cell" overblock-pycell-run-cell t))
          overblock-pycell--move-buttons)
  "The buttons on the bar of a code cell, left to right.
An entry has the shape `overblock-buttons' reads.  A cell bar is drawn
before the cell runs, so `lines' and `image' mean nothing here.

The two move buttons come last, as on every bar (see
`overblock-pycell--move-buttons')."
  :type overblock-button-type
  :set #'overblock-run-set-and-redraw)

;;;; Blocks of every kind

(defun overblock-pycell--drop-rendering (block)
  "Take BLOCK down, and bar the boundary line a rendering leaves behind.
The bar of a rendered markdown cell is an overlay of the block, so it
goes with the block.  No text of that line changed, so nothing else
puts a bar back."
  (let ((markdown (eq (overblock-get block :kind) 'markdown))
        (start (overlay-start block)))
    (overblock-delete block)
    (when markdown
      (when-let* ((from (overblock-pycell--md-cell-start start)))
        (overblock-pycell--cell-bars from start)))))

(defvar overblock-pycell--moving nil
  "Non-nil while `overblock-pycell-move-cell-down' is moving a cell.
`overblock-pycell--stale-when-edited' does nothing meanwhile: a move
relocates whole cells, and the command removes and restores the blocks
of both cells itself.  The moved text is inserted at the anchor of the
cell below, whose `insert-in-front-hooks' would remove its result.")

(defun overblock-pycell--stale-when-edited (block)
  "Take BLOCK down on the next edit of the text it covers.
Not during a move (see `overblock-pycell--moving').  The rendering and
the bar above a rendered cell come down, through
`overblock-pycell--drop-rendering'."
  (overblock-stale-when-edited
   block (lambda (block)
           (unless overblock-pycell--moving (overblock-pycell--drop-rendering block)))))

;;;; Result blocks

(defun overblock-pycell--strip-prompts (text)
  "Return TEXT without the prompts and the Out[N] labels of the shell.
The prompt before the output goes, the prompt after it goes, and so
does one on the same line as output without a final newline, which
`comint-prompt-regexp' cannot see because it anchors to a line start.
An `Out[N]:' label goes where it starts a line.  Call this in the shell
buffer, where that variable has its value."
  (let ((rx (concat "\\(?:" comint-prompt-regexp "\\)")))
    ;; The (> ...) guard stops an endless loop on an empty match. The
    ;; last guard keeps a figure, which is a space with an image.
    (while (and (string-match (concat "\\`[ \t\n]*" rx) text)
                (> (match-end 0) 0)
                (not (text-property-not-all 0 (match-end 0) 'display nil text)))
      (setq text (substring text (match-end 0))))
    (setq text (overblock-repl-strip-trailing-prompt text comint-prompt-regexp))
    ;; A plain python3 shell leaves a prompt on the same line after a
    ;; `sys.stdout.write' without a newline. Take that one off.
    (when (string-match (concat "\\(?:" (string-remove-prefix
                                         "^" comint-prompt-regexp)
                                "\\)[ \t]*\\'")
                        text)
      (setq text (substring text 0 (match-beginning 0)))))
  ;; The search first: `replace-regexp-in-string' copies the text even
  ;; without a match, and a plain python3 shell writes no label.
  ;;
  ;; Anchored to a line start, so a value that contains "Out[1]: " keeps
  ;; it. A label after output without a final newline stays: it cannot
  ;; be told apart from such a value.
  (if (string-search "Out[" text)
      (replace-regexp-in-string "^Out\\[[0-9]+\\]: " "" text)
    text))

(defun overblock-pycell--drop-prompt-face (text)
  "Return TEXT without the face comint paints a prompt with.
comint calls a chunk of output that ends without a newline a prompt,
and paints it `comint-highlight-prompt'.  A cell that prints one line
arrives as one such chunk.

Only that face goes: ansi-color and comint-mime put other faces in the
same property.  A run left without a face loses the property instead
of a nil value, because each face run costs redisplay time.

TEXT changes in place.  It is the copy from `buffer-substring' that
`overblock-pycell--clean' got."
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
The prompts, the Out[N] labels and the prompt face go, and the copy is
cut loose from the shell: see `overblock-pycell--strip-prompts',
`overblock-pycell--drop-prompt-face' and `overblock-repl-detach'.  Call
this in the shell buffer, where `comint-prompt-regexp' has its value."
  (overblock-repl-detach
   (overblock-pycell--drop-prompt-face (overblock-pycell--strip-prompts text))))

(defun overblock-pycell-tab-filter (cmd)
  "Return CMD when point sits at the very end of a cell with a result.
A `menu-item' filter for a key in `overblock-run-result-map'.  It
keeps a key that means something else in the cell, such as TAB, active
only at the end of the cell, next to the result:

  (keymap-set overblock-run-result-map \"TAB\"
              \\='(menu-item \"\" overblock-run-toggle-output
                          :filter overblock-pycell-tab-filter))"
  (and (eolp)
       (seq-some (lambda (o) (eq (point) (overlay-end o)))
                 (overblock-in (max (1- (point)) (point-min)) (point)
                               'result))
       cmd))

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
STATE comes from `overblock-pycell--cell-state'.  A markdown cell
renders here, not by the live cycle, which leaves the cell at point
alone."
  ;; The whole record: the new block has no state of its own.
  (when-let* ((record (car state))
              (block (overblock-run-show beg end "" 0.0)))
    (overblock-set block :data record)
    (overblock-run-update block))
  (when (cdr state)
    (overblock-pycell--md-show (save-excursion (goto-char beg)
                                               (forward-line 1)
                                               (point))
                               end)))

(defun overblock-pycell--running-in-p (beg end)
  "Return non-nil where the cell the shell is running lies in BEG..END.
Only in this buffer: another notebook on the same shell can be the
one running."
  (when-let* ((running (overblock-run-running-region))
              (mark (car running))
              ((eq (marker-buffer mark) (current-buffer))))
    (<= beg mark end)))

;;;###autoload
(defun overblock-pycell-move-cell-down (&optional arg event)
  "Move the cell at point down ARG cells, with what it shows.
A negative ARG moves it up.  EVENT is the click on a button that asked
for the move.

This is an outline move: `code-cells-mode' makes every boundary line an
outline heading, so a cell is a subtree.  Outline cuts the text and
puts it back.  `transpose-regions', which `code-cells-move-cell-down'
uses, leaves overlays where the text was, and joins the last cell to
the previous one when the file has no final newline.

After the move the blocks of both cells come off, orphans are swept,
and the blocks go back on their cells.  Point moves with the cell, so
repeated clicks move the same cell."
  (interactive (list (prefix-numeric-value current-prefix-arg)
                     last-input-event))
  (setq arg (or arg 1))
  ;; The click first, so the cell of the pressed button moves.
  (overblock-goto-event event)
  (pcase-let* ((`(,beg ,end) (code-cells--bounds))
               (`(,nbeg ,nend) (code-cells--neighbor-bounds arg))
               (offset (- (point) beg))
               (mine (overblock-pycell--cell-state beg end))
               (theirs (overblock-pycell--cell-state nbeg nend)))
    ;; A running cell cannot move: the run holds markers into the text
    ;; that the move cuts out.
    (when (overblock-pycell--running-in-p (min beg nbeg) (max end nend))
      (user-error "Wait for the cell to finish, or M-x overblock-run-interrupt"))
    ;; From the boundary line: `outline-regexp' also holds the headings
    ;; of the major mode, so from inside a cell outline finds a `def'.
    (goto-char beg)
    ;; This signals when there is nowhere to move, before anything is
    ;; removed. Every error puts point back: outline moves point before
    ;; it refuses.
    (let ((overblock-pycell--moving t)
          (here (point-marker)))
      (condition-case error
          (outline-move-subtree-down arg)
        ;; The text before the first boundary line is a cell to
        ;; code-cells and no subtree at all to outline.
        (outline-before-first-heading
         (goto-char here)
         (user-error "Can't move the text above the first cell"))
        ;; In the words of cells, not of outline levels.
        (user-error
         (goto-char here)
         (user-error "No cell to swap this one with"))
        (error
         (goto-char here)
         (signal (car error) (cdr error)))))
    ;; Point is at the moved cell, so the buffer gives both ranges.
    (overblock-clear (min beg nbeg) (max end nend))
    ;; The parts of the moved cell outlived their anchor.
    (overblock-sweep-orphans)
    (pcase-let* ((`(,mbeg ,mend) (code-cells--bounds))
                 (`(,tbeg ,tend) (code-cells--neighbor-bounds (- arg))))
      (overblock-pycell--restore-cell mbeg mend mine)
      (overblock-pycell--restore-cell tbeg tend theirs)
      (goto-char (+ mbeg (min offset (- mend mbeg)))))))

;;;###autoload
(defun overblock-pycell-move-cell-up (&optional arg event)
  "Move the cell at point up ARG cells, with what it shows.
EVENT is the click on a button that asked for the move."
  (interactive (list (prefix-numeric-value current-prefix-arg)
                     last-input-event))
  (overblock-pycell-move-cell-down (- (or arg 1)) event))

;;;; Markdown cells

(defconst overblock-pycell--md-boundary
  "#+[[:blank:]]*%%+[[:blank:]]*\\[markdown\\]"
  "What marks a cell boundary line as a markdown cell.
As loose as `code-cells-boundary-regexp': any number of comment
characters, with or without a space, since VS Code and Spyder write
=#%% [markdown]= where jupytext writes =# %% [markdown]=.  A tag list or
a title can follow, as on a code cell.

The comment character is literal, not from the syntax table, so the
answer is the same in a buffer whose mode is not set yet.")

(defun overblock-pycell--md-cell-start (pos)
  "Return the start of the =# %% [markdown]= line above POS, or nil.
A non-nil value marks POS as the body of a markdown cell."
  (save-excursion
    (goto-char pos)
    (forward-line -1)
    (and (looking-at-p overblock-pycell--md-boundary) (point))))

(defun overblock-pycell--keep-result-newline (from to)
  "Keep the newline a result block hangs on out of a fold over FROM..TO.
A fold that reaches the end of the buffer covers that newline, unlike a
fold in the middle.  The block, with the bar that folds the result,
would go with it, so the invisible run is shrunk back off the
newline."
  (dolist (block (overblock-in from to 'result))
    ;; A deleted overlay has no end, and this runs on every fold.
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
FLAG is non-nil where `outline-flag-region' hid the region.

A rendered markdown cell is the content of its cell, so it goes under
the fold: `:hidden' takes it off the screen, and a refresh shows it
again.

A result block stays: it has its own fold button, so code and result
fold apart.  `overblock-pycell--keep-result-newline' leaves it room.

The advice is global, so this runs on every fold in every outline
buffer while a notebook has the mode on.  It tests the blocks, not the
mode, because a buffer can have blocks with the mode off; with no
blocks the scans are cheap."
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
Only the mouse is bound: overblock-pycell binds no keys.  Put your own
here, for example `overblock-pycell-md-edit' and
`overblock-pycell-md-follow-link'.  Point never enters the rendering,
so the overlays of the cell carry this map."
  "<mouse-2>" #'overblock-pycell-md-edit
  "<mouse-1>" #'overblock-pycell-md-raw)

(defun overblock-pycell--md-links (block)
  "Return the links of the rendering of BLOCK, in the order shown.
Each is a cons of the visible text and the URL.

This reads the whole rendering, not its pieces: a piece that holds an
image shows its row on the before-string and has an empty display
string."
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
A click on a link follows it already, through `overblock-md-link-map'.
Point never enters a display string, so this asks the cell for its
links.
With one, it is followed; with several, the reader chooses."
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
With HTML, the cell is not converted again: it was converted with the
rest of the buffer.

The rendering hangs on the source lines, a piece to a line (see
`overblock--pieces'), so the cell scrolls like text and is as tall as
its source; when the rendering is shorter, a cloak hides the lines
left over.  A cell that renders to nothing uses the single string of a
result block, and hides its source as one invisible run.  That run must
start at the end of a visible line: `scroll-down' fails with a
beginning-of-buffer error over a run that starts at a line start.  So
the =# %%= line stays visible.

Only the word =markdown= of the boundary line carries the header, so
=# %%= looks like every other cell boundary and `outline-minor-mode'
still finds its heading."
  (when-let* (;; Still a markdown cell: the boundary line can change
              ;; while an edit buffer is open.
              ((overblock-pycell--md-cell-start beg))
              ;; An empty cell has no region for a block. This can run
              ;; in the comint filter, where an error is costly.
              ((< beg end))
              (rendered (let ((overblock-md-width (overblock-md-columns)))
                          (overblock-md-rendered
                           (overblock-pycell--md-uncomment
                            (buffer-substring-no-properties beg end))
                           html))))
    (overblock-pycell--md-block beg end rendered)))

(defun overblock-pycell--md-bar (hov)
  "Draw the bar HOV of a rendered markdown cell, or draw it again.
The bar is an overlay on the boundary line above the cell, one of the
`:attached' of the block.  A width change redraws only the bar: its
label depends on the width, the rendering does not."
  (when (overlay-buffer hov)
    ;; The overlay does not grow at its end, so a title typed at the end
    ;; of the line would be outside it.
    (save-excursion
      (goto-char (overlay-start hov))
      (move-overlay hov (pos-bol) (pos-eol)))
    (overblock-bar-draw hov 'markdown
                        (overblock-glyph "" "◇" "md")
                        (or (overblock-pycell--cell-title (overlay-start hov)
                                                          (overlay-end hov))
                            "markdown")
                        (overblock-buttons overblock-pycell-md-buttons))))

(defun overblock-pycell--md-block (beg end rendered)
  "Show RENDERED over the markdown cell BEG..END, with a bar above it.
See `overblock-pycell--md-show', which renders and calls this."
  (let* ((start (1- beg))
         (help "RET/mouse-2: edit this markdown cell, mouse-1: show source")
         (text (overblock-fill-props
                (overblock-faced rendered 'default)
                'keymap overblock-pycell-md-map 'help-echo help))
         ;; The bar covers the boundary line up to its newline. Any
         ;; other bar of the line goes first, such as a source bar.
         (hov (let ((from (overblock-pycell--md-cell-start beg)))
                (overblock-pycell--sole-bar from start nil)
                (overblock-bar-over from start)))
         ;; The block covers the source of the cell, not the bar.
         (block (overblock-show beg end
                                :kind 'markdown
                                ;; The source for the editor, as markers
                                ;; that follow edits above the cell.
                                ;; They outlive the block: the click
                                ;; that opens the editor removes the
                                ;; rendering first.
                                :data (cons (copy-marker beg)
                                            (copy-marker end t))
                                :over text
                                :keymap overblock-pycell-md-map
                                :help-echo help
                                :attached (list hov))))
    (overlay-put hov 'keymap overblock-pycell-md-map)
    ;; A click on the bar lands on this overlay.
    (overlay-put hov 'overblock-pycell-main block)
    (overblock-pycell--md-bar hov)
    ;; An edit of the source removes the rendering and the bar, which no
    ;; edit of the cell reaches.
    (overblock-pycell--stale-when-edited block)
    block))

(defun overblock-pycell--md-cells ()
  "Return the body of every markdown cell of the buffer, in order.
Each is a cons of the start and the end of the body, which is the next
boundary line or the end of the buffer.  An empty cell is left out."
  (save-excursion
    (goto-char (point-min))
    (let (cells)
      (while (re-search-forward (concat "^" overblock-pycell--md-boundary) nil t)
        (forward-line 1)
        (let ((from (point))
              (to (if (re-search-forward code-cells-boundary-regexp nil t)
                      (pos-bol)
                    (point-max))))
          (when (< from to) (push (cons from to) cells))
          (goto-char to)))
      (nreverse cells))))

;;;###autoload
(defun overblock-pycell-render-buffer ()
  "Render the markdown cells of the buffer that want it.
A markdown cell is one whose boundary line reads \"# %% [markdown]\",
and `overblock-live-wanted-p' says which want rendering: not those
rendered already, and not the one at point.  The live cycle of the
mode calls this whenever the reader stops.

One asynchronous converter process does all of them, so the reader
does not wait.  Nothing happens without a converter;
`overblock-pycell-mode' says so once when it goes on."
  (interactive)
  (overblock-md-render-regions
   (overblock-pycell--md-cells)
   'markdown
   (lambda (from to)
     (overblock-pycell--md-uncomment (buffer-substring-no-properties from to)))
   #'overblock-pycell--md-show))

(defun overblock-pycell--md-at (event)
  "Return the markdown block at point, or at the click in EVENT.
A click on the bar lands on the overlay of the bar, which points back
at the block.  Signal a `user-error' where there is no rendered cell."
  (overblock-goto-event event)
  (or (overblock-at 'markdown)
      (seq-some (lambda (ov) (overlay-get ov 'overblock-pycell-main))
                (overlays-in (max (1- (point)) (point-min))
                             (min (1+ (point)) (point-max))))
      (user-error "No rendered markdown cell here")))

;;;###autoload
(defun overblock-pycell-md-render-cell (&optional event)
  "Render the markdown cell at point, or the one whose button EVENT clicked.
`overblock-pycell-render-buffer' does the whole buffer.  This is the
button on the bar of a cell that shows its source."
  (interactive (list last-input-event))
  (overblock-goto-event event)
  (pcase-let ((`(,beg ,end) (code-cells--bounds nil nil t)))
    (unless (overblock-pycell--md-cell-start beg)
      (user-error "This is not a markdown cell"))
    (overblock-pycell--md-show beg end)))

;;;###autoload
(defun overblock-pycell-md-raw (&optional event)
  "Show the markdown cell at point, or the one in EVENT, as plain source.
The cell is then editable in place, and renders again when point
leaves it.  The button on its bar renders it at once."
  (interactive (list last-input-event))
  (overblock-take-down (overblock-pycell--md-at event)))

(defun overblock-pycell--md-put (beg end md)
  "Write the edited MD back into the markdown cell BEG..END and render it.
The cell reaches to the next boundary line, so it holds the blank line
jupytext writes between cells.  The whitespace after the body goes back
as it was, so an unchanged commit does not change the file.

Empty MD stays empty: `overblock-pycell--md-comment' would write a bare
#."
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
     (list :name (format "*overblock-pycell md: %s:%d*" (buffer-name)
                         (line-number-at-pos beg))
           :label "markdown cell"
           :mode (if (fboundp 'markdown-mode) #'markdown-mode #'text-mode)
           ;; Trimmed on the right: the blank line between cells stays
           ;; out of the edit buffer, and `--md-put' restores it.
           :text (lambda (from to)
                   (string-trim-right
                    (overblock-pycell--md-uncomment
                     (buffer-substring-no-properties from to))))
           :put #'overblock-pycell--md-put))))

;;;; The bar over a boundary line

(defun overblock-pycell--cell-title (bol eol)
  "Return the title written on the boundary line BOL..EOL, or nil.
What follows the =%%= marker is the title, as jupytext writes it,
without the tag list of a =# %% [markdown]= line."
  (save-excursion
    (goto-char bol)
    (when (looking-at code-cells-boundary-regexp)
      ;; Trimmed before the anchored tag search too: a space follows the
      ;; marker.
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
GLYPH comes before the label, PLAIN is the label of a cell without a
title, and BUTTONS are the buttons of the bar.  `overblock-bar-line'
draws it."
  (overblock-bar-line bol eol kind glyph
                      (or (overblock-pycell--cell-title bol eol) plain)
                      (overblock-buttons buttons)))

(defun overblock-pycell--source-bar (bol eol)
  "Draw the bar of the markdown cell BOL..EOL that is showing its source.
A rendered markdown cell has the bar of its rendering.  This bar is for
a cell without one: a new one, or one taken back to its source."
  ;; The label is "source": the glyph says markdown already, and the
  ;; label tells it from a rendered cell.
  (overblock-pycell--bar-line bol eol 'source
                    (overblock-glyph "" "◇" "md") "source"
                    overblock-pycell-source-buttons))

(defun overblock-pycell--code-bar (bol eol)
  "Draw the bar of the code cell whose boundary line is BOL..EOL."
  (overblock-pycell--bar-line bol eol 'code
                    (overblock-glyph "" "◆" "py") "python"
                    overblock-pycell-cell-buttons))

(defun overblock-pycell--drop-bar (bar)
  "Take BAR down, and the rendering it belongs to where it has one.
A markdown bar is an overlay of a block: the block goes with it, and
the source of the cell comes back."
  (if-let* ((block (overlay-get bar 'overblock-pycell-main)))
      (overblock-delete block)
    (delete-overlay bar)))

(defun overblock-pycell--cell-bars (start end)
  "Draw the bar of every code cell whose boundary line START..END touches.
Whole lines, whatever START and END are: this is called with the
bounds of a change.  `overblock-pycell--bar-this-line' decides the bar
of each line."
  (save-excursion
    (let ((from (progn (goto-char (min start end)) (pos-bol)))
          (to (progn (goto-char (max start end)) (pos-eol)))
          done)
      ;; The lines with a bar: one can have stopped being a boundary.
      (dolist (bar (seq-filter #'overblock-bar-kind
                               (overlays-in from (min (point-max) (1+ to)))))
        (when-let* ((pos (overlay-start bar)))
          (goto-char pos)
          (forward-line 0)
          (push (point) done)
          (overblock-pycell--bar-this-line)))
      ;; The boundary lines, searched for: a `revert-buffer' reports one
      ;; change over the whole buffer.
      (goto-char from)
      ;; Point first: `forward-line' can carry point past TO, and a bound
      ;; behind point is an error.
      (while (and (< (point) to)
                  (re-search-forward code-cells-boundary-regexp to t))
        (forward-line 0)
        ;; Not a second time.
        (unless (memq (point) done)
          (overblock-pycell--bar-this-line))
        (forward-line 1)))))

(defun overblock-pycell--sole-bar (bol eol kinds)
  "Return the one bar to keep on the line BOL..EOL, and drop the others.
KINDS names the kinds to keep, best first; every bar of another kind
goes, and so does a second bar of the same kind.  Nil keeps none.

A line has one bar, and both the pass over boundary lines and the
rendering of a markdown cell put one there."
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
     ;; No boundary line now: its buttons would act on the wrong cell.
     ((not (looking-at-p code-cells-boundary-regexp))
      (overblock-pycell--sole-bar bol eol nil))
     ;; A rendered markdown cell has the bar of its rendering; one that
     ;; shows its source gets a source bar.
     ((looking-at-p overblock-pycell--md-boundary)
      (let ((bar (overblock-pycell--sole-bar bol eol '(markdown source))))
        ;; Drawn again, for a title edited on the line.
        (if (eq (overblock-bar-kind bar) 'markdown)
            (overblock-pycell--md-bar bar)
          (overblock-pycell--source-bar bol eol))))
     (t
      ;; A markdown or source bar on a code boundary goes.
      (overblock-pycell--sole-bar bol eol '(code))
      (overblock-pycell--code-bar bol eol)))))

(defun overblock-pycell--bars-after-change (beg end _length)
  "Draw the bars of the lines the change BEG..END touched.
On `after-change-functions', not `jit-lock-register': an error in
another jit-lock function, such as a `python-ts-mode' grammar that does
not match, skips the rest.

The match data belongs to the caller: a change hook can run between a
search and its `replace-match'."
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

The colon is required, so output that ends with the name of an
exception, such as `print(type(err).__name__)', is no failure.  The
two names IPython prints alone are `overblock-pycell--error-alone'.")

(defconst overblock-pycell--error-alone
  "\\`\\(?:KeyboardInterrupt\\|SystemExit\\)\\'"
  "The exceptions IPython reports with nothing after the name.
An interrupted cell ends with a bare `KeyboardInterrupt', and
`sys.exit()' with a bare `SystemExit'; every other report carries a
colon and a message.")

(defun overblock-pycell--error-p (text)
  "Return non-nil when TEXT is the output of a cell that failed.
A traceback says so in its first line, but `SyntaxError' and
`SystemExit' print only the name of the exception.  So the last line,
where the name of the exception is, counts too."
  (or (string-match-p "Traceback (most recent call last)" text)
      (when-let* ((lines (split-string (string-trim-right text) "\n" t "[ \t\r]+"))
                  (last (car (last lines))))
        (or (string-match-p overblock-pycell--error-tail last)
            (string-match-p overblock-pycell--error-alone last)))))



(defun overblock-pycell--ipython-syntax-p (beg end)
  "Return non-nil when BEG..END holds syntax that only IPython reads.
A magic, a shell escape or a help request: a line that begins with %
or !, or one that ends in ?.

Only where the character has that meaning, so this reads the syntax of
the buffer.  A continuation line in brackets can start with a modulo,
and a comment or a docstring can hold either character.  A plain cell
must not go the IPython way, because a shell without IPython answers
with a NameError."
  (save-excursion
    (goto-char beg)
    (catch 'found
      (while (< (point) end)
        (let ((state (syntax-ppss (point)))
              (eol (min end (pos-eol))))
          ;; The line starts as code, not in a string, a comment or an
          ;; open bracket.
          (when (and (not (python-syntax-comment-or-string-p state))
                     (zerop (nth 0 state)))
            (when (looking-at-p "[ \t]*[%!]")
              (throw 'found t))
            (let ((last (save-excursion
                          (goto-char eol)
                          ;; Never before the line or the region.
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
reader of IPython, which turns %, ! and ? into calls, never sees it.
`run_cell' gets the source instead, base64 encoded so its quotes and
newlines are safe.  The trailing None hides the result object.

Tracebacks then count lines from the top of the cell, not the file."
  (python-shell-send-string
   (format "get_ipython().run_cell(__import__(\"base64\")\
.b64decode(\"%s\").decode(\"utf-8\"))\nNone\n"
           (base64-encode-string (encode-coding-string code 'utf-8) t))
   proc))

(defun overblock-pycell--send-region (proc beg end)
  "Send BEG..END to PROC, as the `:send' of the backend.
A cell of IPython syntax goes to the reader of IPython; every other
one goes through `python-shell-send-region', which pads it so the line
numbers of a traceback match the buffer."
  (if (overblock-pycell--ipython-syntax-p beg end)
      (overblock-pycell--send-to-ipython
       proc (buffer-substring-no-properties beg end))
    (python-shell-send-region beg end)))

(defun overblock-pycell--start ()
  "Start an inferior Python, and return nil: it prompts later.
This is the `:start' of the backend.  Nil tells the runner to arm the
work on the first prompt."
  (run-python nil (overblock-pycell--dedicated))
  nil)

(defun overblock-pycell--arm (thunk)
  "Call THUNK on the first prompt of the Python shell of this notebook.
This is the `:arm' of the backend.  THUNK runs after the setup of
comint-mime on the same hook, hence the depth.  A shell that signals
here leaves nothing armed.

The hook is local and the function removes itself, so no stale thunk
stays."
  (with-current-buffer (process-buffer (python-shell-get-process-or-error))
    (letrec ((once (lambda ()
                     (remove-hook 'python-shell-first-prompt-hook once t)
                     (funcall thunk))))
      (add-hook 'python-shell-first-prompt-hook once 90 t))))

(defun overblock-pycell--step ()
  "Run the cell at point, and say whether to wait for a prompt.
This is the `:step' of the backend, with which `overblock-run-next'
walks a pass down the notebook.  A markdown cell renders here, with no
prompt to wait for.  One that is rendered already is left alone, which
saves a converter process per cell."
  (pcase-let ((`(,beg ,end) (code-cells--bounds nil nil t)))
    (unless (and (overblock-pycell--md-cell-start beg)
                 (overblock-in beg end 'markdown))
      (overblock-pycell-eval-region beg end))
    (not (overblock-pycell--md-cell-start beg))))

(defun overblock-pycell--cell-at ()
  "Return the cell point is in as (BEG . END), boundary line included."
  (pcase-let ((`(,beg ,end) (code-cells--bounds)))
    (cons beg end)))

(defun overblock-pycell--backend ()
  "Return what `overblock-run' needs to drive an inferior Python.
The commentary of `overblock-run' lists the slots."
  (list :name "overblock-pycell"
        :unit "cell"
        :process #'python-shell-get-process
        :start #'overblock-pycell--start
        :arm #'overblock-pycell--arm
        :send #'overblock-pycell--send-region
        :prompt-p #'python-shell-comint-end-of-output-p
        :clean #'overblock-pycell--clean
        :error-p #'overblock-pycell--error-p
        :step #'overblock-pycell--step
        :region-at #'overblock-pycell--cell-at
        :starts #'overblock-pycell--cell-starts
        :redraw (lambda () (mapc #'overblock-pycell--bar-redraw (overblock-bars)))
        :buttons 'overblock-pycell-result-buttons
        :stale #'overblock-pycell--stale-when-edited))

(defun overblock-pycell--dedicated ()
  "Return what a new shell is dedicated to, as the reader asked.
`python-shell-dedicated' says it.  Its `project' value makes
`run-python' ask which project when the file belongs to none.
`python-shell-get-process-name' names such a shell the shared one, so
this returns nil then."
  (unless (and (eq python-shell-dedicated 'project)
               (not (project-current)))
    python-shell-dedicated))

(defun overblock-pycell-eval-region (start end)
  "Evaluate START..END as a cell and mirror the output below it.
This matches the calling convention of
`code-cells-eval-region-commands'.  A markdown cell renders instead.
Without an interpreter, one starts and the cell follows on its first
prompt.  A cell sent while another one runs is queued behind it."
  (if (overblock-pycell--md-cell-start start)
      (progn
        (overblock-pycell--md-show start end)
        ;; Redisplay pushes point out of the hidden text upwards; put it
        ;; below instead.
        (when (<= (1- start) (point) end)
          (goto-char end)))
    (overblock-run-region start end)))

;;;###autoload
(defun overblock-pycell-restart ()
  "Restart the Python interpreter and remove every result.
The rendered markdown cells stay: a pass that stops early would leave
the cells after it plain."
  (interactive)
  (overblock-run-restart
   "The interpreter was restarted"
   (lambda (proc)
     ;; `python-shell-restart' needs a shell.
     (if proc (python-shell-restart) (overblock-pycell--start)))))

(defun overblock-pycell--cell-starts ()
  "Return a marker on the first line of every cell of the buffer, in order.
The text above the first boundary line is a cell too, where there is
any."
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
The same as `code-cells-eval' (\\[code-cells-eval]) on that cell."
  (interactive (list last-input-event))
  (overblock-goto-event event)
  (apply #'code-cells-eval (code-cells--bounds nil nil t)))

;;;###autoload
(defun overblock-pycell-restart-and-run-all ()
  "Restart the Python interpreter, then evaluate every cell in order.
The pass stops at the first error, or on `overblock-run-stop'."
  (interactive)
  (overblock-pycell-restart)
  ;; A restarted shell has a live process that has not prompted yet.
  (overblock-run-on-prompt (overblock-pycell--cell-starts)
                         "overblock-pycell: evaluating all cells"))

(defvar-keymap overblock-pycell-mode-map
  :doc "Keymap of `overblock-pycell-mode', empty on purpose.
overblock-pycell binds no keys; put your own here, for example:

  (keymap-set overblock-pycell-mode-map \"C-c C-k\" #\\='overblock-run-interrupt)")

;;;###autoload
(define-minor-mode overblock-pycell-mode
  "Show Python cell results, and markdown cells, inline.
While the mode is on, cell evaluation goes through
`overblock-pycell-eval-region'.  Turn it off to remove all blocks and
to get plain `python-shell-send-region' back.  The mode binds no keys:
`overblock-pycell-mode-map' is empty.

`overblock-md-command' renders the markdown cells.  When none of its
candidates is installed, they stay plain and the code cells still
run."
  ;; The :lighter also keeps the body out of the deprecated
  ;; positional INIT-VALUE argument.
  :lighter " PyCell"
  (when overblock-pycell-mode
    (overblock-only-in 'overblock-pycell-mode 'python-base-mode))
  (if overblock-pycell-mode
      (progn
        (overblock-run-attach (overblock-pycell--backend))
        ;; Point moving into a rendered cell changes nothing; a click
        ;; shows its source.
        (setq-local overblock-live-source-at-point nil)
        ;; One advice for the session, added by the first notebook and
        ;; removed by the last, not at load time.
        (advice-add 'outline-flag-region :after
                    #'overblock-pycell--outline-flag-blocks)
        (add-hook 'after-change-functions #'overblock-pycell--bars-after-change nil t)
        ;; The whole buffer, also when narrowed.
        (without-restriction
          (overblock-pycell--cell-bars (point-min) (point-max)))
        ;; Said once, and only when there is a markdown cell.
        (when (and (not (overblock-md-program))
                   (overblock-pycell--md-cells))
          (message "overblock-pycell: %s, cells stay plain"
                   (if (fboundp 'libxml-parse-html-region)
                       (format "no markdown converter found (%s)"
                               (string-join (ensure-list overblock-md-command)
                                            ", "))
                     "this Emacs was built without libxml, which shr reads \
the converter's HTML with")))
        (overblock-live-start 'markdown #'overblock-pycell-render-buffer))
    (overblock-live-stop 'markdown)
    (overblock-run-detach)
    (kill-local-variable 'overblock-live-source-at-point)
    (remove-hook 'after-change-functions #'overblock-pycell--bars-after-change t)
    ;; The last notebook removes the advice. The mode variable of this
    ;; buffer is already nil.
    (unless (seq-some (lambda (buffer)
                        (buffer-local-value 'overblock-pycell-mode buffer))
                      (buffer-list))
      (advice-remove 'outline-flag-region #'overblock-pycell--outline-flag-blocks))))

;;;###autoload
(defun overblock-pycell-mode-maybe ()
  "Enable `overblock-pycell-mode' in Python cell buffers.
Add it to `code-cells-mode-hook':

  (add-hook \\='code-cells-mode-hook #\\='overblock-pycell-mode-maybe)

The package installs no hook itself."
  (when (derived-mode-p 'python-base-mode)
    (overblock-pycell-mode)))

;; Keyed on the minor mode: with it off, code-cells falls through to its
;; stock python entry, `python-shell-send-region'.
(setf (alist-get 'overblock-pycell-mode code-cells-eval-region-commands)
      #'overblock-pycell-eval-region)

(provide 'overblock-pycell)
;;; overblock-pycell.el ends here
