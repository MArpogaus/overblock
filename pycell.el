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

(require 'pycell-block)
(require 'code-cells)
(require 'comint-mime)
;; comint-mime renders a table with it, and a block lays that table
;; out again.  Optional, as it is in comint-mime: an Emacs without
;; vtable shows the text of the table as it came.
(require 'vtable nil t)
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

(defconst pycell--button-type
  '(repeat
    (list (symbol :tag "Key")
          (repeat :tag "Glyph candidates" string)
          (string :tag "Tooltip")
          (function :tag "Command")
          (choice :tag "Shows"
                  (const :tag "Always" t)
                  (const :tag "With an image" image)
                  (const :tag "With output" lines))))
  "The customize type of a list of header buttons.")

(defcustom pycell-result-buttons
  '((move-up ("󰅃" "⌃" "u") "Move this cell up" pycell-move-cell-up t)
    (move-down ("󰅀" "⌄" "d") "Move this cell down" pycell-move-cell-down t)
    (save-image ("󰇚" "↧" "↓") "Save the result's image to a file"
                pycell-save-image image)
    (copy ("󰆏" "❐" "≡") "Copy this result" pycell-copy-output lines)
    (pop ("󰏋" "↗" "^") "Show this result in its own buffer"
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
  :type pycell--button-type)

(defcustom pycell-markdown-buttons
  '((move-up ("󰅃" "⌃" "u") "Move this cell up" pycell-move-cell-up t)
    (move-down ("󰅀" "⌄" "d") "Move this cell down" pycell-move-cell-down t)
    (edit ("󰏋" "↗" "^") "Edit this markdown cell in its own buffer"
          pycell-md-edit t)
    (source ("󰅖" "✕" "x") "Show the plain source" pycell-md-raw t))
  "The buttons on the header of a rendered markdown cell.
The entries read as in `pycell-result-buttons'.  A markdown cell has
no output, so `lines' and `image' say nothing here."
  :type pycell--button-type)

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

;;;; What a block shows: text, buttons and bars

(defun pycell-remove-overlays (&optional beg end kind)
  "Remove the blocks between BEG and END, of KIND when it is given.
This is the command a reader binds; `pycell-block-clear\=' is the same
thing inside the block layer, which keeps its own so that it travels
whole.
BEG and END default to the whole buffer.  Results and rendered
markdown cells go; the text of the buffer is not touched."
  (interactive)
  (pycell-block-clear beg end kind))


(defun pycell--faced (string face)
  "Add FACE below the faces STRING already carries.  Return STRING.
STRING is modified in place.
An overlay string without a face inherits one from the buffer text
next to it, so every block needs at least a base face."
  (add-face-text-property 0 (length string) face t string)
  string)

(defun pycell--fill-prop (string prop value)
  "Set PROP to VALUE where STRING does not carry PROP yet.
Return STRING, which is modified in place.  shr gives a link its own
keymap and help echo; a plain `propertize' would clobber both, and the
link would then run this block's commands instead of following the URL."
  (let ((pos 0) (len (length string)))
    (while (< pos len)
      (let ((next (or (next-single-property-change pos prop string) len)))
        (unless (get-text-property pos prop string)
          (put-text-property pos next prop value string))
        (setq pos next))))
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

(defun pycell--buttons (descriptors &optional imagep lines)
  "Return the icon group that DESCRIPTORS ask for.
Each descriptor is (KEY GLYPHS HELP COMMAND WHEN), as in
`pycell-result-buttons'.  IMAGEP says the result holds an image and
LINES how many lines it has; a descriptor whose WHEN is `image' or
`lines' waits for those."
  (concat
   (string-join
    (delq nil
          (mapcar
           (lambda (descriptor)
             (pcase-let ((`(,_key ,glyphs ,help ,command ,when) descriptor))
               (when (pcase when
                       ('image imagep)
                       ('lines (> (or lines 0) 0))
                       (_ t))
                 (pycell--button (apply #'pycell--glyph glyphs) help command))))
           descriptors))
    "  ")
   " "))

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

;;;; Result blocks

(defun pycell--table-at (text)
  "Return the vtable that part of TEXT was rendered from, and where.
comint-mime renders an HTML table with vtable, a DataFrame among them,
and the copy carries the table object in a text property.  The value is
\(TABLE BEG END), or nil."
  (when (fboundp 'vtable-p)
    (let ((len (length text))
          (pos 0)
          table beg end)
      ;; The newline that ends a row carries no property of the table, so
      ;; the run is not one stretch: take the first and the last place
      ;; that names a table, and everything between them belongs to it.
      ;; Run to run, not character to character: measured, a step of one
      ;; cost 30 milliseconds over a hundred thousand characters and 223
      ;; over eight hundred thousand, where a jump costs nothing.
      (while (and pos
                  (setq pos (text-property-not-all pos len 'vtable nil text)))
        (let ((here (get-text-property pos 'vtable text))
              (next (or (next-single-property-change pos 'vtable text) len)))
          (when (vtable-p here)
            (unless table (setq table here beg pos))
            (setq end next))
          (setq pos (and (< next len) next))))
      (when table (list table beg end)))))

(defun pycell--table-copy (table)
  "Return a table of the rows and columns of TABLE, for another buffer.
The table of a result belongs to the shell that drew it.  Emacs 31
refuses to insert one vtable into a second buffer — \"A vtable cannot be
inserted into more than one buffer\" — and even where it is allowed, two
buffers holding one object is not a state worth having."
  (make-vtable :columns (mapcar (lambda (column)
                                  (list :name (vtable-column-name column)
                                        :width (vtable-column-width column)
                                        :align (vtable-column-align column)
                                        :primary (vtable-column-primary column)))
                                (vtable-columns table))
               :objects (vtable-objects table)
               :getter (vtable-getter table)
               :formatter (vtable-formatter table)
               :separator-width (vtable-separator-width table)
               ;; The rows show in the order the first table showed them,
               ;; which is the order of its objects put through its sort.
               :sort-by (vtable-sort-by table)
               ;; comint-mime draws the names of the columns into the
               ;; buffer, where `make-vtable' would put them on the
               ;; window's header line and shift every row up by one.
               :use-header-line (vtable-use-header-line table)
               :insert nil))

(defun pycell--table-text (table)
  "Return TABLE as text whose columns line up in characters.
A vtable aligns with stretches of pixels measured in the window that
drew it, and it measures a header cell in the face of a header.  A copy
is shown elsewhere, in a face of its own, so the columns are laid out
again here: one space of padding to the widest cell of each column, and
nothing that a face can move."
  (let* ((columns (vtable-columns table))
         (rows (cons (mapcar #'vtable-column-name columns)
                     (mapcar
                      (lambda (object)
                        (let ((index -1))
                          (mapcar
                           (lambda (_column)
                             (setq index (1+ index))
                             (format "%s"
                                     (if-let* ((getter (vtable-getter table)))
                                         (funcall getter object index table)
                                       (elt object index))))
                           columns)))
                      (vtable-objects table))))
         ;; Every row has a cell for every column, the header row
         ;; included, so a column is as wide as its widest cell.
         (widths (seq-map-indexed
                  (lambda (_column index)
                    (apply #'max (mapcar (lambda (row)
                                           (string-width (nth index row)))
                                         rows)))
                  columns))
         (lines (mapcar
                 (lambda (row)
                   (string-trim-right
                    (string-join
                     (seq-mapn (lambda (cell width)
                                 (concat cell
                                         (make-string
                                          (max 0 (- width (string-width cell)))
                                          ?\s)))
                               row widths)
                     "  ")))
                 rows)))
    ;; the names of the columns, in bold as a markdown table has them
    (setcar lines (propertize (car lines) 'face 'bold))
    (string-join lines "\n")))

(defun pycell--strip-prompts (text)
  "Return TEXT without the shell\'s prompts and its Out[N] labels.
The prompt before the output goes, the prompt after it goes, and so does
the one that ends up on the same line as output which stopped without a
newline — `comint-prompt-regexp\=' anchors to a line start and cannot see
that one.  An `Out[N]:\=' label goes wherever it stands, since IPython
writes one in front of the value of every cell.  Call this in the shell
buffer, where that variable has its value."
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
  (if (string-search "Out[" text)
      (replace-regexp-in-string "^Out\\[[0-9]+\\]: " "" text)
    text))

(defun pycell--detach (text)
  "Return the part of TEXT a block shows, cut loose from the shell.
The outer whitespace goes, except whitespace that carries a display
property: comint-mime renders an image as one space with such a
property, and `string-trim\=' would delete it.

What the shell buffer shows is not what a copy of it shows.  comint-mime
renders a DataFrame as a vtable, which aligns its columns with pixel
targets measured in that window and carries the keymap of a live table.
In a result block the targets land elsewhere, and no binding of that
keymap can find a table.  So the columns become literal spaces, the
keymap, the mouse face and the help echo go, and a table keeps its
object under `pycell-table\=', which `pycell-pop-output\=' shows live."
  (let* ((beg 0)
         (end (length text))
         (blank (lambda (i) (and (memq (aref text i) '(?\s ?\t ?\n ?\r))
                                 (not (get-text-property i 'display text))))))
    (while (and (< beg end) (funcall blank beg)) (setq beg (1+ beg)))
    (while (and (< beg end) (funcall blank (1- end))) (setq end (1- end)))
    (let ((copy (let ((cut (substring text beg end)))
                  ;; Only a rendering leaves alignment stretches behind,
                  ;; and a stretch is a display property: plain output
                  ;; skips the copy through a buffer.  Measured, that
                  ;; round trip costs 23 milliseconds over eight hundred
                  ;; thousand characters of propertized text.
                  (if (text-property-not-all 0 (length cut) 'display nil cut)
                      (pycell--flattened cut)
                    cut))))
      (remove-list-of-text-properties
       0 (length copy) '(keymap local-map mouse-face help-echo) copy)
      (if-let* ((found (pycell--table-at copy)))
          (pcase-let* ((`(,table ,tbeg ,tend) found)
                       (laid-out (propertize
                                  (pycell--table-text table)
                                  'pycell-table table)))
            (concat (substring copy 0 tbeg) laid-out (substring copy tend)))
        copy))))

(defun pycell--clean (text)
  "Return TEXT as a result block can show it.
The prompts and the Out[N] labels go, and the copy is cut loose from
the shell; see `pycell--strip-prompts\=' and `pycell--detach\=' for what
each of those means.  Call this in the shell buffer, where
`comint-prompt-regexp\=' has its value."
  (pycell--detach (pycell--strip-prompts text)))

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

(defun pycell--image-limit ()
  "Return how many pixels tall an image may be drawn, or nil for no cap.
The share is `pycell-max-image-height\=' of the window that shows the
notebook.  A cell can finish while its notebook is elsewhere — sent and
switched away from, or one of a whole run — and no window at all would
mean no cap and a block the wheel cannot get past.  The selected window
is a guess at the size the notebook will have, and a guess that comes
out small only draws a smaller figure."
  (when-let* (((numberp pycell-max-image-height))
              ((> pycell-max-image-height 0))
              (window (or (get-buffer-window nil t) (selected-window)))
              (limit (round (* pycell-max-image-height
                               (window-body-height window t))))
              ((> limit 0)))
    limit))

(defun pycell--fit (line)
  "Return LINE with its images capped to `pycell-max-image-height'.
The line kept for `pycell-pop-output' is not touched: this copies
before it caps."
  (if-let* ((limit (pycell--image-limit))
            ((pycell-block-image-in line)))
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

(defun pycell--lines-up-to (text limit)
  "Return the first LIMIT lines of TEXT and how many lines TEXT has.
The value is (LINES . TOTAL).  Only the part that shows is copied, and
only where there is more than that: measured, splitting ten thousand
propertized lines to keep twelve of them cost 15 milliseconds, and a
text of fewer lines than the limit is not copied at all."
  (let ((pos 0) (count 0) (cut nil))
    (while (setq pos (string-search "\n" text pos))
      (setq count (1+ count)
            pos (1+ pos))
      (when (and (null cut) (= count limit)) (setq cut (1- pos))))
    (cons (split-string (if cut (substring text 0 cut) text) "\n")
          (1+ count))))

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
             (image (and (display-images-p) (pycell-block-image-in l))))
        (push (if image (pycell--fit l) (pycell--shorten l)) shown)
        (when image (setq stop t))))
    (nreverse shown)))

(defun pycell--header (folded total shown runtime state imagep)
  "Return the header bar of a result.
FOLDED is non-nil when only the header shows.
TOTAL and SHOWN count the lines and the inline subset.  RUNTIME is the
time in seconds since the cell started.  STATE is `running\=' while the
cell runs, `died\=' where the interpreter went away before the cell
ended, and nil where the cell finished.  IMAGEP marks a result with an image."
  (let* ((icons (pycell--buttons pycell-result-buttons imagep total))
         ;; The stopwatch drives the spinner: one frame for each tick.
         (mark (cond ((eq state 'running)
                       (let ((frames (pycell--glyph "⠋⠙⠹⠸⠼⠴⠦⠧⠇⠏" "|/-\\")))
                         (string ?\s (aref frames (mod (truncate runtime 0.2)
                                                       (length frames))))))
                     ((eq state 'died) (pycell--glyph " 󰀪" " ⚠" " !"))
                      ;; A single line can still be tall: one image is
                      ;; one line, and that is the block worth folding.
                      ((> total 0)
                       (pycell--button (if folded
                                           (pycell--glyph " 󰍟" " ▸" " >")
                                         (pycell--glyph " 󰍝" " ▾" " v"))
                                          "Fold or unfold this result"
                                          #'pycell-toggle-output))
                      ((zerop total) (pycell--glyph " 󰄬" " ✓" " ."))
                      (t " ")))
         (label (cond ((> total 0)
                       (format "%d line%s%s" total (if (= total 1) "" "s")
                               (if (< shown total)
                                   (format ", showing %d" shown) "")))
                      ((not state) "no output")))
         (time (format "%.1fs" runtime)))
    (pycell--bar
     (concat mark " " (string-join (delq nil (list label time)) " · "))
     icons)))

(defun pycell--update (block)
  "Make the header and the body of the result BLOCK again, and show them.
The lines are counted once for both: the header says how many there
are and how many of them show, and the body is those that show."
  (pcase-let ((`(,folded ,text ,runtime ,state ,total)
               (pycell-block-get block :data)))
    (pcase-let* ((`(,lines . ,count)
                  (if (string-empty-p text)
                      '(nil . 0)
                    (pycell--lines-up-to text pycell-max-lines)))
                 (shown (pycell--body-lines lines)))
      (pycell-block-set block :header
                         (pycell--header folded (or total count)
                                         (length shown) runtime state
                                         (and lines (pycell-block-image-in text))))
      (pycell-block-set block :body
                         (when (and shown (not folded))
                           (pycell--faced (string-join shown "\n")
                                          'pycell-output)))
      (pycell-block-refresh block))))

(defun pycell--tab-filter (cmd)
  "Return CMD when point sits at the very end of a cell with a result."
  (and (eolp)
       (seq-some (lambda (o) (eq (point) (overlay-end o)))
                 (pycell-block-in (max (1- (point)) (point-min)) (point)
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
  (let* ((old (car (pycell-block-in beg end 'result)))
         (folded (and old (car (pycell-block-get old :data))))
         (data (list folded text runtime state total)))
    (if (and old (= (overlay-start old) beg))
        ;; The ticker of a running cell comes here five times a second
        ;; with nothing new but its data.  Keeping the block it has saves
        ;; two overlays and a scan of the region on every tick, and it
        ;; leaves redisplay alone.
        (progn (pycell-block-set old :data data)
               (pycell--update old)
               old)
      ;; The newline that ends the cell carries the result; give the
      ;; last cell of the buffer one.
      (when (and (= end (point-max)) (not (eq (char-before end) ?\n)))
        (save-excursion (goto-char end) (insert "\n")))
      (let ((block (pycell-block-show beg end
                                       :kind 'result
                                       :data data
                                       :keymap pycell-overlay-map)))
        ;; An edit of the cell makes the result stale; it goes.
        (overlay-put block 'modification-hooks
                     (list (lambda (o &rest _) (pycell-block-delete o))))
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
  (or (pycell-block-at 'result)
      (car (apply #'pycell-block-in (append (code-cells--bounds) '(result))))
      (user-error "No result here")))

(defun pycell-toggle-output (&optional event)
  "Fold or unfold the result at point, or the one clicked in EVENT."
  (interactive (list last-input-event))
  (let* ((block (pycell--result-at event))
         (data (pycell-block-get block :data)))
    (pycell-block-set block :data (cons (not (car data)) (cdr data)))
    (pycell--update block)))

(defun pycell-discard-output (&optional event)
  "Discard the result at point, or the one clicked in EVENT."
  (interactive (list last-input-event))
  (pycell-block-delete (pycell--result-at event)))

;;;; Moving a cell

(defun pycell--cell-state (beg end)
  "Return what the cell BEG..END shows, to put back after a move.
The car is the record of its result, or nil, and the cdr says whether
its markdown was rendered."
  (cons (when-let* ((block (car (pycell-block-in beg end 'result))))
          (copy-sequence (pycell-block-get block :data)))
        (and (pycell-block-in beg end 'markdown) t)))

(defun pycell--restore-cell (beg end state)
  "Show STATE on the cell BEG..END again.
STATE comes from `pycell--cell-state'.  A markdown cell is rendered by
the caller, which does the whole buffer at once."
  (when-let* ((record (car state)))
    (pcase-let* ((`(,folded ,text ,runtime ,state ,total) record)
                 (block (pycell--show beg end text runtime state total)))
      (when folded
        (pycell-block-set block :data record)
        (pycell--update block)))))

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
    (pycell-block-clear (min beg nbeg) (max end nend))
    (let ((mine-beg (if down (- nend mine-length) nbeg))
          (their-beg (if down beg (- end their-length))))
      (pycell--restore-cell mine-beg (+ mine-beg mine-length) mine)
      (pycell--restore-cell their-beg (+ their-beg their-length) theirs)
      (when (or (cdr mine) (cdr theirs))
        (pycell-md-render-all))
      (goto-char (+ mine-beg (min offset mine-length))))))

;;;###autoload
(defun pycell-move-cell-up (&optional arg)
  "Move the cell at point up ARG cells, with what it shows."
  (interactive "p")
  (pycell-move-cell-down (- (or arg 1))))

(defun pycell--text (block)
  "Return the text of the result BLOCK.
The one reader of that field: a record that each caller takes apart
by hand is a record that cannot change shape."
  (nth 1 (pycell-block-get block :data)))

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
         (img (or (pycell-block-image-in text)
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
        (if-let* ((table (get-text-property (or (text-property-not-all
                                                 0 (length text)
                                                 'pycell-table nil text)
                                                0)
                                           'pycell-table text)))
            (vtable-insert (pycell--table-copy table))
          (insert text)))
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
  (dolist (block (pycell-block-in from to 'result))
    (when-let* ((nl (pycell-block-get block :newline))
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
is what leaves it room."
  (when flag (pycell--keep-result-newline from to))
  (dolist (block (pycell-block-in from to 'markdown))
    (pycell-block-set block :hidden flag)
    (pycell-block-refresh block)))

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

(defun pycell--md-html-batch (texts)
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

(defun pycell--md-image-file (src)
  "Return the readable local image file that SRC names, or nil.
A markdown cell writes `![a figure](figure.png)\=', and a path like that
belongs to the directory of the notebook.  An absolute path and a
`file://\=' URL name the file directly; anything with another scheme is
not ours to open."
  (when-let* ((path (cond ((string-prefix-p "file://" src)
                           (url-unhex-string (substring src 7)))
                          ((not (string-match-p "\\`[a-zA-Z][a-zA-Z0-9+.-]*:"
                                                src))
                           src)))
              ((not (string-empty-p path)))
              (file (expand-file-name path))
              ((file-readable-p file))
              ((image-supported-file-p file)))
    file))

(defun pycell--md-tag-img (dom)
  "Draw the image DOM names when it is a file, and leave the rest to shr.
shr fetches an image with `url-queue-retrieve\=', which answers long
after the cell is rendered, so the rendering keeps the grey placeholder
that shr leaves in the meantime: measured with a relative path, an
absolute one and a `file://\=' URL alike, every local image stayed a
placeholder.  A file on disk needs no fetching.

The alt text carries the image, so a terminal that draws none still
says what is there, and the figure is capped like a result\='s."
  (if-let* ((file (pycell--md-image-file (or (dom-attr dom 'src) ""))))
      (let ((limit (pycell--image-limit)))
        (insert (propertize (or (dom-attr dom 'alt) " ")
                            'display (apply #'create-image file nil nil
                                            (and limit
                                                 (list :max-height limit))))))
    (shr-tag-img dom)))

(defconst pycell--md-rendering-functions
  (list (cons 'th #'pycell--md-tag-th)
        (cons 'code #'pycell--md-tag-code)
        (cons 'img #'pycell--md-tag-img)
        (cons 'table #'pycell--md-tag-table))
  "How this package renders the tags shr renders differently.
See `shr-external-rendering-functions'.")

(defun pycell--space-columns (spec column)
  "Return the columns that the space SPEC covers at COLUMN, or nil.
A `:align-to\=' spec names where the space ends and a `:width\=' spec how
wide it is.  Both count pixels in a list and characters in a bare
number; a terminal's pixel is a column, a graphic frame's is
`frame-char-width\='."
  (let* ((plist (cdr spec))
         (to (plist-get plist :align-to))
         (width (plist-get plist :width))
         (chars (lambda (n) (if (consp n)
                                (round (car n) (frame-char-width))
                              (and (numberp n) (round n))))))
    (cond ((and to (funcall chars to))
           (max 0 (- (funcall chars to) column)))
          ((and width (funcall chars width))
           (max 0 (funcall chars width))))))

(defun pycell--md-flatten-alignment ()
  "Turn the space stretches of this buffer into real spaces.
shr aligns table columns with `(space :align-to (N))\=' display specs,
and vtable, which is how comint-mime shows a DataFrame, with
`(space :width (N))\='.  Both count from the window they were measured
in.  A rendered cell and a result block are shown indented — line
numbers, margins — so the stretches land elsewhere there and the
columns of a row drift apart.  Literal padding aligns anywhere.  Left
to right, so `current-column\=' already sees the padding put in before
it."
  (goto-char (point-min))
  (let (match)
    (while (setq match (text-property-search-forward 'display))
      (let ((spec (prop-match-value match)))
        (when (eq (car-safe spec) 'space)
          (let* ((beg (prop-match-beginning match))
                 (end (prop-match-end match))
                 (pad (pycell--space-columns
                       spec (save-excursion (goto-char beg)
                                            (current-column)))))
            (when pad
              (goto-char beg)
              (delete-region beg end)
              ;; Zero is a zero-width stretch: the column is already
              ;; there, and a forced space would push this row one past
              ;; its sisters.
              (insert (make-string pad ?\s)))))))))

(defun pycell--flattened (text)
  "Return TEXT with its space stretches as real spaces.
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
         ;; The bar covers the word =markdown= of the boundary line, and
         ;; nothing else: =# %%= keeps the look of every other cell
         ;; boundary and `outline-minor-mode\=' still finds a heading
         ;; line where it expects one.  The overlay stops before the
         ;; newline, where the cell begins.
         (hov (make-overlay (save-excursion
                              (goto-char (pycell--md-cell-start beg))
                              (if (re-search-forward "\\[markdown\\]"
                                                     (pos-eol) t)
                                  (match-beginning 0)
                                (pos-eol)))
                            start))
         ;; The block covers the source of the cell.  The pieces hang
         ;; on those lines, and the bar above them is not part of it.
         (block (pycell-block-show beg end
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
                 (pycell--bar "markdown"
                              (pycell--buttons pycell-markdown-buttons)))
    ;; An edit of the source takes the rendering with it, the bar
    ;; included.  The block itself evaporates with the text it covers,
    ;; and the bar sits on the boundary line above, where no edit of the
    ;; cell reaches it: it would be left behind, and `pycell-md-commit'
    ;; would draw a second bar beside it.
    (overlay-put block 'modification-hooks
                 (list (lambda (o &rest _) (pycell-block-delete o))))
    block))

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
                      (pycell--md-html-batch
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
  (pycell-block-clear (point-min) (point-max) 'markdown))

(defun pycell--md-at (event)
  "Return the markdown block at point, or at the click in EVENT.
A click on the bar lands on the small overlay that draws it, which
points back at the block.  Signals a `user-error\=' where there is no
rendered cell, which is the answer the commands that call it give."
  (pycell--goto-event event)
  (or (pycell-block-at 'markdown)
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
  (pycell-block-delete (pycell--md-at event)))

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
               (`(,beg . ,end) (pycell-block-get block :data))
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
since the one before it.

The three fields a tick reads and writes have accessors below.  A record
that each caller takes apart by hand is a record that cannot change
shape.")

(defun pycell--run-tail ()
  "Return the recent output kept for the prompt detection."
  (nth 3 pycell--run))

(defun pycell--set-run-tail (value)
  "Keep VALUE as the recent output for the prompt detection."
  (setf (nth 3 pycell--run) value))

(defun pycell--run-head ()
  "Return the part of the output the block shows, or nil while it grows."
  (nth 6 pycell--run))

(defun pycell--set-run-head (value)
  "Keep VALUE as the part of the output the block shows."
  (setf (nth 6 pycell--run) value))

(defun pycell--run-count ()
  "Return the (POSITION . LINES) a tick counted, or nil for none yet."
  (nth 7 pycell--run))

(defun pycell--set-run-count (value)
  "Keep VALUE as the (POSITION . LINES) counted so far."
  (setf (nth 7 pycell--run) value))

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

A cell that prints much on few lines never reaches that line, so its
text is never kept and every tick reads it whole.  A bound in characters
would end that, and it may not be had: comint-mime sends an image as one
escape sequence of its own, a cut inside it leaves an unfinished
sequence, and this function drops what follows one.  Measured, that
turned a figure into a result of no characters at all.
Nothing is kept while the head is empty: an escape sequence that has
not arrived in full swallows everything after it until it does, and a
cell whose first lines are still on their way has more to come."
  (or (pycell--run-head)
      (let* ((budget (and (natnump pycell-max-line-length)
                          (> pycell-max-line-length 0)
                          (* (+ pycell-max-lines 4)
                             (1+ pycell-max-line-length))))
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
          (pycell--set-run-head text))
        text)))

(defun pycell--total (from)
  "Return the number of lines the running cell has printed after FROM.
Counted where they arrive: reading the whole output again is a pass
over everything printed so far, and a cell that prints a lot pays
that pass five times a second.  Leading blank lines go, as
`pycell--clean' drops them, so the count agrees with the one the
finished cell shows."
  (let* ((state (or (pycell--run-count)
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
      (pycell--set-run-count
       (cons (if-let* ((marker (car-safe state))
                       ((markerp marker)))
                 (set-marker marker (point))
               (point-marker))
             count)))
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
    (let ((tail (concat (pycell--run-tail)
                        (ansi-color-filter-apply output))))
      (pycell--set-run-tail
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
    (pycell--show beg fin "" 0.0 'running)
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
  "Restart the Python interpreter, and remove every result and rendering.
A rendered markdown cell is a block like a result, so it goes too and
shows its source again."
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
