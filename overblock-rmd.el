;;; overblock-rmd.el --- Inline results for the R chunks of an Rmd file  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Marcel Arpogaus

;; Author: Marcel Arpogaus <znepry.necbtnhf@tznvy.pbz>
;; Assisted-by: Claude:claude-opus-5
;; Version: 1.0
;; Package-Requires: ((emacs "29.1") (overblock "1.0") (overblock-md "1.0") (ess "24.1"))
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

;; Notebook style results for the R chunks of an Rmd file, built from
;; ESS alone -- no knitr run, no rendered document.
;;
;; Turn `overblock-rmd-mode' on in an Rmd buffer and every
;; ```{r} chunk gets a bar with a run button, the prose between the
;; chunks reads as it will look, and running a chunk grows its result
;; below the code: a header bar with a spinner, a stopwatch and
;; buttons, and the output of R underneath.
;;
;; An Rmd file is the inverse of a Python notebook.  A `.py' notebook
;; is code with `# %%' lines cutting it into cells; an Rmd file is
;; markdown prose with fenced chunks of code inside it.  So this
;; package composes rather than copies: the chunks come from the fence
;; walk of `overblock-md-preview', the prose between them is rendered by
;; the same live cycle that `overblock-md-preview-mode' uses, and the
;; running and the result blocks are `overblock-run''s -- the layer that
;; `overblock-pycell' runs its cells through as well.  What is here is
;; the part that knows about R and about Rmd: the chunks, the bars, the
;; commands, and eleven lines of ESS.
;;
;; A chunk reaches R as one statement, not as its own lines:
;;
;;     source(exprs = parse(text = "..."), print.eval = TRUE)
;;
;; That is what makes the result collectable.  Sent line by line, R
;; prompts after every statement of the chunk and the prompts land in
;; the middle of the output, where nothing can tell them from a line
;; that happens to look like one.  Wrapped like this the chunk is one
;; statement, one prompt comes back at the end, and `source' with
;; `print.eval' prints the value of every top level expression -- which
;; is what a notebook cell does and what a bare `eval' would not.
;;
;; The other road was ESS's own send-and-collect: `ess-command' is
;; synchronous and would freeze Emacs for the length of a chunk, and
;; `ess-async-command' says in its own docstring that it is for
;; background jobs and that long output escapes into the process
;; buffer.
;;
;; A figure comes back the way knitr brings one in: R draws to a
;; graphics device rather than to its terminal, so the wrapper opens a
;; PNG device before the chunk, closes it after, and names each file it
;; wrote on a line of its own.  The result reads those lines back as
;; images, and from there a figure is what a figure is in the Python
;; notebook: capped to the window, saved with its button, popped out,
;; and named in a terminal.  `overblock-rmd-figure-size' is what knitr
;; calls `fig.width' and `fig.height'.
;;
;; What draws a block on the screen is not here: `overblock' puts text
;; over a region of a buffer with a header above it, `overblock-md'
;; turns markdown into a string it can show, and `overblock-run' sends a
;; region to a shell and shows what comes back.

;;; Code:

(require 'overblock)
(require 'overblock-run)
(require 'overblock-repl)
(require 'overblock-md)
(require 'overblock-md-preview)
(require 'ess-inf)
(require 'ess-r-mode)
(require 'seq)
(require 'subr-x)

(defgroup overblock-rmd nil
  "Inline results for the R chunks of an Rmd file."
  :group 'ess
  :prefix "overblock-rmd-")

(defcustom overblock-rmd-result-buttons
  '((stop ("" "□" "stop") "Stop the run after this chunk"
          overblock-run-stop running)
    (save-image ("" "↧" "save") "Save the result's figure to a file"
                overblock-run-save-image image)
    (copy ("" "◫" "copy") "Copy this result" overblock-run-copy-output lines)
    (pop ("" "↗" "pop") "Show this result in its own buffer"
         overblock-run-pop-output lines)
    (discard ("" "✕" "drop") "Discard this result"
             overblock-run-discard-output t))
  "The buttons on the header of a result, left to right.
Each entry is (KEY GLYPHS HELP COMMAND WHEN):

- KEY names the button for you, and nothing else reads it.
- GLYPHS are the candidates for its label.  The first one the frame
  can draw wins, and the last one always answers, so keep something
  every display has at the end.  Three of them is the shape used
  across this repository: a nerd glyph, a character an ordinary
  monospace font has, and a short word for the display that has
  neither.

  Every nerd glyph here is a codicon, the set whose names begin
  nf-cod- and which VS Code draws its own buttons with, and each one
  means here what it means in `overblock-pycell-result-buttons': a
  reader who moves between a `.py' notebook and an Rmd file reads the
  same row.
- HELP is the tooltip.
- COMMAND runs on a click.
- WHEN says when the button shows: t always, `image' only with a
  figure in the result, `lines' only with output, `running' only while
  the chunk runs.

The same five buttons as `overblock-pycell-result-buttons' carries, less
the pair that moves a cell: a chunk sits inside prose that reads about
it.  The fold arrow and the spinner are not buttons of this list: they
say what the result is doing."
  :type overblock-button-type
  :set #'overblock-run-set-buttons)

(defcustom overblock-rmd-chunk-buttons
  '((run-above ("" "⇈" "above") "Run every chunk above this one"
               overblock-run-above t)
    (run ("" "▷" "run") "Run this chunk" overblock-rmd-run-chunk t))
  "The buttons on the bar of an R chunk, left to right.
The entries read as in `overblock-rmd-result-buttons'.  A chunk bar is
drawn before the chunk has run, so `lines' says nothing here.

Both glyphs are the ones `overblock-pycell-cell-buttons' uses for the
same two commands.  A chunk has no move buttons: the cells of a `.py'
notebook are its top level structure and moving one is an ordinary
edit, while a chunk sits inside prose that reads about it, and moving
the code away from its paragraph is not what the reader meant."
  :type overblock-button-type
  :set #'overblock-run-set-buttons)

(defcustom overblock-rmd-max-lines 12
  "Number of result lines that show inline.
Zero shows all of them.
A result block is one buffer line however tall it is, so a long result
makes one long step for `next-line' and for the wheel.  The header says
how many lines there are in all where it shows fewer.

See `overblock-rmd-max-line-length' for the width."
  :type 'natnum)

(defcustom overblock-rmd-figure-size '(7 . 5)
  "Width and height of a figure a chunk draws, in inches.
What knitr calls `fig.width' and `fig.height', and the same default:
the PNG device is opened at this size and 96 dots an inch, so a figure
of 7 by 5 inches is 672 by 480 pixels.  `overblock-image-height' then
caps what shows inline, and `overblock-run-save-image' writes the
original."
  :type '(cons (number :tag "Width") (number :tag "Height")))

(defcustom overblock-rmd-max-line-length 2000
  "Number of characters of a result line that show inline.
Zero shows all of them.  A line longer than this is cut, and the cut is
marked with an ellipsis.

One long line is one line, so `overblock-rmd-max-lines' does not bound
it, and a block laid out on every redisplay costs what it holds: a
`print' of a wide matrix or a long vector is one such line."
  :type 'natnum)


;;;; The result of a chunk

(defvar-keymap overblock-rmd-result-map
  :doc "Keymap inside a chunk that shows a result, empty on purpose.
overblock-rmd binds no keys; put your own here.
`overblock-run-toggle-output' is the natural candidate:

  (keymap-set overblock-rmd-result-map \"C-c C-o\"
              #\\='overblock-run-toggle-output)")

(defun overblock-rmd--strip-prompt (text)
  "Return TEXT without the prompt R wrote when the chunk was done.
Call this in the shell buffer, where `inferior-ess-primary-prompt' has
its value.

The prompt after the output, and nothing in the middle: a chunk goes to
R as one statement, so one prompt comes back and it comes back last.
The newlines before the first line of output go as well, but not the
spaces: R prints its tables with the header indented, and the columns
of a `summary' line up on exactly those spaces.

`inferior-ess-primary-prompt' and not `comint-prompt-regexp', which is
the variable a comint filter would normally ask.  ess-tracebug — on by
default — hands the prompt to `comint-output-filter' with
`comint-prompt-regexp' bound to \"^$\", so that comint does not erase a
prompt tracebug wrote itself; this runs from that same filter, where
the real value of that variable is therefore out of reach.  Measured: every
result came back with a bare > on a line of its own."
  (let ((rx (concat "\\(?:" inferior-ess-primary-prompt "\\)")))
    ;; The match is at least the newline the pattern opens with, so the
    ;; text shrinks on every turn and the loop ends.
    (while (string-match (concat "\n[ \t]*" rx "[ \t\n]*\\'") text)
      (setq text (substring text 0 (match-beginning 0))))
    ;; A prompt with nothing before it: the chunk printed nothing at
    ;; all, which is what an assignment does.
    (when (string-match-p (concat "\\`[ \t\n]*" rx "[ \t\n]*\\'") text)
      (setq text ""))
    text))

(defun overblock-rmd--figures (text)
  "Return TEXT with each figure line replaced by the image it names.
The wrapper `overblock-rmd--send' puts around a chunk writes one line
for every PNG the chunk drew, `overblock-figure:' and the path.  Each
becomes what comint-mime hands the Python notebook: one space carrying
the image, with the file's bytes in it, so the block, the save button
and the pop-out read a figure of R as they read one of Python.  Where
this Emacs draws no PNG the line names the file instead.

The newline before the line goes with it, so a figure follows the text
of the chunk without a blank row between them."
  (if (not (string-search "overblock-figure:" text))
      text
    (replace-regexp-in-string
     "\n?overblock-figure:.+"
     (lambda (line)
       (let ((file (substring line (1+ (string-search ":" line)))))
         (if (and (image-type-available-p 'png) (file-readable-p file))
             (propertize " " 'display
                         (create-image (with-temp-buffer
                                         (set-buffer-multibyte nil)
                                         (insert-file-contents-literally file)
                                         (buffer-string))
                                       'png t))
           (format "[figure %s]" file))))
     text t t)))

(defun overblock-rmd--clean (text)
  "Return TEXT as a result block can show it.
The prompt goes, the figures come in, and the copy is cut loose from
the shell; see `overblock-rmd--strip-prompt', `overblock-rmd--figures'
and `overblock-repl-detach' for what each of those means.  Call this in
the shell buffer."
  (overblock-repl-detach
   (overblock-rmd--figures (overblock-rmd--strip-prompt text))))

;;;; The chunks

(defconst overblock-rmd-chunk-regexp
  "^[[:blank:]]*```+[[:blank:]]*{[[:blank:]]*[rR][[:blank:],}]"
  "What the opening line of an R chunk looks like.
The engine name whole and then whatever may follow it — a blank before
the name of the chunk, a comma before its options, or the closing brace —
so a ```{python} chunk of the same file is left alone and a
```{rmarkdown} one is not mistaken for R.")

(defun overblock-rmd-chunks ()
  "Return the R chunks of the buffer, in order.
Each is a list (OPEN CODE-BEG CODE-END): where the opening fence line
begins, and the code between the two fences.  CODE-END is where the
closing fence line begins, so the region is the code lines whole, their
last newline included — the newline a result block hangs on.

A chunk with no code in it is left out: there is nothing to run and
nothing to hang a result on.  The fences come from
`overblock-md-preview-fences', which is the walk that knows a fence
from a line that looks like one."
  (let (chunks)
    (dolist (fence (overblock-md-preview-fences (point-max)))
      (save-excursion
        (goto-char (car fence))
        (when (looking-at-p overblock-rmd-chunk-regexp)
          (forward-line 1)
          (let ((code-beg (point))
                (code-end (save-excursion (goto-char (cdr fence)) (pos-bol))))
            (when (< code-beg code-end)
              (push (list (car fence) code-beg code-end) chunks))))))
    (nreverse chunks)))

(defun overblock-rmd--chunk-at (&optional pos)
  "Return the chunk POS, or point, stands in, or nil for none.
Both fence lines count as part of the chunk, so a click on the bar and
a point at the end of the code find the same one."
  ;; ponytail: the whole buffer is walked for one answer, which is what
  ;; a pass down a file of chunks pays once a chunk.  A file where that
  ;; is too much wants the walk cached against
  ;; `buffer-chars-modified-tick'.
  (let ((pos (or pos (point))))
    (seq-find (lambda (chunk)
                (and (<= (nth 0 chunk) pos)
                     (<= pos (save-excursion
                               (goto-char (nth 2 chunk))
                               (pos-eol)))))
              (overblock-rmd-chunks))))

(defun overblock-rmd--chunk-name (bol eol)
  "Return the name written in the chunk header BOL..EOL, or nil.
The word after the engine and before the first comma or brace, as knitr
reads it: ```{r plot-one, echo=FALSE} is called plot-one.  An option
written where a name would stand is not one, which is why the word has
to end at a comma or a brace: ```{r echo=FALSE} names no chunk."
  (save-excursion
    (goto-char bol)
    (when (re-search-forward
           (concat "```+[[:blank:]]*{[[:blank:]]*[rR][[:blank:]]+"
                   "\\([^,}=[:blank:]]+\\)[[:blank:]]*[,}]")
           eol t)
      (match-string-no-properties 1))))

(defun overblock-rmd--prose (beg end)
  "Return the prose blocks of the buffer between BEG and END, in order.
The paragraphs, and not the fences: what a fence holds is code, and a
chunk of R is run rather than rendered.  This is what
`overblock-md-preview-regions-function' is set to, so the live cycle of
`overblock-md-preview' renders the prose and leaves the chunks alone."
  (seq-filter (lambda (region)
                (and (< (car region) (cdr region))
                     (<= beg (car region) end)))
              (overblock-md-preview-paragraphs
               end (overblock-md-preview-fences end))))


;;;; The bar over a chunk header

(defun overblock-rmd--bar (open)
  "Draw the bar over the opening fence line that begins at OPEN.
A bar already there is drawn again rather than replaced, so its own
state — the label and the width it was cut for — is what
`overblock-bar-draw' compares against.

The glyph in front of the label is the R logo of the devicons, the
family the Python notebook draws its snake and its markdown mark from;
the label is the chunk's name, or the language where it has none, as a
code cell is called python."
  (save-excursion
    (goto-char open)
    (let* ((bol (pos-bol))
           (eol (pos-eol))
           (there (overblock-bar-in bol (min (point-max) (1+ eol))))
           (ov (if (eq (overblock-bar-kind there) 'chunk)
                   there
                 (overblock-bar-over bol eol))))
      ;; Text typed at the end of the line is outside the overlay, and
      ;; the bar then covered the header only as far as it reached when
      ;; the line was shorter.
      (move-overlay ov bol eol)
      (overblock-bar-draw
       ov 'chunk
       (concat (overblock-glyph "" "◆" "R") " "
               (or (overblock-rmd--chunk-name bol eol) "R"))
       (overblock-buttons overblock-rmd-chunk-buttons)
       'overblock-bar))))

(defun overblock-rmd--hide-fence (close)
  "Hide the closing fence line that begins at CLOSE.
Three backquotes under a result read as litter: the chunk has a bar
above it and its result a bar of its own, and the fence between them
says nothing a reader needs.  The line is hidden and not merely blanked,
so nothing is left standing where it was.

Not painted over, either.  Font lock gives the fence the background of
`markdown-code-face\', and the face of the text under a display string
is what wins over the string\'s own: measured on a frame, a rule drawn
there came out as a band of that same grey however the face that drew it
was written.

The overlay is one of this mode\'s bars, so `overblock-rmd--bars\' sweeps
it away with the rest when the chunk it closes is gone."
  (save-excursion
    (goto-char close)
    (let* ((bol (pos-bol))
           (end (min (point-max) (1+ (pos-eol))))
           (there (overblock-bar-in bol end))
           (ov (if (eq (overblock-bar-kind there) 'chunk-end)
                   there
                 (make-overlay bol end nil t))))
      (overlay-put ov 'evaporate t)
      (overlay-put ov 'overblock-bar 'chunk-end)
      (overlay-put ov 'invisible t)
      (move-overlay ov bol end))))

(defun overblock-rmd--bars ()
  "Bar the header of every R chunk, and drop the bars of what is not one.
Called from the idle cycle rather than from a change hook: a file of
many chunks is walked once the reader has stopped rather than once a
keypress, and a bar appears a moment after the header that wants it is
written."
  (let* ((chunks (overblock-rmd-chunks))
         (opens (mapcar #'car chunks))
         ;; The line the closing fence begins, which is where
         ;; `overblock-rmd-chunks' ends the code.
         (closes (mapcar (lambda (chunk) (nth 2 chunk)) chunks)))
    (dolist (bar (overblock-bars))
      (let ((kind (overblock-bar-kind bar))
            (bol (save-excursion (goto-char (overlay-start bar)) (pos-bol))))
        (when (or (and (eq kind 'chunk) (not (memql bol opens)))
                  (and (eq kind 'chunk-end) (not (memql bol closes))))
          (delete-overlay bar))))
    (mapc #'overblock-rmd--bar opens)
    (mapc #'overblock-rmd--hide-fence closes)))

(defun overblock-rmd-render-buffer ()
  "Bar every chunk of the buffer and render the prose between them.
Both on the one idle timer: `overblock-live-start' calls this when the
reader stops, and each of the two walks the buffer once."
  (overblock-rmd--bars)
  (overblock-md-preview-render-buffer))


;;;; R at the other end

(defun overblock-rmd--r-processes ()
  "Return the names of the R processes ESS has running, dead ones aside.
Of R and of nothing else: `ess-process-name-list' holds every inferior
ESS of the session, and a Julia or a Stata is no use to a chunk of R."
  (update-ess-process-name-list)
  (seq-filter (lambda (name)
                (when-let* ((proc (get-process name)))
                  (equal "R" (buffer-local-value 'ess-dialect
                                                 (process-buffer proc)))))
              (mapcar #'car ess-process-name-list)))

(defun overblock-rmd--process ()
  "Return the live R process of this buffer, or nil for none.
`ess-local-process-name' is where ESS keeps the answer, and
`overblock-rmd--start' is what puts it there.

Where this buffer has no answer yet and exactly one R runs, that one is
adopted and its name written there, as `ess-request-a-process' would
answer for the same buffer.  A getter with a side effect, and the
reason is `overblock-rmd-restart': a file that had not run a chunk yet
found no process of its own to restart, so it quietly joined the R that
was already open — with everything that session had defined still in
it, which is the one thing a restart is asked for."
  (unless ess-local-process-name
    (when-let* ((names (overblock-rmd--r-processes))
                ((null (cdr names))))
      (setq-local ess-local-process-name (car names))))
  (when-let* ((name ess-local-process-name)
              (proc (get-process name))
              ((process-live-p proc)))
    proc))

(defun overblock-rmd--start ()
  "Attach an R process to this buffer, starting one where none runs.
The backend's `:start'.  It answers the process rather than nil, unlike
the Python notebook's: `inferior-ess' waits for the interpreter's first
prompt before it returns, so R is ready to take a chunk the moment this
comes back and nothing has to be armed on a prompt that has already
been.

`ess-force-buffer-current' is the ESS road in: it takes the one R that
runs, asks where there are several, and starts one where there is none.
It reads `ess-dialect', which the mode sets, because an Rmd buffer is
not an ESS buffer and would otherwise be asked which language to run."
  (ess-force-buffer-current "R process to use: ")
  (overblock-rmd--process))

(defun overblock-rmd--r-string (text)
  "Return TEXT as an R string literal, escapes and quotes and all.
The newlines go in escaped, so the whole chunk travels as one line: a
literal newline inside the string is legal R, but comint would send it
as a line of its own and R would answer with a continuation prompt in
the middle of the result."
  ;; `prin1-to-string' writes exactly this literal: quotes doubled,
  ;; backslashes doubled, and with `print-escape-newlines' the newlines
  ;; as \n.  Checked against the three regexps this replaced on
  ;; backslashes, doubled backslashes, quotes, newlines, tabs, carriage
  ;; returns and non-ASCII text: the same string every time.  R reads
  ;; the same escapes as Lisp prints, which is why one stands for the
  ;; other here.
  (let ((print-escape-newlines t))
    (prin1-to-string text)))

(defun overblock-rmd--send (proc beg end)
  "Send the chunk BEG..END to PROC, as the backend's `:send'.
Wrapped in a `source' of its own parse, so the chunk is one statement
and one prompt comes back at the end of it; `print.eval' is what makes
R print the value of every top level expression, as it does at its own
prompt and as a notebook cell does.  The commentary of this file says
why the lines cannot simply be sent.

Around the `source' a PNG device, opened before the chunk at
`overblock-rmd-figure-size' and closed after it whatever the chunk did:
a chunk that draws leaves a file for each page, and the exit names each
on a line of its own, which `overblock-rmd--figures' reads back.  Only
where R can draw a PNG at all; a chunk that draws nothing leaves no
file and names none.  The names are R's own temporary files, and go
with the session.

`ess-send-string' and not `ess-send-region': the region is not what
goes down, and `ess-send-region' hands a chunk to ess-tracebug where
that is on, which would wrap the wrapper.

A long chunk on one line is no trouble: R\'s console reads a line of
any length, and 48 kilobytes of escaped chunk — 700 statements — sent
to R 4.6 through a real pseudo terminal came back with the right answer
and no continuation prompt."
  (ess-send-string
   proc
   (format "local({.f <- tempfile(\"overblock-\", fileext = \"-%%03d.png\"); \
.png <- capabilities(\"png\"); \
if (.png) png(.f, width = %s, height = %s, units = \"in\", res = 96); \
on.exit({if (.png) invisible(dev.off()); \
for (.p in Sys.glob(sub(\"%%03d\", \"*\", .f, fixed = TRUE))) \
cat(\"\\noverblock-figure:\", .p, \"\\n\", sep = \"\")}); \
source(exprs = parse(text = %s), print.eval = TRUE)})"
           (car overblock-rmd-figure-size) (cdr overblock-rmd-figure-size)
           (overblock-rmd--r-string (buffer-substring-no-properties beg end)))
   nil))

(defun overblock-rmd--prompt-p (tail)
  "Return non-nil where TAIL ends at R's prompt.
`inferior-ess-primary-prompt' says what one looks like, and ESS's own
`inferior-ess--set-status' asks this same question of that same
variable.  Call this in the shell buffer, where it has its value."
  (string-match-p (concat inferior-ess-primary-prompt "\\'") tail))

(defconst overblock-rmd--error-regexp "^Error\\(?: in \\|: \\|\\'\\)"
  "What R writes at the start of a line when a chunk fails.
`Error in CALL : MESSAGE' where there is a call to name, `Error: '
where there is none, and a bare `Error' where the message follows on
the next line.")

(defun overblock-rmd--error-p (text)
  "Return non-nil where TEXT is the output of a chunk that failed.
A pass over the buffer stops at the first chunk this answers for.

A chunk whose own output has a line beginning `Error in ' — one that
prints a log it read from somewhere — answers as a failure too.  The
two cannot be told apart: R writes its errors to the same stream, in
the same shape, as the chunk writes everything else."
  (string-match-p overblock-rmd--error-regexp text))

(defun overblock-rmd--step ()
  "Run the chunk at point, and say whether to wait for its prompt.
The backend's `:step', which is how `overblock-run-next' walks a pass
down the buffer.  Every region a pass over an Rmd file queues is a
chunk that goes to R, so the walk always waits; the prose between the
chunks is rendered by the live cycle and is never queued.

A marker whose chunk the reader has deleted since finds nothing, and
the walk goes on to the next rather than stopping there."
  (when-let* ((chunk (overblock-rmd--chunk-at)))
    (overblock-run-region (nth 1 chunk) (nth 2 chunk))
    t))

(defun overblock-rmd--region-at ()
  "Return the chunk point is in as (OPEN . CODE-END), or nil for none.
From its opening fence, which is where `overblock-rmd--starts' marks a
chunk, to the end of its code, where its result hangs."
  (when-let* ((chunk (overblock-rmd--chunk-at)))
    (cons (nth 0 chunk) (nth 2 chunk))))

(defun overblock-rmd--starts ()
  "Return a marker on the opening fence of every chunk, in order."
  (mapcar (lambda (chunk) (copy-marker (nth 0 chunk)))
          (overblock-rmd-chunks)))

(defun overblock-rmd--backend ()
  "Return what `overblock-run' needs to drive an inferior R.
The commentary of `overblock-run' lists the slots.  There is no `:arm':
`overblock-rmd--start' answers with a process that has already
prompted, so nothing is ever waiting for one.  There is no `:done'
either, because no buffer follows a running chunk."
  (list :name "overblock-rmd"
        :unit "chunk"
        :process #'overblock-rmd--process
        :start #'overblock-rmd--start
        :send #'overblock-rmd--send
        :prompt-p #'overblock-rmd--prompt-p
        :clean #'overblock-rmd--clean
        :error-p #'overblock-rmd--error-p
        :step #'overblock-rmd--step
        :region-at #'overblock-rmd--region-at
        :starts #'overblock-rmd--starts
        :redraw #'overblock-rmd--bars
        :keymap overblock-rmd-result-map
        :buttons 'overblock-rmd-result-buttons
        :fold #'overblock-run-toggle-output
        :header-face 'overblock-bar
        :output-face 'overblock-body
        :lines 'overblock-rmd-max-lines
        :chars 'overblock-rmd-max-line-length))


;;;; The commands

(defun overblock-rmd--chunk-here (event)
  "Return the chunk at point, or the one whose bar EVENT clicked.
Signals a `user-error' where there is none: that is the answer the
commands give their reader."
  (overblock-goto-event event)
  (or (overblock-rmd--chunk-at)
      (user-error "No R chunk here")))

;;;###autoload
(defun overblock-rmd-run-chunk (&optional event)
  "Run the chunk at point, or the one whose button EVENT clicked.
The result grows below the code while it runs.  A chunk sent while
another one runs is queued behind it."
  (interactive (list last-input-event))
  (let ((chunk (overblock-rmd--chunk-here event)))
    (overblock-run-region (nth 1 chunk) (nth 2 chunk))))

;;;###autoload
(defun overblock-rmd-restart ()
  "Restart R, and remove every result of this buffer.
The renderings of the prose stay: a rendering has nothing to do with
the interpreter.

The process is killed and a fresh one started in its place.  ESS has no
restart that asks the reader nothing — `ess-quit' runs `ess-cleanup',
which offers to kill the buffers of the session — and starting again in
the same buffer is a case `inferior-ess' is written for."
  (interactive)
  (when-let* ((proc (overblock-rmd--process)))
    ;; End the running chunk first, and as a death: the interpreter it
    ;; waits for is about to go.  Its chunk can belong to another
    ;; buffer on the same R, whose block would otherwise keep a running
    ;; header — spinner and stopwatch frozen where the ticker stopped —
    ;; for the rest of the session.
    (with-current-buffer (process-buffer proc)
      (overblock-run-abort "R was restarted"))
    (overblock-run-queue-set nil)
    (delete-process proc)
    ;; ESS keeps the names of the processes it started in a list of its
    ;; own, and reads that list to find a free one.  Asked to refresh it
    ;; here, the dead process leaves its name behind and the new R takes
    ;; the same name and the same buffer.
    (update-ess-process-name-list))
  (overblock-run-clear-results)
  (overblock-rmd--start))

;;;###autoload
(defun overblock-rmd-restart-and-run-all ()
  "Restart R, then run every chunk of the buffer in order.
The pass stops at the first error, or on `overblock-run-stop'."
  (interactive)
  (overblock-rmd-restart)
  (overblock-run-cells (overblock-rmd--starts)
                       "overblock-rmd: running every chunk"))

;;;; The mode

(defvar-keymap overblock-rmd-mode-map
  :doc "Keymap of `overblock-rmd-mode', empty on purpose.
overblock-rmd binds no keys; put your own here.
`overblock-rmd-run-chunk' and `overblock-run-interrupt' are the natural
candidates:

  (keymap-set overblock-rmd-mode-map \"C-c C-c\" #\\='overblock-rmd-run-chunk)
  (keymap-set overblock-rmd-mode-map \"C-c C-k\" #\\='overblock-run-interrupt)")

;;;###autoload
(define-minor-mode overblock-rmd-mode
  "Run the R chunks of this buffer and show their results inline.
Every chunk gets a bar with a run button, the prose between the chunks
reads as it will look, and a click on a rendering gives its source
back.  Turn the mode off to remove the bars, the results and the
renderings.  The mode binds no keys: `overblock-rmd-mode-map' is empty
and yours to fill.

`overblock-md-command' is what renders the prose, and the prose stays
as it is where none of its candidates is installed; the chunks run
either way."
  :lighter " overblock-rmd"
  (if overblock-rmd-mode
      (progn
        ;; Both modes render prose through the same live cycle and the
        ;; same kind of block, so two of them in one buffer would each
        ;; take the other's renderings down.  This one renders the prose
        ;; of an Rmd file itself, so it is the one to keep.
        (when (bound-and-true-p overblock-md-preview-mode)
          (overblock-md-preview-mode -1)
          (message "overblock-rmd: overblock-md-preview-mode off, %s"
                   "this mode renders the prose itself"))
        ;; What the runner reads to know this is a notebook it may draw
        ;; in, and how to reach R.
        (overblock-run-attach (overblock-rmd--backend))
        ;; ESS asks these of the buffer it starts a process for, and an
        ;; Rmd buffer is no ESS buffer: without them
        ;; `ess-force-buffer-current' would ask the reader which
        ;; language to run before it ran anything.
        (setq-local ess-dialect "R")
        (setq-local ess-language "S")
        ;; The chunks are code and are run; the prose is what gets
        ;; rendered.
        (setq-local overblock-md-preview-regions-function
                    #'overblock-rmd--prose)
        (overblock-live-start 'md-preview #'overblock-rmd-render-buffer
                              overblock-md-preview-idle))
    (overblock-live-stop)
    (overblock-run-detach)
    (kill-local-variable 'ess-dialect)
    (kill-local-variable 'ess-language)
    (kill-local-variable 'overblock-md-preview-regions-function)))

;;;###autoload
(defun overblock-rmd-mode-maybe ()
  "Enable `overblock-rmd-mode' in a buffer visiting an Rmd file.
Made for a major mode hook, where your configuration adds it:

  (add-hook \\='markdown-mode-hook #\\='overblock-rmd-mode-maybe)

The package installs no hook itself: installing it must not change how
Emacs behaves."
  (when (and buffer-file-name
             (string-match-p "\\.[rR]md\\'" buffer-file-name))
    (overblock-rmd-mode)))

(provide 'overblock-rmd)
;;; overblock-rmd.el ends here
