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
;; ESS alone: no knitr run, no rendered document.
;;
;; Turn `overblock-rmd-mode' on in an Rmd buffer, and every ```{r}
;; chunk gets a bar with a run button.  The prose between the chunks
;; reads as it will look.  Running a chunk grows its result below the
;; code: a header bar with a spinner, a stopwatch and buttons, and the
;; output of R underneath.
;;
;; An Rmd file is the inverse of a Python notebook.  A `.py' notebook
;; is code with `# %%' lines cutting it into cells; an Rmd file is
;; markdown prose with fenced chunks of code inside it.  So this
;; package composes.  The chunks are the fences that `overblock-md'
;; finds, and the prose is its paragraphs, rendered by a live cycle as
;; `overblock-md-preview-mode' renders a markdown file.  The two modes
;; would render the same prose, so this one turns the preview off.  The
;; running and the result blocks belong to `overblock-run', which
;; `overblock-pycell' uses too, and so do the commands.  This file holds
;; what knows about R and Rmd: the chunks, the bar of a chunk, and the
;; calls into ESS that start R and send a chunk.
;;
;; A chunk reaches R as one statement, not as its own lines:
;;
;;     source(exprs = parse(text = "..."), print.eval = TRUE)
;;
;; That makes the result collectable.  Sent line by line, R prompts
;; after every statement, and the prompts land in the middle of the
;; output.  There nothing can tell them from output.  Wrapped, the chunk
;; is one statement and one prompt comes back at the end.  `source'
;; with `print.eval' prints the value of every top level expression, as
;; a notebook cell does and a bare `eval' does not.
;;
;; ESS's own send-and-collect does not fit.  `ess-command' is
;; synchronous and freezes Emacs for the length of a chunk.
;; `ess-async-command' is for background jobs, and long output escapes
;; into the process buffer.
;;
;; A figure comes back as knitr brings one in.  The wrapper opens a PNG
;; device before the chunk, closes it after, and names each file on a
;; line of its own.  The result reads those lines back as images.  A
;; figure then behaves as in the Python notebook: capped to the window,
;; saved with its button, popped out, and named in a terminal.
;; `overblock-rmd-figure-size' is knitr's `fig.width' and `fig.height'.
;;
;; Under polymode (`poly-markdown+r-mode') the buffer stays in its host
;; mode while this mode is on.  polymode shows an R chunk in an indirect
;; buffer when point enters it, and moves every overlay to that buffer.
;; The bars and blocks of this mode are overlays.  Their owners (the
;; live cycle, the runner) stay in the base buffer, and would draw them
;; all again.  The mode sets the polymode slot `keep-in-mode' to `host'.
;; The chunks still get fontification and indentation from polymode.
;; Only the ESS keymap inside a chunk is lost, and
;; `overblock-rmd-mode-map' reaches every line of the file.
;;
;; `overblock' draws the blocks, `overblock-md' turns markdown into a
;; string, and `overblock-run' sends a region to a shell and shows what
;; comes back.

;;; Code:

(require 'overblock)
(require 'overblock-run)
(require 'overblock-repl)
(require 'overblock-md)
(require 'eieio)                        ; `eieio-declare-slots', `eieio-oset'
(require 'ess-inf)
(require 'ess-r-mode)
(require 'seq)
(require 'subr-x)

;; The polymode slot that `overblock-rmd--stay-in-host' sets, declared
;; for the compiler.
(eieio-declare-slots keep-in-mode)

;; The preview this mode keeps off; see `overblock-rmd--no-preview'.
(defvar overblock-md-preview-mode)
(declare-function overblock-md-preview-mode "overblock-md-preview" (&optional arg))

;;;; Options

(defgroup overblock-rmd nil
  "Inline results for the R chunks of an Rmd file."
  :group 'ess
  :group 'overblock
  :prefix "overblock-rmd-")

(defcustom overblock-rmd-bar-buttons
  (overblock-run-bar-buttons "chunk")
  "The buttons on the bar of an R chunk, left to right.
An entry has the shape `overblock-buttons' reads.  A chunk bar is
drawn before the chunk runs, so `lines' means nothing here.

The default is `overblock-run-bar-buttons' worded for a chunk: the row
of the `.py' notebook without the two that move a cell, because a
chunk sits inside prose about it."
  :type overblock-button-type
  :set #'overblock-run-set-and-redraw)

(defcustom overblock-rmd-result-buttons
  (overblock-run-result-buttons "chunk" "figure")
  "The buttons on the header of a result, left to right.
An entry has the shape `overblock-buttons' reads.  The default is
`overblock-run-result-buttons' worded for a chunk: the row of the
`.py' notebook without the two that move a cell, because a chunk sits
inside prose about it.  The fold arrow and the spinner are not in this
list: they show the state of the result."
  :type overblock-button-type
  :set #'overblock-run-set-and-redraw)

(defcustom overblock-rmd-figure-size '(7 . 5)
  "Width and height of a figure a chunk draws, in inches.
This is knitr's `fig.width' and `fig.height', with the same default,
for a chunk whose header names neither.  A header such as ```{r plot,
fig.width=8, fig.height=3, dpi=120} sets its own size, as under knitr.
The PNG device uses 96 dots an inch unless the header says `dpi'.
`overblock-image-height' caps what shows inline, and
`overblock-run-save-image' writes the original."
  :type '(cons (number :tag "Width") (number :tag "Height")))

;;;; Regions

(defconst overblock-rmd-chunk-regexp
  "^[[:blank:]]*```+[[:blank:]]*{[[:blank:]]*[rR][[:blank:],}]"
  "What the opening line of an R chunk looks like.
The whole engine name, then a blank before the chunk name, a comma
before the options, or the closing brace.  So a ```{rmarkdown} chunk is
not taken for R.")

(defvar-local overblock-rmd--chunks nil
  "The chunks of the last walk, for `overblock-cached'.
The bars and a pass ask for the chunk of each start, and the walk reads
the whole buffer.")

(defun overblock-rmd-chunks ()
  "Return the R chunks of the buffer, in order.
Each is a list (OPEN CODE-BEG CODE-END): where the opening fence line
begins, and the code between the two fences.  CODE-END is where the
closing fence line begins, so the region holds whole code lines,
including the last newline, on which a result block hangs.

A chunk with no code is left out.  The fences come from
`overblock-md-fences'.  The walk is kept until the text or the
narrowing changes."
  (overblock-cached 'overblock-rmd--chunks #'overblock-rmd--walk))

(defun overblock-rmd--walk ()
  "Return the R chunks of the buffer, as `overblock-rmd-chunks' says."
  (let (chunks)
    (dolist (fence (overblock-md-fences))
      (save-excursion
        (goto-char (car fence))
        (when (looking-at-p overblock-rmd-chunk-regexp)
          (forward-line 1)
          (let ((code-beg (point))
                (code-end
                 (save-excursion
                   (goto-char (cdr fence))
                   ;; The closing fence line is not code. Without one,
                   ;; the code runs to the end of the buffer, and the
                   ;; last line is code, not a fence.
                   (if (save-excursion
                         (goto-char (pos-bol))
                         (looking-at-p
                          overblock-md-closing-fence-regexp))
                       (pos-bol)
                     (point)))))
            (when (< code-beg code-end)
              (push (list (car fence) code-beg code-end) chunks))))))
    (nreverse chunks)))

(defun overblock-rmd--chunk-at (&optional pos)
  "Return the chunk POS, or point, stands in, or nil for none.
Both fence lines count as part of the chunk, so a click on the bar and
a point at the end of the code find the same one."
  (let ((pos (or pos (point))))
    (seq-find (lambda (chunk)
                (and (<= (nth 0 chunk) pos)
                     (<= pos (save-excursion
                               (goto-char (nth 2 chunk))
                               (pos-eol)))))
              (overblock-rmd-chunks))))

(defun overblock-rmd--region-at ()
  "Return the chunk point is in as (OPEN . CODE-END), or nil for none.
From its opening fence, where `overblock-rmd--starts' marks it, to the
end of its code, where its result hangs."
  (when-let* ((chunk (overblock-rmd--chunk-at)))
    (cons (nth 0 chunk) (nth 2 chunk))))

(defun overblock-rmd--starts ()
  "Return a marker on the opening fence of every chunk, in order."
  (mapcar (lambda (chunk) (copy-marker (nth 0 chunk)))
          (overblock-rmd-chunks)))

(defun overblock-rmd--code-at ()
  "Return the code of the chunk point is in as (BEG . END), or nil.
This is the `:code-at' of the backend: the code between the fences."
  (when-let* ((chunk (overblock-rmd--chunk-at)))
    (cons (nth 1 chunk) (nth 2 chunk))))

(defun overblock-rmd--chunk-name (bol eol)
  "Return the name written in the chunk header BOL..EOL, or nil.
The name is the word after the engine and before the first comma or
brace, as knitr reads it: ```{r plot-one, echo=FALSE} is plot-one.  The
word must end at a comma or a brace, so ```{r echo=FALSE} names no
chunk."
  (save-excursion
    (goto-char bol)
    (when (re-search-forward
           (concat "```+[[:blank:]]*{[[:blank:]]*[rR][[:blank:]]+"
                   "\\([^,}=[:blank:]]+\\)[[:blank:]]*[,}]")
           eol t)
      (match-string-no-properties 1))))

(defun overblock-rmd--regions ()
  "Return the prose blocks of the buffer, in order.
The paragraphs, not the fences: a chunk is code that runs, so the live
cycle renders the prose and leaves the chunks alone."
  (overblock-md-regions 'prose-only))

;;;; Bars

(defun overblock-rmd--hide-fence (close)
  "Hide the closing fence line that begins at CLOSE, and return the overlay.
The chunk has a bar above it and its result has a bar, so the fence
between them tells the reader nothing.  The line is invisible, not
blank, so no empty row stays.

It is not painted over: font lock gives the fence the background of
`markdown-code-face', and the face of the text under a display string
wins over the face of the string.

The overlay is a bar of this mode, so `overblock-run-bars' removes it
when its chunk is gone."
  (save-excursion
    (goto-char close)
    ;; Only a fence line: an unclosed chunk ends at the end of the
    ;; buffer, on a line of code.
    (when (looking-at-p overblock-md-closing-fence-regexp)
      (let* ((bol (pos-bol))
             (end (min (point-max) (1+ (pos-eol))))
             (there (overblock-bar-in bol end))
             (ov (if (eq (overblock-bar-kind there) 'chunk-end)
                     there
                   (make-overlay bol end nil t))))
        (overlay-put ov 'evaporate t)
        (overlay-put ov 'overblock-bar 'chunk-end)
        (overlay-put ov 'invisible t)
        (move-overlay ov bol end)))))

(defun overblock-rmd--bar ()
  "Draw the bar over the chunk header point is on, and hide its closing fence.
This is the `:bar' of the backend, and returns both overlays.  A bar
that is already there is drawn again, not replaced, so
`overblock-bar-draw' compares against its state.

The glyph is the R logo of the devicons, the family of the Python
notebook glyphs.  The label is the chunk name, or R when there is
none."
  (when-let* ((chunk (overblock-rmd--chunk-at)))
    (list (overblock-bar-line (pos-bol) (pos-eol) 'chunk
                              (overblock-glyph "" "◆" "R")
                              (or (overblock-rmd--chunk-name (pos-bol) (pos-eol))
                                  "R")
                              (overblock-buttons overblock-rmd-bar-buttons))
          (overblock-rmd--hide-fence (nth 2 chunk)))))

;;;; Rendering

(defun overblock-rmd--show (beg end &optional html)
  "Render the prose BEG..END over its own source, and return the block.
HTML is the answer of the converter for it, where a batch converted
the buffer."
  (overblock-md-show beg end (overblock-md-source beg end) html 'default
                     :kind 'rmd
                     :keymap overblock-live-map
                     :help-echo "mouse-1: edit this text"))

;;;###autoload
(defun overblock-rmd-render-buffer ()
  "Render the prose of the buffer that is not rendered yet.
One asynchronous converter process does all of it, so the reader does
not wait.  `overblock-live-start' calls this again whenever the reader
stops."
  (interactive)
  (overblock-md-render-regions (overblock-rmd--regions) 'rmd
                               #'overblock-md-source #'overblock-rmd--show))

;;;; Backend

(defun overblock-rmd--r-processes ()
  "Return the names of the R processes ESS has running, dead ones aside.
Only R: `ess-process-name-list' holds every inferior ESS of the
session, Julia and Stata too."
  (update-ess-process-name-list)
  (seq-filter (lambda (name)
                (when-let* ((proc (get-process name)))
                  (equal "R" (buffer-local-value 'ess-dialect
                                                 (process-buffer proc)))))
              (mapcar #'car ess-process-name-list)))

(defun overblock-rmd--process ()
  "Return the live R process of this buffer, or nil for none.
ESS keeps the name in `ess-local-process-name', and
`overblock-rmd--start' sets it.

When this buffer has no name yet and exactly one R runs, this adopts
that R and sets the name, as `ess-request-a-process' does.  The side
effect is for `overblock-run-restart': a file that has not run a chunk
must still find the process to restart."
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
This is the `:start' of the backend.  It returns the process, unlike
the Python notebook: `inferior-ess' waits for the first prompt before
it returns, so R is ready for a chunk and nothing has to be armed.

`ess-force-buffer-current' takes the one R that runs, asks when there
are several, and starts one when there is none.  It reads
`ess-dialect', which the mode sets, because an Rmd buffer is not an
ESS buffer."
  ;; `inferior-ess' shows its console, which can push the Rmd file out
  ;; of view.
  (save-window-excursion
    (ess-force-buffer-current "R process to use: "))
  (overblock-rmd--process))

(defun overblock-rmd--restart (proc)
  "Kill PROC, where there is one, and start a new R in its place.
This is the `:restart' of the backend.  ESS has no restart that asks
nothing: `ess-quit' runs `ess-cleanup', which offers to kill the
buffers of the session."
  (when proc
    (delete-process proc)
    ;; Refreshed, the list drops the dead process, so the new R takes
    ;; the same name and buffer.
    (update-ess-process-name-list))
  (overblock-rmd--start))

(defun overblock-rmd--r-string (text)
  "Return TEXT as an R string literal, escapes and quotes and all.
The newlines are escaped, so the chunk travels as one line.  comint
sends a literal newline as a line of its own, and R answers with a
continuation prompt in the middle of the result."
  ;; R reads the escapes that `prin1-to-string' writes.
  (let ((print-escape-newlines t))
    (prin1-to-string text)))

(defun overblock-rmd--figure-size (open)
  "Return (WIDTH HEIGHT DPI) for the chunk whose header begins at OPEN.
These are the knitr options `fig.width', `fig.height' and `dpi' of the
header, else `overblock-rmd-figure-size' and 96 dots an inch.  Only a
literal number counts: an expression is for R to evaluate, so the
default applies."
  (save-excursion
    (goto-char open)
    (let ((eol (pos-eol)))
      (mapcar (lambda (option)
                (goto-char open)
                (if (re-search-forward
                     ;; Anchored at both ends, so `fig.width=2*w' does
                     ;; not read as 2.
                     (concat "[,{[:blank:]]" (regexp-quote (car option))
                             "[[:blank:]]*=[[:blank:]]*"
                             "\\([0-9.]+\\)[[:blank:]]*\\(?:[,}]\\|$\\)")
                     eol t)
                    (string-to-number (match-string 1))
                  (cdr option)))
              (list (cons "fig.width" (car overblock-rmd-figure-size))
                    (cons "fig.height" (cdr overblock-rmd-figure-size))
                    (cons "dpi" 96))))))

(defun overblock-rmd--send (proc beg end)
  "Send the chunk BEG..END to PROC, as the backend's `:send'.
The chunk is wrapped in a `source' of its own parse, so it is one
statement and one prompt comes back at its end.  `print.eval' makes R
print the value of every top level expression.  The commentary of this
file says why the lines are not sent one by one.

Around the `source', a PNG device opens before the chunk at the size
of `overblock-rmd--figure-size' and closes after it, whatever the
chunk did.  The exit names each page file on a line of its own, which
`overblock-repl-file-images' reads back.  This happens only where R can
draw a PNG.  The files are temporary files of the R session.

`ess-send-string', not `ess-send-region': the text sent is not the
region, and `ess-send-region' gives the chunk to ess-tracebug when that
is on, which wraps the wrapper.

The console of R reads a line of any length, so a long chunk on one
line is safe."
  (pcase-let ((`(,width ,height ,dpi)
               ;; The header is the line above the code.
               (overblock-rmd--figure-size
                (save-excursion (goto-char beg) (forward-line -1) (point)))))
    (ess-send-string
     proc
     (format "local({.f <- tempfile(\"overblock-\", fileext = \"-%%03d.png\"); \
.png <- capabilities(\"png\"); \
if (.png) png(.f, width = %s, height = %s, units = \"in\", res = %s); \
on.exit({if (.png) invisible(dev.off()); \
for (.p in Sys.glob(sub(\"%%03d\", \"*\", .f, fixed = TRUE))) \
cat(\"\\noverblock-figure:\", .p, \"\\n\", sep = \"\")}); \
source(exprs = parse(text = %s), print.eval = TRUE)})"
             width height dpi
             (overblock-rmd--r-string (buffer-substring-no-properties beg end)))
     nil)))

(defun overblock-rmd--prompt-p (tail)
  "Return non-nil where TAIL ends at R's prompt.
`inferior-ess-primary-prompt' says what a prompt looks like, as in
`inferior-ess--set-status'.  Call this in the shell buffer, where the
variable has its value."
  (string-match-p (concat inferior-ess-primary-prompt "\\'") tail))

(defun overblock-rmd--clean (text)
  "Return TEXT as a result block can show it.
The prompt goes, the figures come in, and the copy is cut loose from
the shell.  Call this in the shell buffer.

A chunk goes to R as one statement, so one prompt comes back, last.
The prompt is `inferior-ess-primary-prompt', not `comint-prompt-regexp':
ess-tracebug (on by default) calls `comint-output-filter' with
`comint-prompt-regexp' bound to \"^$\", and this runs from that filter.

The wrapper of `overblock-rmd--send' names each figure on a line of its
own, which `overblock-repl-file-images' reads back."
  (overblock-repl-detach
   (overblock-repl-file-images
    (overblock-repl-drop-prompt-face
     (overblock-repl-strip-trailing-prompt text inferior-ess-primary-prompt))
    "overblock-figure:")))

(defconst overblock-rmd--error-regexp "^Error\\(?: in\\>\\|:\\|$\\)"
  "What R writes at the start of a line when a chunk fails.
`Error in CALL : MESSAGE' where there is a call to name, also with the
call on the next line, `Error: ' where there is none, and a bare
`Error' where the message follows on the next line.")

(defun overblock-rmd--error-p (text)
  "Return non-nil where TEXT is the output of a chunk that failed.
A pass over the buffer stops at the first such chunk.

A line of the output that matches `overblock-rmd--error-regexp' marks
it: R writes its errors to the same stream as all other output."
  (string-match-p overblock-rmd--error-regexp text))

(defconst overblock-rmd--no-eval-regexp
  (concat "\\`.*[{,[:blank:]]eval[[:blank:]]*=[[:blank:]]*F\\(?:ALSE\\)?\\_>"
          "\\|^#|[[:blank:]]*eval:[[:blank:]]*false\\_>")
  "The chunk option that knitr reads as \"do not evaluate\".
On the opening fence line, as eval=FALSE, or on a #| line of the
code, as eval: false.")

(defun overblock-rmd--step ()
  "Run the chunk at point, and say whether to wait for its prompt.
This is the `:step' of the backend, with which `overblock-run-next'
walks a pass down the buffer.  The prose is never queued.

A pass skips a chunk that knitr does not evaluate, as knitr does; a
run of that one chunk still sends it.  A marker whose chunk is deleted
finds nothing, and the walk goes on to the next one."
  (when-let* ((chunk (overblock-rmd--chunk-at))
              ((not (let ((case-fold-search nil))
                      (string-match-p
                       overblock-rmd--no-eval-regexp
                       (buffer-substring-no-properties
                        (nth 0 chunk) (nth 2 chunk)))))))
    (overblock-run-region (nth 1 chunk) (nth 2 chunk))
    t))

(defun overblock-rmd--backend ()
  "Return what `overblock-run' needs to drive an inferior R.
The commentary of `overblock-run' lists the slots.  There is no `:arm':
`overblock-rmd--start' returns a process that has already prompted."
  (list :name "overblock-rmd"
        :unit "chunk"
        :process #'overblock-rmd--process
        :start #'overblock-rmd--start
        :restart #'overblock-rmd--restart
        :send #'overblock-rmd--send
        :prompt-p #'overblock-rmd--prompt-p
        :clean #'overblock-rmd--clean
        :error-p #'overblock-rmd--error-p
        :step #'overblock-rmd--step
        :region-at #'overblock-rmd--region-at
        :code-at #'overblock-rmd--code-at
        :starts #'overblock-rmd--starts
        :bar #'overblock-rmd--bar
        :buttons 'overblock-rmd-result-buttons))

;;;; Mode

(defvar-keymap overblock-rmd-mode-map
  :doc "Keymap of `overblock-rmd-mode', empty on purpose.
overblock-rmd binds no keys; put your own here.  The Python notebook
binds none either.  For example:

  (keymap-set overblock-rmd-mode-map \"C-<return>\" #\\='overblock-run-this)
  (keymap-set overblock-rmd-mode-map \"S-<return>\"
              #\\='overblock-run-and-step)
  (keymap-set overblock-rmd-mode-map \"C-c C-k\" #\\='overblock-run-interrupt)")

;;;###autoload
(define-minor-mode overblock-rmd-mode
  "Run the R chunks of this buffer and show their results inline.
Every chunk gets a bar with a run button, the prose between the chunks
shows as it will look, and a click on a rendering shows its source.
Turn the mode off to remove the bars, the results and the renderings.
The mode binds no keys: `overblock-rmd-mode-map' is empty.

`overblock-md-command' renders the prose.  When none of its candidates
is installed, the prose stays as it is and the chunks still run.

Under polymode the buffer stays in its host mode while this mode is on,
because polymode moves the overlays to an indirect buffer for each
chunk.  The chunks are still fontified and indented as R."
  :lighter " Rmd"
  (when overblock-rmd-mode
    (overblock-only-in 'overblock-rmd-mode 'markdown-mode))
  (if overblock-rmd-mode
      (progn
        (overblock-rmd--no-preview)
        (add-hook 'overblock-md-preview-mode-hook #'overblock-rmd--no-preview
                  nil t)
        (overblock-run-attach (overblock-rmd--backend))
        ;; Without these, `ess-force-buffer-current' asks which
        ;; language to run: an Rmd buffer is no ESS buffer.
        (setq-local ess-dialect "R")
        (setq-local ess-language "S")
        (overblock-rmd--stay-in-host)
        (add-hook 'polymode-init-host-hook #'overblock-rmd--stay-in-host nil t)
        (overblock-live-start 'rmd #'overblock-rmd-render-buffer))
    (overblock-live-stop 'rmd)
    (remove-hook 'overblock-md-preview-mode-hook #'overblock-rmd--no-preview t)
    (overblock-run-detach)
    (remove-hook 'polymode-init-host-hook #'overblock-rmd--stay-in-host t)
    (overblock-rmd--stay-in-host 'off)
    (kill-local-variable 'ess-dialect)
    (kill-local-variable 'ess-language)))

;;;###autoload
(defun overblock-rmd-mode-maybe ()
  "Enable `overblock-rmd-mode' in a buffer visiting an Rmd file.
Add it to a major mode hook:

  (add-hook \\='markdown-mode-hook #\\='overblock-rmd-mode-maybe)

The package installs no hook itself."
  (when (and buffer-file-name
             (string-match-p "\\.[rR]md\\'" buffer-file-name))
    (overblock-rmd-mode)))

;;;; Hooks

(defun overblock-rmd--stay-in-host (&optional off)
  "Keep polymode from leaving this buffer for an inner one, or let it, with OFF.
The commentary of this file says why.  Nothing happens where polymode
is off.

Called when the mode goes on and from `polymode-init-host-hook':
polymode runs `markdown-mode-hook', which turns the mode on, before it
sets `pm/polymode'."
  (when (bound-and-true-p pm/polymode)
    (eieio-oset pm/polymode 'keep-in-mode (unless off 'host))))

(defun overblock-rmd--no-preview ()
  "Turn `overblock-md-preview-mode' off, because this mode renders the prose.
Both would render the same paragraphs over each other.  Called when
this mode goes on, and from the preview's own hook after that, so the
order in which a configuration turns the two on does not matter."
  (when (bound-and-true-p overblock-md-preview-mode)
    (overblock-md-preview-mode -1)
    (message "overblock-rmd: overblock-md-preview-mode off, %s"
             "this mode renders the prose itself")))

(provide 'overblock-rmd)
;;; overblock-rmd.el ends here
