;;; overblock-run.el --- A region sent to a shell, and the result shown  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Marcel Arpogaus

;; Author: Marcel Arpogaus <znepry.necbtnhf@tznvy.pbz>
;; Assisted-by: Claude:claude-opus-5
;; Keywords: convenience, tools
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

;; A notebook is a buffer of regions and a shell to send them to.  Send
;; one, watch what the shell prints, notice the prompt that says it is
;; done, and show the result under the region.  That loop is the same
;; for every language, and this file holds it.  It holds the run state,
;; the queue of a pass over the buffer and the result block.  It also
;; holds the ticker that mirrors a running region five times a second,
;; and the filter that waits for the prompt.
;;
;; Nothing here knows a language.  One plist says what one is.
;;
;; `overblock-run-backend' is the notebook.  It is a buffer-local plist
;; that `overblock-run-attach' sets for the mode of the notebook.  A
;; send copies it into the shell buffer for the filter and the ticker.
;; The shell and the regions:
;;
;;   :name      the word messages carry, as in "NAME: stopped at error"
;;   :unit      what a region is called in a message: "cell", "chunk"
;;   :process   () -> the live shell process, or nil.  In the notebook
;;   :start     () -> start one; the process where it is ready to take a
;;              region at once, nil where it will only prompt later
;;   :arm       (THUNK) -> run THUNK on the first prompt of a new or
;;              restarted shell.  Optional: without it, a shell prompts
;;              at once
;;   :send      (PROC BEG END) -> send the region.  In the notebook
;;   :prompt-p  (TAIL) -> non-nil where TAIL ends at a prompt.  In the shell
;;   :clean     (TEXT) -> TEXT as a block can show it.  In the shell
;;   :error-p   (TEXT) -> non-nil where the region failed, which stops a pass
;;   :step      () -> run whatever is at point, and answer non-nil where
;;              the walk must wait for a prompt before the next one
;;   :region-at () -> (BEG . END) of the region point is in, or nil
;;   :starts    () -> a marker on the start of every region, in order
;;   :redraw    () -> draw the mode's own bars again, for a new width or
;;              a new button list.  Optional
;;
;; And the look of a result block, which `overblock-run-show' draws:
;;
;;   :buttons      the option that holds the button descriptors, a symbol
;;   :stale        a function called with each new result block, which
;;                 makes it go on an edit, `overblock-stale-when-edited'
;;                 where the backend names none
;;
;; The button option is named, not copied, because the reader can
;; customize it while the notebook is open.  The bar has the face
;; `overblock-bar' and the body `overblock-body', in every notebook.
;;
;; `overblock-pycell' sends Python cells to an inferior Python, and
;; `overblock-rmd' sends the R chunks of an Rmd file to an inferior R.
;; Each keeps its own buttons and options.  The commands (run
;; what is above or below, stop, interrupt, fold, copy and discard a
;; result) are below, the same in both.

;;; Code:

(require 'overblock)
(require 'overblock-repl)
(require 'seq)
(require 'subr-x)
(require 'map)
(require 'ansi-color)
(require 'vtable)

(defvar-local overblock-run-backend nil
  "The backend of the shell of this buffer, a plist, or nil.
The commentary of this file lists the slots.  The mode of a notebook
sets it and removes it when turned off, so the runner draws only in a
buffer that has one.

`overblock-run--send' copies it into the shell buffer, where the filter
and the ticker read it.")

(defun overblock-run--call (slot &rest args)
  "Call SLOT of the backend of this buffer on ARGS, or return nil."
  (when-let* ((fn (plist-get overblock-run-backend slot)))
    (apply fn args)))

(defun overblock-run--must ()
  "Return the backend of this buffer, or signal that it is no notebook.
A command of a notebook mode is autoloaded and can be called anywhere."
  (or overblock-run-backend
      (user-error "This buffer runs nothing: it has no notebook mode on")))

(defun overblock-run--name ()
  "Return the word that the messages of this backend carry."
  (or (plist-get overblock-run-backend :name) "overblock"))

(defun overblock-run--unit (&optional plural)
  "Return what this backend calls a region, PLURAL where that is asked."
  (concat (or (plist-get overblock-run-backend :unit) "region")
          (if plural "s" "")))

(defun overblock-run-set-and-redraw (symbol value)
  "Set SYMBOL to VALUE, and draw every notebook again.
This is the `:set' of the options that blocks on the screen follow,
such as the buttons of a bar and how much of a result shows, so a
change shows at once."
  (set-default symbol value)
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (when overblock-run-backend
        (overblock-bars-stale)
        (overblock-run--redraw)))))

(defcustom overblock-run-max-lines 12
  "Number of result lines that show inline, in every notebook.
Zero shows all of them.
A result block is one buffer line however tall it is, so a long
result makes one long step for `next-line' and for the wheel.  Use
`overblock-run-pop-output' to see all of it.

The cost of redisplay follows the number of face runs, not the length.
For width, see `overblock-run-max-line-length'.  A change applies to
the results on the screen."
  :type 'natnum
  :group 'overblock
  :set #'overblock-run-set-and-redraw)

(defcustom overblock-run-max-line-length 2000
  "Number of characters of a result line that show inline.
Zero shows all of them.  A line longer than this is cut, and the cut
is marked with an ellipsis; `overblock-run-pop-output' shows all of
it.

`overblock-run-max-lines' does not bound one long line, such as a
`print' of a long list or a base64 blob, and a block costs what it
holds on every redisplay.  A change applies to the results on the
screen."
  :type 'natnum
  :group 'overblock
  :set #'overblock-run-set-and-redraw)

(defcustom overblock-run-scroll t
  "Non-nil scrolls each window of a notebook to the region its pass runs.
Point goes there too.  Point and the view come back where the pass was
asked for when it ends.  The scrolling stops at the first command of
the reader that scrolls a window of the notebook or moves point in it,
so the reader can look at another part while the pass runs; the pass
then leaves point and the windows alone.  Nil never scrolls."
  :type 'boolean
  :group 'overblock)

(defvar-local overblock-run--view nil
  "The start of each window of this notebook as its pass began to scroll.
An alist of (WINDOW . MARKER): `overblock-run-go-home' gives each
window its view back.")

(defvar overblock-run--scrolled nil
  "The notebooks whose pass scrolls to its regions.")

(defvar overblock-run--seen nil
  "How the windows of `overblock-run--scrolled' were before this command.
A list of (WINDOW START VSCROLL POINT).  Only a command is the reader:
redisplay moves a start for a scroll margin, and output arrives
between commands, so neither stops the scrolling.")

(defvar-keymap overblock-run-result-map
  :doc "Keymap inside a region that shows a result, empty on purpose.
The runner binds no keys; put your own here, for the cells of a
Python notebook and the chunks of an Rmd file alike.  For example:

  (keymap-set overblock-run-result-map \"C-c C-o\"
              #\\='overblock-run-toggle-output)

Point never enters the block, so the overlays of the region carry
this map.")

(defun overblock-run--shorten (line chars)
  "Return LINE cut to CHARS characters.
The cut is marked with an ellipsis.  A CHARS of zero leaves the line
whole."
  (if (or (zerop chars)
          (<= (length line) chars))
      line
    (concat (substring line 0 chars)
            (overblock-glyph "…" "..."))))

(defconst overblock-run--interval 0.2
  "Seconds between two looks at the output of a running region.
The spinner turns one frame a tick, so `overblock-run-header' divides
the runtime by this to pick its glyph.")

(defun overblock-run--body-lines (lines chars)
  "Return LINES as they show inline.
Each is cut to CHARS characters, and nothing shows after the first line
with an image that can be drawn: more figures would make the block,
and the scroll step, grow without bound.  `overblock-repl-first-lines'
decides before this how many lines show.  A display without images
names them instead.  A line with an image is not cut, since the image
can be past the cut; its images are capped to `overblock-image-height'."
  (let (shown stop)
    (while (and lines (not stop))
      (let* ((l (pop lines))
             (imagep (overblock-image-in l))
             ;; A terminal shows only a space for an image, so it does
             ;; not stop there.
             (drawp (and imagep (display-images-p))))
        (push (cond (drawp (overblock-image-cap l))
                    ;; A label, not a blank row.
                    (imagep (overblock-run--shorten (overblock-image-label l)
                                                    chars))
                    (t (overblock-run--shorten l chars)))
              shown)
        (when drawp (setq stop t))))
    (nreverse shown)))

(defun overblock-run--mark (folded total runtime state)
  "Return the mark at the start of the bar of a result.
A spinner while the region runs, a warning where the interpreter went
away, a fold arrow where there is something to fold, and a tick where
the region printed nothing.  FOLDED, TOTAL, RUNTIME and STATE are those
of `overblock-run-header'.

The mark is the first character of the bar, as the glyph of every
other bar, so it aligns with the glyph of the cell above."
  (cond ((eq state 'running)
         ;; One frame for each tick. Braille, not a codicon: the icon
         ;; set has no frames, and these ten have one weight and size.
         (let ((frames (overblock-glyph "⠋⠙⠹⠸⠼⠴⠦⠧⠇⠏" "|/-\\")))
           (string (aref frames (mod (truncate runtime overblock-run--interval)
                                     (length frames))))))
        ((eq state 'died) (overblock-glyph "" "⚠" "!"))
        ;; A single line can still be tall: one image is one line, and
        ;; that is the block worth folding.
        ((> total 0)
         (overblock-button (if folded
                               (overblock-glyph "" "▸" ">")
                             (overblock-glyph "" "▾" "v"))
                           "Fold or unfold this result"
                           #'overblock-run-toggle-output))
        ;; Nothing printed.
        (t (overblock-glyph "" "✓" "."))))

(defun overblock-run-result-buttons (unit picture)
  "Return the five buttons every result header carries.
UNIT is the name of a region in a tooltip (a cell, a chunk), and
PICTURE the name of a picture in a result (an image, a figure).

Both notebooks draw these five, in this order, so a `.py' file and an
Rmd file show the same row.  A notebook adds its own buttons after
them.  An entry has the shape `overblock-buttons' reads."
  `((stop ("" "□" "stop") ,(format "Interrupt this %s, and stop the pass" unit)
          overblock-run-interrupt running)
    (save-image ("" "↧" "save") ,(format "Save the result's %s to a file" picture)
                overblock-run-save-image image)
    (copy ("" "◫" "copy") "Copy this result" overblock-run-copy-output lines)
    (pop ("" "↗" "pop") "Show this result in its own buffer"
         overblock-run-pop-output lines)
    (discard ("" "✕" "drop") "Discard this result"
             overblock-run-discard-output done)))

(defun overblock-run-header (folded total shown runtime state imagep)
  "Return the header bar of a result, as the backend of this buffer says.
FOLDED is non-nil when only the header shows.
TOTAL and SHOWN count the lines and the inline subset.  RUNTIME is the
time in seconds since the cell started.  STATE is `running' while the
cell runs, `died' where the interpreter went away before the cell
ended, `failed' where the backend calls the result an error, and nil
where the cell finished.  IMAGEP marks a result with an image."
  (let* ((icons (overblock-buttons (symbol-value (plist-get overblock-run-backend :buttons))
                                   imagep total (eq state 'running)))
         (mark (overblock-run--mark folded total runtime state))
         (label (cond ((> total 0)
                       (format "%d line%s%s" total (if (= total 1) "" "s")
                               (if (and (not folded) (< shown total))
                                   (format ", showing %d" shown) "")))
                      ((not state) "no output")))
         (failed (and (eq state 'failed) (propertize "error" 'face 'error)))
         (time (format "%.1fs" runtime)))
    (overblock-bar
     mark (string-join (delq nil (list failed label time)) " · ")
     icons 'overblock-bar)))

(defun overblock-run-restart (reason restart)
  "End what runs and take point home, drop the queue and results, RESTART.
REASON goes to a region still running, through `overblock-run-abort':
its region can be in another buffer on the same shell, whose block
would otherwise keep a frozen running header.

RESTART is called with the old process, or nil where there was none,
and starts the new interpreter, which is the job of the notebook.  A
region asked for before its first prompt waits for it."
  (let ((proc (overblock-run--call :process)))
    ;; The pass ends here: point and the windows go back, so a pass
    ;; asked for at once starts where the first one did. Before the
    ;; abort, which drops the home as a death.
    (overblock-run-go-home)
    (when proc
      (with-current-buffer (process-buffer proc)
        (overblock-run-abort reason)))
    (overblock-run--queue-set nil)
    (overblock-run-clear-results)
    (funcall restart proc)
    (overblock-run--arm)))

;;;###autoload
(defun overblock-run-clear-results ()
  "Take the results of this buffer down, and sweep what lost its anchor.
Other renderings stay (the prose of an Rmd file, the markdown cells of
a notebook).  A clear that names a kind cannot sweep an orphan,
because an orphan has no kind, so the sweep is explicit: else the
cloak of a lost block keeps lines invisible."
  (interactive)
  (overblock-clear nil nil 'result)
  (overblock-sweep-orphans))

(defun overblock-run-update (block)
  "Make the header and the body of the result BLOCK again, and show them.
The lines are counted once for both: the header says how many there
are and how many show, and the body is those that show."
  (let* ((data (overblock-get block :data))
         (folded (plist-get data :folded))
         (text (plist-get data :text))
         (total (plist-get data :total)))
    (let* ((empty (string-empty-p text))
           (max overblock-run-max-lines)
           (chars overblock-run-max-line-length)
           (lines (unless empty (overblock-repl-first-lines text max)))
           (shown (overblock-run--body-lines lines chars))
           ;; Counted once and kept, or a fold scans the whole output
           ;; on every keypress.
           (count (cond (empty 0)
                        (total)
                        (t (let ((n (overblock-repl-count-lines text)))
                             (overblock-set block :data
                                            (plist-put data :total n))
                             n)))))
      (overblock-set block :header
                     (overblock-run-header folded count (length shown)
                                           (plist-get data :runtime)
                                           (plist-get data :state)
                                           (and lines
                                                (overblock-image-in text))))
      (overblock-set block :body
                     (when (and shown (not folded))
                       (overblock-faced (string-join shown "\n") 'overblock-body)))
      (overblock-refresh block))))

(defun overblock-run-show (beg end text runtime &optional state total)
  "Show TEXT as the result of the region BEG..END, as the backend says.
RUNTIME is the time in seconds since the cell started.  STATE is
`running' while the cell runs, `died' where the interpreter went away
before the cell ended, `failed' where the backend calls the result an
error, and nil where the cell finished.

Empty TEXT gets a header that says \"no output\", so the cell shows as
evaluated.  A replaced result keeps its fold state.  TOTAL is the
number of lines the cell printed, for a running cell whose TEXT is only
the part that shows; without it the lines of TEXT are counted."
  (let* ((old (car (overblock-in beg end 'result)))
         (data (list :folded (and old (plist-get (overblock-get old :data)
                                                 :folded))
                     :text text :runtime runtime :state state :total total)))
    (if (and old (= (overlay-start old) beg))
        ;; The ticker comes here five times a second with new data only.
        ;; Keeping the block saves two overlays and a scan per tick.
        (progn (overblock-set old :data data)
               (overblock-run-update old)
               old)
      ;; The newline that ends the cell carries the result, so the last
      ;; cell of the buffer gets one. Without the restriction, because
      ;; `point-max' of a narrowing can be inside the buffer. A
      ;; read-only buffer keeps its text: this runs in the process
      ;; filter, where an error would leave the shell busy.
      (without-restriction
        (when (and (= end (point-max)) (not (eq (char-before end) ?\n)))
          (ignore-error buffer-read-only
            (save-excursion (goto-char end) (insert "\n")))))
      (let ((block (overblock-show beg end
                                   :kind 'result
                                   :data data
                                   :keymap overblock-run-result-map)))
        ;; An empty cell has no newline of its own, and `overblock-show'
        ;; returns nil. No error: this runs in the process filter.
        (when block
          (funcall (or (plist-get overblock-run-backend :stale)
                       #'overblock-stale-when-edited)
                   block)
          (overblock-run-update block))
        block))))

(defvar-local overblock-run--queue nil
  "The regions a pass, or a reader pressing early, has left to run.
`overblock-run--queued' says what an entry is.
`overblock-run-cells', `overblock-run-on-prompt' and
`overblock-run--enqueue' fill it, and `overblock-run-next' empties it.
It is local to the shell buffer, beside `overblock-run--state', so each
shell has its own queue.  `overblock-run-shell' finds it.")

(defvar-local overblock-run--follower nil
  "What a buffer that follows one result knows of it: (SHELL . REGION).
SHELL is the buffer of the interpreter of the result, and REGION the
marker of the run on its region, or nil where the result had ended
when the buffer was made.  A follower is no notebook and has no
backend; its shell is this one, never another from the settings.
`overblock-run-interrupt' compares the region, so it stops only the
cell the buffer shows.

The variable is local where the buffer is a follower, whatever its
value, also when the shell is gone.")

(defun overblock-run-shell ()
  "Return the buffer that holds the queue and the run state for this one.
That is the shell: this buffer where it is one, the shell of a
follower, else the shell this notebook sends to.  Return nil where
there is no shell, and then nothing is queued.

The backend is copied into the shell, because the filter, the ticker
and the walk armed on the first prompt read it there, also before the
first send."
  (if (local-variable-p 'overblock-run--follower)
      (let ((shell (car overblock-run--follower)))
        (and (buffer-live-p shell) shell))
    (when-let* ((proc (overblock-run--call :process))
                (shell (process-buffer proc)))
      (unless (buffer-local-value 'overblock-run-backend shell)
        (let ((backend overblock-run-backend))
          (with-current-buffer shell
            (setq-local overblock-run-backend backend))))
      shell)))

(defun overblock-run-running-region ()
  "Return the region the shell of this buffer runs, as (BEG . END).
Markers in the buffer of the region, which can be another one: two
notebooks can share a shell.  Return nil where nothing runs.  Public
because a caller that moves text must know what must not move."
  (when-let* ((shell (overblock-run-shell))
              (state (buffer-local-value 'overblock-run--state shell))
              (beg (plist-get state :beg)))
    (cons beg (plist-get state :end))))

(defvar-local overblock-run--home nil
  "Where point goes in the notebook when the queue of this shell ends.
While the pass scrolls (see `overblock-run-scroll'), point walks
down the notebook.  Where the pass scrolled to its end, point then goes
back to where the pass was asked for.")

(defvar-local overblock-run--armed nil
  "The process of this shell that has not prompted yet since it started.
`overblock-run-next' runs on that prompt and clears it.")

(defun overblock-run--queued ()
  "Return the regions a pass still has to run, in order.
Each is a marker, where the `:step' of the backend decides what runs,
or a cons of two markers for a region the reader sent, which runs as
it was sent."
  (when-let* ((shell (overblock-run-shell)))
    (buffer-local-value 'overblock-run--queue shell)))

(defun overblock-run-go-home ()
  "Put point back where the pass that has just ended was asked for.
The windows that show the notebook go there too, because a window
keeps its own point while its buffer is not selected, and each window
shows again what it showed as the pass began to scroll.  Where the pass
did not scroll to its end (see `overblock-run-scroll'), point and the
windows stay where the reader put them."
  (when-let* ((shell (overblock-run-shell))
              (home (buffer-local-value 'overblock-run--home shell)))
    ;; Freed first, also when the notebook is killed: comint adjusts
    ;; every marker of the buffer on every insertion.
    (with-current-buffer shell (setq overblock-run--home nil))
    (when (buffer-live-p (marker-buffer home))
      (with-current-buffer (marker-buffer home)
        (when (memq (current-buffer) overblock-run--scrolled)
          (goto-char home)
          (dolist (window (get-buffer-window-list nil nil t))
            (when-let* ((start (alist-get window overblock-run--view)))
              (set-window-start window start t))
            (set-window-point window home)))
        (overblock-run--scroll-stop)))
    (set-marker home nil)))

(defun overblock-run--scroll-start ()
  "Have the pass of this notebook scroll to its regions, where the option says."
  (when overblock-run-scroll
    (dolist (view overblock-run--view) (set-marker (cdr view) nil))
    (setq overblock-run--view
          (mapcar (lambda (window)
                    (cons window (copy-marker (window-start window))))
                  (get-buffer-window-list nil nil t)))
    (add-to-list 'overblock-run--scrolled (current-buffer))
    (add-hook 'pre-command-hook #'overblock-run--scroll-see)
    (add-hook 'post-command-hook #'overblock-run--scroll-check)
    ;; This command can move point after it started the scrolling.
    (overblock-run--scroll-see)))

(defun overblock-run--scroll-stop ()
  "Stop the pass of this notebook from scrolling to its regions.
The command hooks go with the last notebook that scrolls."
  (setq overblock-run--scrolled (delq (current-buffer) overblock-run--scrolled))
  (dolist (view overblock-run--view) (set-marker (cdr view) nil))
  (setq overblock-run--view nil)
  (unless overblock-run--scrolled
    (setq overblock-run--seen nil)
    (remove-hook 'pre-command-hook #'overblock-run--scroll-see)
    (remove-hook 'post-command-hook #'overblock-run--scroll-check)))

(defun overblock-run--scroll-windows ()
  "Return (WINDOW START VSCROLL POINT) for each window of a scrolling notebook."
  (setq overblock-run--scrolled (seq-filter #'buffer-live-p overblock-run--scrolled))
  (mapcan (lambda (buffer)
            (mapcar (lambda (window)
                      (list window (window-start window)
                            (window-vscroll window t) (window-point window)))
                    (get-buffer-window-list buffer nil t)))
          overblock-run--scrolled))

(defun overblock-run--scroll-see ()
  "Note how the windows of the scrolling notebooks are before a command."
  (setq overblock-run--seen (overblock-run--scroll-windows)))

(defun overblock-run--scroll-check ()
  "Stop the scrolling in each notebook whose window the command just moved."
  (dolist (now (overblock-run--scroll-windows))
    (let ((before (assq (car now) overblock-run--seen)))
      (when (and before (not (equal before now)))
        (with-current-buffer (window-buffer (car now))
          (overblock-run--scroll-stop)))))
  ;; A notebook killed while it scrolls takes no stop with it.
  (unless overblock-run--scrolled (overblock-run--scroll-stop)))

(defun overblock-run--scroll-to (m)
  "Put point at M, and every window that shows this notebook there too.
The windows stay while a minibuffer is open: its exit gives them back
as they were, which would read as a scroll of the reader.  The next
region catches up."
  (goto-char m)
  (unless (> (minibuffer-depth) 0)
    (dolist (window (get-buffer-window-list nil nil t))
      (set-window-point window m)
      (set-window-start window m)))
  ;; The pass moved them, not the reader, also inside a command.
  (overblock-run--scroll-see))

(defun overblock-run--home-set (marker)
  "Give the shell MARKER as the place its pass came from, or nil for none.
A marker starts the scrolling of its notebook, where
`overblock-run-scroll' says.  A marker it held before is freed: comint
adjusts every marker of the shell on every insertion."
  (when-let* ((shell (overblock-run-shell)))
    (with-current-buffer shell
      (overblock-run--home-drop)
      (setq overblock-run--home marker))
    (when marker
      (with-current-buffer (marker-buffer marker)
        (overblock-run--scroll-start)))))

(defun overblock-run--home-drop ()
  "Free the home of this shell, and stop the scrolling of its notebook.
Call this in the shell buffer; it works also when its process is gone."
  (when (markerp overblock-run--home)
    (when-let* ((notebook (marker-buffer overblock-run--home)))
      (with-current-buffer notebook (overblock-run--scroll-stop)))
    (set-marker overblock-run--home nil))
  (setq overblock-run--home nil))

(defun overblock-run--queue-set (cells)
  "Give the shell CELLS to run, and return them."
  (when-let* ((shell (overblock-run-shell)))
    (with-current-buffer shell (setq overblock-run--queue cells))))

(defvar-local overblock-run--state nil
  "State of the region that runs in this shell, or nil.
A plist:

  :from   where the output of the region starts in this buffer
  :beg    :end  the region in its own buffer
  :tail   the recent output, for the prompt detection
  :start  the `float-time' of the send
  :timer  the ticker
  :head   the part of the output the block shows, once it can no
          longer change
  :count  (POSITION . LINES) counted up to POSITION, so a tick reads
          only what arrived since the one before it
  :follow (BUFFER . MARKER), the buffer that follows the run and how
          far it has copied

The :head and :count slots belong to the live mirror.")

(defun overblock-run--whole-escapes (text)
  "Return TEXT without an escape sequence that has not arrived in full.
comint-mime sends an image as one escape sequence, and half of one
hides everything after it until the rest comes.

The match is anchored at the end, so `substring' cuts it: it is faster
than `replace-regexp-in-string', which copies the text twice."
  (if (string-match "\e\\][^\e]*\\'" text)
      (substring text 0 (match-beginning 0))
    text))

(defun overblock-run--output-so-far (from)
  "Return the output of the running cell after FROM, cleaned.
An incomplete escape sequence at the end is dropped: comint-mime
renders it only when it is complete."
  (overblock-run--call :clean
                       (overblock-run--whole-escapes
                        (buffer-substring from (point-max)))))

(defun overblock-run-output-head (from)
  "Return as much of the output after FROM as the block can show.
Call this in the shell, where the `:clean' of the backend takes the
prompts off.  `overblock-run--body-lines' shows only the first lines,
so a tick does not read or clean all the output.  When those lines are
complete, the text cannot change and is kept, and later ticks read
nothing.  A block that shows every line reads everything on every
tick.

The read is also bounded in characters, for output of few long lines,
but only where no escape sequence starts inside the bound: a cut
inside the escape of an image drops the figure.  Nothing is kept while
the head is empty: an incomplete escape sequence hides what follows."
  (or (plist-get overblock-run--state :head)
      (let* ((lines overblock-run-max-lines)
             (chars overblock-run-max-line-length)
             (budget (and (> chars 0)
                          (> lines 0)
                          ;; What `overblock-run--body-lines' can show.
                          (* lines (1+ chars))))
             (limit (if (zerop lines)
                        (point-max)
                      (save-excursion
                        (goto-char from)
                        ;; `:clean' trims the leading blank lines.
                        (skip-chars-forward " \t\n")
                        (forward-line (+ lines 4))
                        (point))))
             ;; The bound in characters, unless an escape sequence
             ;; starts inside it (see the docstring).
             (limit (if (and budget
                             (> (- limit from) budget)
                             (not (save-excursion
                                    (goto-char from)
                                    (search-forward
                                     "\e]" (min (point-max) (+ from budget))
                                     t))))
                        (+ from budget)
                      limit))
             (text (overblock-run--call :clean
                                        (overblock-run--whole-escapes
                                         (buffer-substring from limit)))))
        (when (and (< limit (point-max))
                   (not (string-empty-p text)))
          (setq overblock-run--state (plist-put overblock-run--state :head text)))
        text)))

(defun overblock-run-total (from)
  "Return the number of lines the running cell has printed after FROM.
Lines are counted as they arrive, so a tick does not read all the
output again.  Leading blank lines do not count, as the `:clean' of
the backend drops them, so the count agrees with the finished cell."
  (let* ((state (or (plist-get overblock-run--state :count)
                    (cons (save-excursion
                            (goto-char from)
                            (skip-chars-forward " \t\n")
                            (point-marker))
                          0)))
         (count (cdr state)))
    (save-excursion
      ;; `count-lines' counts in C, much faster than a loop. A partial
      ;; last line is counted below.
      (goto-char (point-max))
      (let ((bol (pos-bol)))
        (setq count (+ count (count-lines (car state) bol)))
        (goto-char bol))
      ;; Moved, not made again: comint adjusts every marker of the
      ;; buffer on every insertion.
      (setq overblock-run--state
            (plist-put overblock-run--state :count
                       (cons (set-marker (car state) (point)) count))))
    (if (and (> (point-max) (marker-position from))
             (not (eq (char-before (point-max)) ?\n)))
        (1+ count)
      count)))

(defun overblock-run--show-in-notebook (beg fin text seconds state &optional total)
  "Show TEXT as the result of the region BEG..FIN, where it can be shown.
Nothing happens where the notebook is gone or its mode is off: the
mode removes the blocks when turned off, and a block put back would
have no bars and no hooks.  A mode that is off has no backend."
  (when (buffer-live-p (marker-buffer beg))
    (with-current-buffer (marker-buffer beg)
      (when overblock-run-backend
        (overblock-run-show beg fin text seconds state total)))))

(defun overblock-run--release (&rest markers)
  "Point every marker of MARKERS nowhere, and ignore what is not one.
A marker stays in the chain of its buffer until a garbage collection,
and comint adjusts the whole chain on every insertion."
  (dolist (marker markers)
    (when (markerp marker) (set-marker marker nil))))

(defun overblock-run--end (text &optional died)
  "End the running region and show TEXT as its final result.
The one exit for every way a run ends; DIED marks abnormal ends.
Call this in the shell buffer.

Nothing happens where no cell is running: a failing send can end its
cell through the filter and then signal, and the handler calls this a
second time.  `overblock-run-abort' checks the same."
  (when overblock-run--state
    (pcase-let (((map (:from from) :beg (:end fin) :start :timer :follow
                      (:count count))
                 overblock-run--state)
                (failed nil))
      ;; The last output, then all of it cleaned: the tail of a
      ;; follower is raw, and its final lines come with the prompt.
      (overblock-run--follow-tick)
      (setq overblock-run--state nil)
      (cancel-timer timer)
      ;; Else the next single cell takes point to the old home.
      (when died
        (setq overblock-run--queue nil)
        (overblock-run--home-drop))
      (setq failed (and (not died) (overblock-run--call :error-p text)))
      (overblock-run--show-in-notebook beg fin text (- (float-time) start)
                                       (cond (died 'died) (failed 'failed)))
      (when-let* ((buffer (car-safe follow))
                  ((buffer-live-p buffer)))
        (overblock-run--follow-done buffer text))
      ;; Free the markers of the run.
      (overblock-run--release from beg fin (car-safe count) (cdr-safe follow))
      (overblock-run--continue failed))))

(defun overblock-run--continue (failed)
  "Go on with the pass after a region ended, FAILED or not.
A failure ends the pass and takes point home; otherwise
`overblock-run-next' goes on."
  (if (and failed (or overblock-run--queue overblock-run--home))
      (progn (setq overblock-run--queue nil)
             (message "%s: stopped at error" (overblock-run--name))
             (overblock-run-go-home))
    (overblock-run-next)))

(defun overblock-run-abort (&optional reason)
  "End the running cell abnormally, because its prompt will not return.
A death notice, with the exit status when one is available, follows
the output received so far.  This covers a dead interpreter (the
ticker finds it), a killed shell buffer and a shell restart, which
reinitializes the major mode; hence this is on `kill-buffer-hook' and
`change-major-mode-hook' in the shell.

REASON says what happened, for a caller that knows: a restart is not
an unexpected death."
  (when overblock-run--state
    (let* ((proc (get-buffer-process (current-buffer)))
           (out (overblock-run--output-so-far (plist-get overblock-run--state :from)))
           (msg (propertize
                 (or reason
                     (format "Process unexpectedly died%s"
                             (if proc
                                 (format " (%s %s)" (process-status proc)
                                         (process-exit-status proc))
                               "")))
                 'face 'error)))
      (overblock-run--end (if (string-empty-p out) msg (concat out "\n" msg))
                          t))))

(defun overblock-run-follow (buffer)
  "Have the running region copy what it prints into BUFFER as it prints it.
Call this in the notebook.  Nothing happens where nothing is running.

The output lands in the shell, so the marker of what is copied is
there, in the record of the run."
  (when-let* ((shell (overblock-run-shell)))
    (with-current-buffer shell
      (when overblock-run--state
        (setq overblock-run--state
              (plist-put overblock-run--state :follow
                         (cons buffer
                               (copy-marker (plist-get overblock-run--state :from)))))
        ;; The output so far, not an empty buffer until the next tick.
        (overblock-run--follow-tick)))))

(defun overblock-run--follow-tick ()
  "Copy what the region has printed since the last look into its buffer.
Call this in the shell buffer.

Only what is new, so a tick does not copy the whole output.

Point at the end of the buffer follows the output, in the buffer and in
every window that shows it; point anywhere else stays."
  (when-let* ((follow (plist-get overblock-run--state :follow))
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

(defun overblock-run--tick (buf timer)
  "Mirror the output and the stopwatch of the running region into its block.
TIMER runs this every `overblock-run--interval' seconds for the shell BUF.
It cancels itself when nothing runs there anymore."
  (if (not (and (buffer-live-p buf)
                (buffer-local-value 'overblock-run--state buf)))
      (cancel-timer timer)
    (with-current-buffer buf
      (if (not (process-live-p (get-buffer-process buf)))
          (overblock-run-abort)
        (pcase-let (((map (:from from) :beg (:end fin) :start) overblock-run--state))
          (let* ((text (overblock-run-output-head from))
                 (total (if (string-empty-p text) 0 (overblock-run-total from))))
            (overblock-run--follow-tick)
            (overblock-run--show-in-notebook beg fin text (- (float-time) start)
                                             'running total)))))))

(defun overblock-run--filter (output)
  "Watch OUTPUT for the closing prompt, then end the running region.
The filter stays on `comint-output-filter-functions' and idles while
nothing runs; the ticker does the live mirroring."
  (when overblock-run--state
    ;; A chunk boundary can split the prompt, so match a capped tail;
    ;; `ansi-color-filter-apply' drops the escape sequences.
    (let ((tail (concat (plist-get overblock-run--state :tail)
                        (ansi-color-filter-apply output))))
      (setq overblock-run--state
            (plist-put overblock-run--state :tail
                       (string-limit tail 256 t)))
      (when (overblock-run--call :prompt-p tail)
        ;; To the end of the buffer; `:clean' takes the prompt off. Not
        ;; `comint-last-prompt': comint calls any last line without a
        ;; newline a prompt, which can be inside split output.
        (overblock-run--end
         (overblock-run--call
          :clean
          (buffer-substring (plist-get overblock-run--state :from)
                            (point-max))))))))

(defun overblock-run--send (proc start end)
  "Send START..END to PROC as the running region and track it.
Call this in the notebook: the backend is copied from here into the
shell buffer, where the filter and the ticker read it."
  (let ((beg (copy-marker start))
        (fin (copy-marker end t))
        (backend (overblock-run--must)))
    (with-current-buffer (process-buffer proc)
      (when overblock-run--state
        (user-error "The %s shell is still busy" (overblock-run--name)))
      (setq-local overblock-run-backend backend)
      ;; Idempotent. The filter idles while no cell runs; the other two
      ;; catch the shell going away. The filter is appended, so it runs
      ;; after comint-mime and the copy carries the images.
      (add-hook 'comint-output-filter-functions #'overblock-run--filter t t)
      (add-hook 'kill-buffer-hook #'overblock-run-abort nil t)
      (add-hook 'kill-buffer-hook #'overblock-run--home-drop nil t)
      (add-hook 'change-major-mode-hook #'overblock-run-abort nil t)
      ;; The ticker gets its own timer, so it can cancel itself.
      (let (timer)
        (setq timer (run-with-timer
                     overblock-run--interval overblock-run--interval
                     (let ((buffer (current-buffer)))
                       (lambda () (overblock-run--tick buffer timer)))))
        ;; The process mark, not the end of the buffer: comint inserts
        ;; output at the mark, and a late render of comint-mime can be
        ;; past it. ponytail: such a late render goes into the result
        ;; of the next cell.
        (setq overblock-run--state (list :from (copy-marker (process-mark proc))
                                         :beg beg :end fin :tail ""
                                         :start (float-time) :timer timer
                                         :head nil :count nil))))
    (overblock-run-show beg fin "" 0.0 'running nil)
    ;; The send can fail (an error, or `C-g'), and the state says a
    ;; region runs. A failed send ends the cell as a death, which also
    ;; empties the queue, else the shell stays busy.
    (condition-case error
        (overblock-run--call :send proc beg fin)
      ((error quit)
       (with-current-buffer (process-buffer proc)
         (overblock-run--end (propertize (error-message-string error) 'face 'error)
                             t))
       (signal (car error) (cdr error))))))

(defun overblock-run--pass-over ()
  "End the pass: say that it is done, and take point home.
Here, where the queue runs out, and not where a run ends: the last
cell of a pass can be one the notebook answered itself."
  (when-let* ((shell (overblock-run-shell))
              ((buffer-local-value 'overblock-run--home shell)))
    (message "%s: done" (overblock-run--name)))
  (overblock-run-go-home))

(defun overblock-run-next ()
  "Run the regions of the queue of the shell until one has to wait.
Point walks down the notebook while the pass scrolls (see
`overblock-run-scroll').  Called from the shell on its first prompt and
from `overblock-run--end' when a region finishes, so the queue is
reached through `overblock-run-shell'.

The `:step' of the backend runs what is at point, and says whether the
walk must wait: a region sent to the shell waits, and one the notebook
handles itself (a markdown cell) does not.

A loop, not recursion: a recursive call per markdown cell can reach
`max-lisp-eval-depth', and each frame would run its tail on the way
out."
  (when-let* ((shell (overblock-run-shell)))
    (with-current-buffer shell (setq overblock-run--armed nil)))
  (catch 'waiting
    (while t
      (let* ((cells (overblock-run--queued))
             (entry (car cells))
             (m (if (consp entry) (car entry) entry)))
        (unless m
          (overblock-run--pass-over)
          (throw 'waiting nil))
        (overblock-run--queue-set (cdr cells))
        (unless (buffer-live-p (marker-buffer m))
          (overblock-run--queue-set nil)
          (throw 'waiting nil))
        (with-current-buffer (marker-buffer m)
          (when (overblock-run--step entry m)
            (throw 'waiting nil)))))))

(defun overblock-run--step (entry m)
  "Run the queue ENTRY that begins at M, and say whether to wait.
A pair of markers is a region the reader sent, and goes as it is; a
marker alone is where the `:step' of the backend decides what runs.

While the pass scrolls, the region goes to the top of every window that
shows the notebook (see `overblock-run--scroll-to'), so the code that
runs is visible, and
`overblock-run-go-home' gives point back when the pass ends.  Where it
does not scroll, point and the windows stay as they are."
  (if (memq (current-buffer) overblock-run--scrolled)
      (progn (overblock-run--scroll-to m)
             (overblock-run--step-at entry))
    (save-excursion
      (goto-char m)
      (overblock-run--step-at entry))))

(defun overblock-run--step-at (entry)
  "Run the queue ENTRY at point, and say whether to wait."
  (if (consp entry)
      (progn (overblock-run--send (overblock-run--call :process)
                                  (car entry) (cdr entry))
             t)
    (overblock-run--call :step)))

(defun overblock-run-on-prompt (cells message)
  "Arm CELLS to run on the first prompt of the shell, and say MESSAGE.
The queue is set after `overblock-run--arm': a shell that signals
there leaves nothing set."
  (overblock-run--arm)
  (overblock-run--home-set (point-marker))
  (overblock-run--queue-set cells)
  (message "%s" message))

(defun overblock-run--arm ()
  "Run the queue on the first prompt of the shell, which is busy until then.
For a shell that has not prompted yet: one just started or restarted.
A region asked for meanwhile waits on the queue.  A process is armed
once, however often this is called before its prompt, and a stop
keeps the wait: a region sent before the prompt would get the start-up
banner.  A restart ends a wait for a prompt that never comes.  A
backend with no `:arm' has a shell that prompts at once, and nothing
waits."
  (when-let* (((plist-get overblock-run-backend :arm))
              (shell (overblock-run-shell))
              ((not (process-live-p
                     (buffer-local-value 'overblock-run--armed shell)))))
    (overblock-run--call :arm #'overblock-run-next)
    (with-current-buffer shell
      (setq overblock-run--armed (get-buffer-process shell)))))

(defun overblock-run--pass (cells message)
  "Put CELLS on the queue of the shell and start the pass, saying MESSAGE."
  (overblock-run--home-set (point-marker))
  (overblock-run--queue-set cells)
  (condition-case err
      (overblock-run-next)
    ;; A refused pass clears its home, so point does not jump later.
    (error (overblock-run--queue-set nil)
           (overblock-run--home-set nil)
           (signal (car err) (cdr err))))
  (message "%s" message))

(defun overblock-run-cells (cells message)
  "Run CELLS in order, and say MESSAGE, or what was queued where busy.
Each region goes on the prompt of the one before it: the queue is in
the shell, and `overblock-run-next' takes the next one.  The
interpreter starts where there is none: one that is ready at once
starts the pass now, one that prompts later starts it then.

A region that will not start cancels the whole pass:
`overblock-run--pass' empties the queue on a signal.  While the shell
is busy (see `overblock-run--busy-p'), CELLS go behind what is queued,
as a region does."
  (overblock-run--must)
  (cond
   ((overblock-run--busy-p)
    (overblock-run--enqueue cells))
   ((or (overblock-run--call :process) (overblock-run--call :start))
    (overblock-run--pass cells message))
   (t (message "%s: starting the interpreter…" (overblock-run--name))
      (overblock-run-on-prompt cells message))))

(defun overblock-run-region (start end)
  "Run START..END, starting the interpreter where there is none.
A region sent while the shell is busy (see `overblock-run--busy-p')
goes on the queue and runs when the shell is free; the pass stops if
the running region fails.  While the interpreter starts, the region
waits on the queue for its first prompt, so a second one waits behind."
  (overblock-run--must)
  ;; Markers here, in the notebook: the queue is in the shell, and
  ;; `copy-marker' of a number uses the current buffer.
  ;; One region leaves point and the windows alone; a pass it joins
  ;; goes on as it was.
  (let ((region (lambda () (list (cons (copy-marker start) (copy-marker end t)))))
        (overblock-run-scroll nil))
    (cond
     ((overblock-run--busy-p)
      (overblock-run--enqueue (funcall region)))
     ((when-let* ((proc (or (overblock-run--call :process)
                            (overblock-run--call :start))))
        (overblock-run--send proc start end)
        t))
     (t (overblock-run-on-prompt
         (funcall region)
         (format "%s: starting the interpreter…" (overblock-run--name)))))))

(defun overblock-run--busy-p ()
  "Non-nil where the shell runs a region, or has not prompted yet.
The shell waits for the first prompt of its process after a start or
a restart.  A process that died leaves nothing to wait for."
  (when-let* ((shell (overblock-run-shell)))
    (with-current-buffer shell
      (or overblock-run--state (process-live-p overblock-run--armed)))))

(defun overblock-run--enqueue (entries)
  "Put the queue ENTRIES behind whatever the shell is running.
Each request queues what it asks for, as in Jupyter: a region asked
for twice runs twice, in the order asked.  The home is set here where
none is set.  A pass or a region takes over the home of its notebook
where that notebook does not scroll, as after a waiting region or a
pass that no longer scrolls; a pass then scrolls where
`overblock-run-scroll' says.  Point comes back to the home where the
pass scrolled to its end (see `overblock-run-go-home')."
  ;; A click on another cell moved point in this command: the scrolling
  ;; stops now, not at the end of the command.
  (overblock-run--scroll-check)
  (let ((home (buffer-local-value 'overblock-run--home (overblock-run-shell))))
    (when (or (not home)
              (and (eq (marker-buffer home) (current-buffer))
                   (not (memq (current-buffer) overblock-run--scrolled))))
      (overblock-run--home-set (point-marker))))
  (overblock-run--queue-set (append (overblock-run--queued) entries))
  (message "%s: %s queued"
           (overblock-run--name)
           (if (cdr entries)
               (format "%d %s" (length entries) (overblock-run--unit t))
             (concat "this " (overblock-run--unit)))))

;;;; The notebook and its commands

(defun overblock-run-attach (backend)
  "Make this buffer a notebook that runs through BACKEND.
The mode of a notebook calls this as it goes on, and
`overblock-run-detach' as it goes off.  The results and the bars are
drawn again when the width changes, through
`overblock-width-functions'."
  (setq-local overblock-run-backend backend)
  (add-hook 'overblock-width-functions #'overblock-run--redraw nil t))

(defun overblock-run-detach ()
  "Stop this buffer being a notebook, and take its results and bars down.
Every block goes, whatever made it."
  (kill-local-variable 'overblock-run-backend)
  (remove-hook 'overblock-width-functions #'overblock-run--redraw t)
  (mapc #'delete-overlay (overblock-bars))
  (overblock-clear))

(defun overblock-run--redraw ()
  "Draw the results and the bars of this notebook again.
For a new width or a new button list.  A result is drawn again from
its record, as a tick does, and the `:redraw' of the backend draws the
bars of the mode."
  (dolist (block (overblock-in (point-min) (point-max) 'result))
    (overblock-run-update block))
  (overblock-run--call :redraw))

(defun overblock-run--result-at (event)
  "Return the result block at point, or at the click in EVENT.
Point first, then anywhere in the region around it.  Signal a
`user-error' where there is no result."
  (overblock-goto-event event)
  (overblock-run--must)
  (or (overblock-at 'result)
      (when-let* ((region (overblock-run--call :region-at)))
        (car (overblock-in (car region) (cdr region) 'result)))
      (user-error "No result here")))

(defun overblock-run--result-text (block)
  "Return the text of the result BLOCK.
While the region runs, that is only the part that shows, and a message
says so."
  (let ((data (overblock-get block :data)))
    (when (eq (plist-get data :state) 'running)
      (message "%s: the %s is still running, so this is only what shows"
               (overblock-run--name) (overblock-run--unit)))
    (plist-get data :text)))

;;;###autoload
(defun overblock-run-save-image (&optional event)
  "Save the first image of the result at point, or of the one in EVENT.
The file type comes from the image descriptor, which `create-image'
read from the magic bytes of the data."
  (interactive (list last-input-event))
  (let* ((text (overblock-run--result-text (overblock-run--result-at event)))
         (img (or (overblock-image-in text)
                  (user-error "No image in this result")))
         (data (or (plist-get (cdr img) :data)
                   (user-error "This image carries no data")))
         (type (plist-get (cdr img) :type))
         (file (read-file-name
                "Save image to: " nil nil nil
                (format "figure.%s" (if (eq type 'jpeg) "jpg" type)))))
    (let ((coding-system-for-write 'no-conversion))
      ;; MUSTBENEW: ask before overwriting.
      (write-region data nil file nil nil nil t))
    (message "%s: image saved to %s" (overblock-run--name) file)))

(defvar-keymap overblock-run-pop-map
  :doc "Keymap in a buffer that shows one result, empty on purpose.
The runner binds no keys; put your own here.  `overblock-run-interrupt'
and `overblock-run-stop' act on the shell of the result.  The buffer is
read-only, so a plain key is free:

  (keymap-set overblock-run-pop-map \"i\" #\\='overblock-run-interrupt)"
  :parent special-mode-map)

(defun overblock-run--insert-result (text)
  "Insert TEXT as a popped-out result, in the current buffer.
A table goes in live, as a copy: the bindings of vtable work, and
vtable aligns the columns for this window.  The table of the result
belongs to the shell buffer.  The text around a table goes in too."
  (let ((pos 0)
        (len (length text))
        (drawn nil))
    (while (< pos len)
      (let ((table (get-text-property pos 'overblock-repl-table text))
            (next (or (next-single-property-change
                       pos 'overblock-repl-table text)
                      len)))
        (cond
         ;; The padding between the runs of a table has no property, so
         ;; one table comes in several pieces and is drawn once.
         ((and table (not (eq table drawn)))
          (vtable-insert (overblock-repl-table-copy table))
          ;; `vtable-insert' leaves point after the header row.
          (goto-char (point-max))
          (setq drawn table))
         (table nil)
         (t
          (let ((part (substring text pos next)))
            ;; A figure is a space with an image; without images, a label.
            (insert (if (display-images-p) part
                      (overblock-image-label part))))))
        (setq pos next)))))

(defun overblock-run--follow-done (buffer text)
  "Put TEXT, the whole of what the region printed, into BUFFER.
The tail the run wrote there is raw, with the prompts of the shell.
The finished buffer holds what a result popped out later holds."
  (with-current-buffer buffer
    (let* ((inhibit-read-only t)
           (end (point-max))
           (at-end (= (point) end))
           ;; Every window: `erase-buffer' puts them all at 1.
           (following (seq-filter (lambda (window)
                                    (= (window-point window) end))
                                  (get-buffer-window-list buffer nil t))))
      (erase-buffer)
      (overblock-run--insert-result text)
      (goto-char (if at-end (point-max) (point-min)))
      (dolist (window following)
        (set-window-point window (point-max))))))

;;;###autoload
(defun overblock-run-pop-output (&optional event)
  "Show the result at point, or the one clicked in EVENT, in a buffer.
Each region gets one buffer, named after the notebook and the line the
region starts on, so results can be compared side by side.

A running region writes all its output there, so a long run can be
followed in a window of its own.  Point at the end of that buffer
follows the output; elsewhere it stays.  When the region ends the
buffer is written once more, without the prompts and with live
tables."
  (interactive (list last-input-event))
  (let* ((ov (overblock-run--result-at event))
         (runningp (eq (plist-get (overblock-get ov :data) :state) 'running))
         ;; Not `overblock-run--result-text', which returns only the
         ;; head; a follower gets all the output.
         (text (if runningp "" (overblock-run--result-text ov)))
         (buffer (get-buffer-create
                  (format "*%s: %s:%d*" (overblock-run--name) (buffer-name)
                          (line-number-at-pos (overlay-start ov)))))
         (shell (overblock-run-shell)))
    (with-current-buffer buffer
      (special-mode)
      (use-local-map overblock-run-pop-map)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (overblock-run--insert-result text))
      (goto-char (point-max))
      ;; Set also without a shell, so `overblock-run-interrupt' never
      ;; reaches the shell of another notebook.
      (setq overblock-run--follower
            (cons shell (and runningp shell
                             (plist-get (buffer-local-value
                                         'overblock-run--state shell)
                                        :beg)))))
    (when runningp (overblock-run-follow buffer))
    (pop-to-buffer buffer)))

;;;###autoload
(defun overblock-run-toggle-output (&optional event)
  "Fold or unfold the result at point, or the one clicked in EVENT.
This is what the fold mark of a result runs."
  (interactive (list last-input-event))
  (let* ((block (overblock-run--result-at event))
         (data (overblock-get block :data)))
    (overblock-set block :data
                   (plist-put data :folded (not (plist-get data :folded))))
    (overblock-run-update block)))

;;;###autoload
(defun overblock-run-discard-output (&optional event)
  "Discard the result at point, or the one clicked in EVENT."
  (interactive (list last-input-event))
  (overblock-delete (overblock-run--result-at event)))

;;;###autoload
(defun overblock-run-copy-output (&optional event)
  "Copy the result at point, or the one clicked in EVENT.
The copy keeps its text properties, so images survive a yank."
  (interactive (list last-input-event))
  (kill-new (overblock-run--result-text (overblock-run--result-at event)))
  (message "%s: result copied" (overblock-run--name)))

;;;###autoload
(defun overblock-run-above (&optional event)
  "Run every region above the one at point, or above the one EVENT clicked.
They run in order and the pass stops at the first error, or on
`overblock-run-stop'.  The interpreter keeps its state."
  (interactive (list last-input-event))
  (overblock-goto-event event)
  (overblock-run--must)
  (let* ((beg (car (or (overblock-run--call :region-at)
                       (user-error "No %s here" (overblock-run--unit)))))
         (starts (seq-take-while (lambda (m) (< m beg))
                                 (overblock-run--call :starts))))
    (unless starts (user-error "No %s above this one" (overblock-run--unit)))
    (overblock-run-cells starts (format "%s: running the %s above"
                                        (overblock-run--name)
                                        (overblock-run--unit t)))))

;;;###autoload
(defun overblock-run-below (&optional event)
  "Run the region at point and every one below it, or those at EVENT.
They run in order and the pass stops at the first error, or on
`overblock-run-stop'.  The interpreter keeps its state."
  (interactive (list last-input-event))
  (overblock-goto-event event)
  (overblock-run--must)
  (let* ((beg (car (or (overblock-run--call :region-at)
                       (user-error "No %s here" (overblock-run--unit)))))
         (starts (seq-drop-while (lambda (m) (< m beg))
                                 (overblock-run--call :starts))))
    (overblock-run-cells starts (format "%s: running the %s from here down"
                                        (overblock-run--name)
                                        (overblock-run--unit t)))))

;;;###autoload
(defun overblock-run-stop (&optional event)
  "Stop the pass after the region that is running now.
That region runs to its end; `overblock-run-interrupt' is the harder
stop.  EVENT is the click on a stop button, and names the notebook to
act on.  In a buffer that follows a result, this stops the pass of the
shell of the result."
  (interactive (list last-input-event))
  (overblock-goto-event event)
  (let ((queued (length (overblock-run--queued))))
    (overblock-run--queue-set nil)
    ;; A stopped pass does not take point home.
    (overblock-run--home-set nil)
    (message "%s: %s" (overblock-run--name)
             (if (> queued 0)
                 (format "the pass is stopped, %d %s left unrun"
                         queued (overblock-run--unit (> queued 1)))
               "nothing was queued"))))

;;;###autoload
(defun overblock-run-interrupt (&optional event)
  "Interrupt the region the interpreter is running, and stop the pass.
This works in the notebook and in a buffer that follows a result.
There it interrupts only the region that buffer shows: a follower of
an ended result, or of a shell that is gone, says so.  EVENT is the
click on the stop button of a running result, and names the notebook
to act on.

The pass stops too, in R and in Python alike: R answers an interrupt
with only a new prompt, so no output can stop the pass."
  (interactive (list last-input-event))
  (overblock-goto-event event)
  (let ((shell (or (overblock-run-shell)
                   (user-error "No interpreter for this buffer"))))
    (when (local-variable-p 'overblock-run--follower)
      (let ((mine (cdr overblock-run--follower))
            (running (plist-get (buffer-local-value 'overblock-run--state
                                                    shell)
                                :beg)))
        ;; Both must point somewhere: a killed notebook leaves the
        ;; shared marker pointing nowhere, and `=' then signals.
        (unless (and mine running (marker-buffer running)
                     (eq (marker-buffer running) (marker-buffer mine))
                     (= running mine))
          (user-error "The %s this buffer shows is not running"
                      (overblock-run--unit)))))
    (with-current-buffer shell (setq overblock-run--queue nil))
    ;; A stopped pass does not take point home.
    (overblock-run--home-set nil)
    (interrupt-process (or (get-buffer-process shell)
                           ;; `interrupt-process' of nil takes the
                           ;; process of the current buffer.
                           (user-error "The interpreter is gone")))))

(provide 'overblock-run)
;;; overblock-run.el ends here
