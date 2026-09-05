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
;; done, and show what came back under the region it came from.  That
;; loop is the same whatever language is at the other end, and this file
;; is the whole of it: the run state, the queue of a pass over the
;; buffer, the ticker that mirrors a running region five times a second,
;; the filter that waits for the prompt, and the result block itself.
;;
;; Nothing here knows a language.  Two plists say what one is.
;;
;; `overblock-run-backend' is the shell: a buffer-local plist that the
;; notebook's mode sets, and that a send copies into the shell buffer so
;; the filter and the ticker can reach it there.  Its slots:
;;
;;   :name      the word messages carry, as in "NAME: stopped at error"
;;   :process   () -> the live shell process, or nil.  In the notebook
;;   :start     () -> start one; the process where it is ready to take a
;;              region at once, nil where it will only prompt later
;;   :arm       (THUNK) -> run THUNK on the shell's first prompt.  Only
;;              a `:start' that answers nil needs this
;;   :send      (PROC BEG END) -> send the region.  In the notebook
;;   :prompt-p  (TAIL) -> non-nil where TAIL ends at a prompt.  In the shell
;;   :clean     (TEXT) -> TEXT as a block can show it.  In the shell
;;   :head      (FROM) -> as much of the output after FROM as shows;
;;              `overblock-run-output-head' with the caller's own bounds
;;   :show      (BEG END TEXT RUNTIME STATE TOTAL) -> draw the result;
;;              `overblock-run-show' with the caller's own style
;;   :error-p   (TEXT) -> non-nil where the region failed, which stops a pass
;;   :step      () -> run whatever is at point, and answer non-nil where
;;              the walk must wait for a prompt before the next one
;;   :done      (BUFFER TEXT) -> write the whole result into a follower's
;;              buffer once the region has ended.  Optional
;;
;; A style is the look of a result block, and `overblock-run-show',
;; `overblock-run-update' and `overblock-run-header' take one as their
;; first argument.  It is a plain plist, one per package rather than one
;; per buffer, so a caller can draw a block with no mode turned on:
;;
;;   :keymap       the keymap on it
;;   :buttons      the button descriptors, or a function answering them
;;   :fold         the command the fold mark runs
;;   :header-face  the face of the bar
;;   :output-face  the face of the body
;;   :lines        how many lines show, or a function answering that
;;   :chars        how long a line may be, or a function answering that
;;   :stale        what to do with the block when its region is edited,
;;                 `overblock-delete' where the style names none
;;
;; Two consumers live here: `overblock-pycell' sends Python cells to an
;; inferior Python, and `overblock-rmd' sends the R chunks of an Rmd
;; file to an inferior R.  Both keep their own buttons, faces and
;; options; what they share is every line below.

;;; Code:

(require 'overblock)
(require 'overblock-repl)
(require 'seq)
(require 'subr-x)
(require 'map)
(require 'ansi-color)

(defvar-local overblock-run-backend nil
  "What this buffer's shell is, as a plist, or nil for no notebook.
The commentary of this file lists the slots.  The mode of a notebook
sets it, and takes it away again when it is turned off: the runner asks
it whether a buffer is still a notebook it may draw in.

`overblock-run-send' copies it into the shell buffer, where the filter
and the ticker read it.")

(defun overblock-run--call (slot &rest args)
  "Call SLOT of this buffer's backend on ARGS, or answer nil for none."
  (when-let* ((fn (plist-get overblock-run-backend slot)))
    (apply fn args)))

(defun overblock-run--must ()
  "Return this buffer's backend, or say that the buffer is no notebook.
The runner does nothing at all without one, and a command of a notebook
mode is autoloaded and can be called anywhere; without this it did that
nothing in silence."
  (or overblock-run-backend
      (user-error "This buffer runs nothing: it has no notebook mode on")))

(defun overblock-run--name ()
  "Return the word this backend's messages carry."
  (or (plist-get overblock-run-backend :name) "overblock"))

(defun overblock-run--style (style slot)
  "Return SLOT of STYLE, called where it is a function.
For the three slots whose value a caller may want looked up when the
block is drawn rather than when the style was written: an option it
reads, and the buttons its own `:set' replaces.  Every other slot is a
plain `plist-get', because a face, a keymap or a command must not be
called."
  (let ((value (plist-get style slot)))
    (if (functionp value) (funcall value) value)))

(defun overblock-run-shorten (line chars)
  "Return LINE cut to CHARS characters.
The cut is marked with an ellipsis.  A CHARS of nil, or of zero, leaves
the line whole; see the options of the callers for what that costs the
scroller."
  (if (or (not (natnump chars))
          (zerop chars)
          (<= (length line) chars))
      line
    (concat (substring line 0 chars)
            (overblock-glyph "…" "..."))))

(defconst overblock-run-tick 0.2
  "Seconds between two looks at a running region\'s output.
The spinner turns one frame a tick, so `overblock-run-header\' divides
the runtime by this to pick its glyph: the two have to agree, which is
why the interval has a name.")

(defun overblock-run-body-lines (lines max chars)
  "Return the leading LINES that show inline.
At most MAX of them — every one where MAX is zero — each cut to CHARS
characters, and
nothing after the first line that carries an image it can draw: more
inline figures would grow the block, and the scroll jump with it,
without bound.  A display that shows no images has nothing to stop
for, and names them instead.  A line with an image on it is not cut,
since the image may sit past the cut; its images are capped to
`overblock-image-height' instead."
  (let (shown stop)
    ;; A MAX of zero shows every line, as a CHARS of zero leaves every
    ;; line whole: two options of the same shape, and a zero that meant
    ;; "all of it" in one and "none of it" in the other was a trap.
    (while (and lines (not stop) (or (zerop max) (< (length shown) max)))
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
                    (imagep (overblock-run-shorten (overblock-image-label l)
                                                   chars))
                    (t (overblock-run-shorten l chars)))
              shown)
        (when drawp (setq stop t))))
    (nreverse shown)))

(defun overblock-run--mark (style folded total runtime state)
  "Return the mark that stands at the head of a result's bar.
A spinner while the region runs, a warning where the interpreter went
away, a fold arrow where there is something to fold, and a tick where
the region printed nothing at all.  STYLE, FOLDED, TOTAL, RUNTIME and
STATE are `overblock-run-header''s own."
  (cond ((eq state 'running)
         ;; The stopwatch drives the spinner: one frame for each tick.
         ;; Braille and not a codicon like every other mark here: a
         ;; spinner needs a frame for each tick and the set has one
         ;; still glyph.  These ten are one weight and one size among
         ;; themselves, which is what the rest of the row is for.
         (let ((frames (overblock-glyph "⠋⠙⠹⠸⠼⠴⠦⠧⠇⠏" "|/-\\")))
           (string ?\s (aref frames (mod (truncate runtime overblock-run-tick)
                                         (length frames))))))
        ((eq state 'died) (overblock-glyph " " " ⚠" " !"))
        ;; A single line can still be tall: one image is one line, and
        ;; that is the block worth folding.
        ((> total 0)
         (overblock-button (if folded
                               (overblock-glyph " " " ▸" " >")
                             (overblock-glyph " " " ▾" " v"))
                           "Fold or unfold this result"
                           (plist-get style :fold)))
        ;; nothing printed: every other case is above
        (t (overblock-glyph " " " ✓" " ."))))

(defun overblock-run-header (style folded total shown runtime state imagep)
  "Return the header bar of a result, drawn as STYLE says.
FOLDED is non-nil when only the header shows.
TOTAL and SHOWN count the lines and the inline subset.  RUNTIME is the
time in seconds since the cell started.  STATE is `running' while the
cell runs, `died' where the interpreter went away before the cell
ended, and nil where the cell finished.  IMAGEP marks a result with an image."
  (let* ((icons (overblock-buttons (overblock-run--style style :buttons)
                                   imagep total (eq state 'running)))
         (mark (overblock-run--mark style folded total runtime state))
         (label (cond ((> total 0)
                       (format "%d line%s%s" total (if (= total 1) "" "s")
                               (if (< shown total)
                                   (format ", showing %d" shown) "")))
                      ((not state) "no output")))
         (time (format "%.1fs" runtime)))
    (overblock-bar
     (concat mark " " (string-join (delq nil (list label time)) " · "))
     icons (or (plist-get style :header-face) 'default))))

(defun overblock-run-clear-results ()
  "Take the results of this buffer down, and sweep what lost its anchor.
Whatever else is rendered stays — the prose of an Rmd file, the markdown
cells of a notebook.  A clear that names a kind cannot sweep an orphan,
because an orphan says nothing about the kind it belonged to, so the
sweep is asked for by name here: taking the results down with
`overblock-clear\' alone left the cloak of a lost block keeping lines of
the buffer invisible, with nothing able to remove it."
  (overblock-clear nil nil 'result)
  (overblock-sweep-orphans))

(defun overblock-run-fold (style block)
  "Fold BLOCK where it is unfolded and unfold it where it is folded.
Drawn again as STYLE says.  This is the body of the toggle command each
package binds to its own key and its own fold mark."
  (let ((data (overblock-get block :data)))
    (overblock-set block :data
                   (plist-put data :folded (not (plist-get data :folded))))
    (overblock-run-update style block)))

(defun overblock-run-update (style block)
  "Make the header and the body of the result BLOCK again, and show them.
STYLE is the look it is drawn with.
The lines are counted once for both: the header says how many there
are and how many of them show, and the body is those that show."
  (let* ((data (overblock-get block :data))
         (folded (plist-get data :folded))
         (text (plist-get data :text))
         (total (plist-get data :total)))
    (let* ((empty (string-empty-p text))
           (max (overblock-run--style style :lines))
           (chars (overblock-run--style style :chars))
           (lines (unless empty (overblock-repl-first-lines text max)))
           (shown (overblock-run-body-lines lines max chars))
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
                     (overblock-run-header style folded count (length shown)
                                           (plist-get data :runtime)
                                           (plist-get data :state)
                                           (and lines
                                                (overblock-image-in text))))
      (overblock-set block :body
                     (when (and shown (not folded))
                       (overblock-faced
                        (string-join shown "\n")
                        (or (plist-get style :output-face) 'default))))
      (overblock-refresh block))))

(defun overblock-run-show (style beg end text runtime &optional state total)
  "Show TEXT as the result of the region BEG..END, drawn as STYLE says.
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
               (overblock-run-update style old)
               old)
      ;; The newline that ends the cell carries the result; give the
      ;; last cell of the buffer one.  The whole buffer: under a
      ;; narrowing `point-max' is the end of the accessible part, and
      ;; the newline went into the middle of the buffer — measured, it
      ;; cut a `print(2)' in two.
      ;;
      ;; A buffer that refuses the write keeps its text, and the block
      ;; hangs on its anchor instead: a notebook opened through
      ;; `view-file' or from a read-only checkout answered
      ;; `buffer-read-only' here, inside the process filter, and that
      ;; signal took the rest of the filter with it — the cell was never
      ;; ended, the shell stayed busy for the session, and a run-all
      ;; stopped where it was with its queue still armed.
      (without-restriction
        (when (and (= end (point-max)) (not (eq (char-before end) ?\n)))
          (ignore-error buffer-read-only
            (save-excursion (goto-char end) (insert "\n")))))
      (let ((block (overblock-show beg end
                                   ;; `result' and not a slot of the
                                   ;; style: the block a new result
                                   ;; replaces is looked for by that
                                   ;; kind above, so a style that named
                                   ;; another would have stopped
                                   ;; replacing its own results.
                                   :kind 'result
                                   :data data
                                   :keymap (plist-get style :keymap))))
        ;; An empty cell — a boundary line directly followed by the
        ;; next — has no newline of its own to hang a block on, and
        ;; `overblock-show' answers nil rather than anchor a
        ;; zero-length overlay that would evaporate.  The cell was
        ;; evaluated either way, and the caller that counts results
        ;; takes the nil; what it must not do is crash inside the
        ;; process filter, where the signal left the shell busy for
        ;; the session and the queue wedged.
        (when block
          ;; An edit of the region makes the result stale; it goes.
          (funcall (or (plist-get style :stale)
                       #'overblock-stale-when-edited)
                   block)
          (overblock-run-update style block))
        block))))

(defvar-local overblock-run--queue nil
  "Start markers of the regions a pass over the whole notebook has left.
`overblock-run-cells' fills it and `overblock-run-next' empties it as it
goes.  Kept in the shell's buffer, beside `overblock-run--state': a
notebook with a shell of its own has a queue of its own.  One global
list let a pass in one notebook discard another's regions and then feed
its own down that notebook's interpreter.  `overblock-run-shell' is how
to reach it.")

(defun overblock-run-shell ()
  "Return the buffer that holds the queue and the run state for this one.
That is the shell: this buffer where it is one, and the shell this
notebook sends to otherwise.  Nil where there is no shell, and then
there is nothing queued either.

The backend goes with it.  Everything the runner does in the shell —
the filter, the ticker, the walk down the queue armed on the first
prompt — reads the backend there, and only a notebook has one of its
own; a shell that had not been sent to yet answered as if it had no
queue at all, so a pass over a notebook whose interpreter was still
starting never began."
  (when-let* ((proc (overblock-run--call :process))
              (shell (process-buffer proc)))
    (unless (buffer-local-value 'overblock-run-backend shell)
      (let ((backend overblock-run-backend))
        (with-current-buffer shell
          (setq-local overblock-run-backend backend))))
    shell))

(defvar-local overblock-run--home nil
  "Where point goes in the notebook when this shell's queue runs out.
`overblock-run-next' walks point down the notebook, which is what makes
a pass over the whole buffer visible.  A pass asked for from one region
gives point back instead: the reader pressed a button there.")

(defun overblock-run-queued ()
  "Return the regions a pass still has to run, in order."
  (when-let* ((shell (overblock-run-shell)))
    (buffer-local-value 'overblock-run--queue shell)))

(defun overblock-run-go-home ()
  "Put point back where the pass that has just ended was asked for.
The windows showing the notebook go there too: a window keeps a point
of its own while its buffer is not the selected one, and a pass ended
while the reader looked elsewhere left that window at whatever line it
had been scrolled to."
  (when-let* ((shell (overblock-run-shell))
              (home (buffer-local-value 'overblock-run--home shell)))
    ;; The marker goes whatever happens next, so a notebook that was
    ;; killed while its pass ran leaves nothing behind to act on.  Freed
    ;; and not merely dropped: a marker stays in its buffer's chain
    ;; until it is set to nowhere, and comint adjusts that whole chain
    ;; on every insertion.
    (with-current-buffer shell (setq overblock-run--home nil))
    (when (buffer-live-p (marker-buffer home))
      (with-current-buffer (marker-buffer home)
        (goto-char home)
        (dolist (window (get-buffer-window-list nil nil t))
          (set-window-point window home))))
    (set-marker home nil)))

(defun overblock-run-home-set (marker)
  "Give the shell MARKER as the place its pass came from, or nil for none."
  (when-let* ((shell (overblock-run-shell)))
    (with-current-buffer shell (setq overblock-run--home marker))))

(defun overblock-run-queue-set (cells)
  "Give the shell CELLS to run, and answer them."
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

The last two belong to the live mirror.")

(defun overblock-run--whole-escapes (text)
  "Return TEXT without an escape sequence that has not arrived in full.
comint-mime sends an image as one escape sequence, and half of one
swallows everything after it until the rest comes.

A match anchored at the end of the string is a truncation, so this cuts
rather than replaces: `replace-regexp-in-string' copies its argument
twice whether it matches or not, and over a hundred kilobytes of
propertized text a hundred passes measured 0.210 seconds against 0.102
for the `substring' here."
  (if (string-match "\e\\][^\e]*\\'" text)
      (substring text 0 (match-beginning 0))
    text))

(defun overblock-run--output-so-far (from)
  "Return the running cell's output after FROM, cleaned.
An incomplete escape sequence at the end is dropped: comint-mime
renders it only when it is complete."
  (overblock-run--call :clean
                       (overblock-run--whole-escapes
                        (buffer-substring from (point-max)))))

(defun overblock-run-output-head (from lines chars clean)
  "Return as much of the output after FROM as the block can show.
CLEAN takes the prompts off what is read, and CHARS is how long a line
of it may be.
`overblock-run-body-lines' takes the first LINES lines and stops, so a
tick has no reason to read — or clean — everything the
region has printed.  Once those lines are all in, the text cannot
change anymore and is kept, and the ticks after that read nothing.

A cell that prints much on few lines never reaches that line, so the
read is bounded in characters as well; the comment below says why that
bound holds only where no escape sequence begins inside it.
Nothing is kept while the head is empty: an escape sequence that has
not arrived in full swallows everything after it until it does, and a
cell whose first lines are still on their way has more to come."
  (or (plist-get overblock-run--state :head)
      (let* ((budget (and (natnump chars)
                          (> chars 0)
                          ;; what `overblock-run-body-lines' can show, and no
                          ;; more: the lines it keeps, each cut to the
                          ;; length it cuts them to
                          (* lines (1+ chars))))
             (limit (save-excursion
                      (goto-char from)
                      (forward-line (+ lines 4))
                      (point)))
             ;; A cell that prints much on few lines never reaches that
             ;; line, so its text is never kept and every tick reads and
             ;; cleans everything printed so far: measured, 68
             ;; milliseconds a tick over a hundred thousand characters on
             ;; one line, five times a second, for the two thousand
             ;; characters that show.  The body cuts each line to
             ;; CHARS anyway, so a bound in characters
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
             (text (funcall clean
                            (overblock-run--whole-escapes
                             (buffer-substring from limit)))))
        (when (and (< limit (point-max))
                   (not (string-empty-p text)))
          (setq overblock-run--state (plist-put overblock-run--state :head text)))
        text)))

(defun overblock-run-total (from)
  "Return the number of lines the running cell has printed after FROM.
Counted where they arrive: reading the whole output again is a pass
over everything printed so far, and a cell that prints a lot pays
that pass five times a second.  Leading blank lines go, as
the backend's `:clean' drops them, so the count agrees with the one the
finished cell shows."
  (let* ((state (or (plist-get overblock-run--state :count)
                    (cons (save-excursion
                            (goto-char from)
                            (skip-chars-forward " \t\n")
                            (point-marker))
                          0)))
         (count (cdr state)))
    (save-excursion
      ;; `count-lines' between two beginnings of lines counts the
      ;; newlines between them, and it counts them in C: measured over
      ;; 60000 lines, twenty passes cost 0.014 seconds against 0.596 for
      ;; a `search-forward' loop.  The line that has arrived only in
      ;; part is counted by the caller below, as it always was.
      (goto-char (point-max))
      (let ((bol (pos-bol)))
        (setq count (+ count (count-lines (car state) bol)))
        (goto-char bol))
      ;; The marker is moved rather than made again.  Every marker left
      ;; behind stays in the buffer's chain until a garbage collection,
      ;; and comint adjusts the whole chain on every insertion: 2000
      ;; ticks over 60000 inserted lines measured 0.144 seconds with a
      ;; fresh marker each time and 0.036 with this one.
      (setq overblock-run--state
            (plist-put overblock-run--state :count
                       (cons (set-marker (car state) (point)) count))))
    ;; A line that has not ended yet is a line all the same.
    (if (and (> (point-max) (marker-position from))
             (not (eq (char-before (point-max)) ?\n)))
        (1+ count)
      count)))

(defun overblock-run-show-in-notebook (beg fin text seconds state &optional total)
  "Show TEXT as the result of the region BEG..FIN, where it can be shown.
The notebook's own backend draws it, through its `:show'.

Nothing where the notebook is gone, and nothing where its mode is off
in it: the mode's own body takes the blocks and the bars away, and a
block put back after that would sit in a buffer with no bars and none
of the mode's hooks, where no key of the mode could fold it again.  A
mode that is off has no backend either, which is how that is asked."
  (when (buffer-live-p (marker-buffer beg))
    (with-current-buffer (marker-buffer beg)
      (overblock-run--call :show beg fin text seconds state total))))

(defun overblock-run--release (&rest markers)
  "Point every marker of MARKERS nowhere, and ignore what is not one.
A marker of a buffer stays in its chain until a garbage collection, and
comint adjusts the whole chain on every insertion: measured over 60000
inserted lines, 0.144 seconds against 0.036."
  (dolist (marker markers)
    (when (markerp marker) (set-marker marker nil))))
(defun overblock-run--end (text &optional died)
  "End the running region and show TEXT as its final result.
The one exit for every way a run ends; DIED marks abnormal ends.
Call this in the shell buffer.

Nothing happens where no cell is running: a failing send can end its
cell through the filter and then signal, and the handler would call this
a second time — `cancel-timer' of nil raised, which masked the error it
was reporting.  `overblock-run-abort' asks the same question."
  (when overblock-run--state
    (pcase-let (((map (:from from) :beg (:end fin) :start :timer :follow
                      (:count count))
                 overblock-run--state))
      ;; The last of the output, and then the whole of it cleaned: the
      ;; tail a follower wrote is raw, and its final lines arrive with
      ;; the closing prompt.
      (overblock-run-follow-tick)
      (setq overblock-run--state nil)
      (cancel-timer timer)
      ;; The pass is over, and so is the place it came from: a marker
      ;; left behind would take point there at the end of the next
      ;; single cell to run.
      (when died (setq overblock-run--queue nil overblock-run--home nil))
      (overblock-run-show-in-notebook beg fin text (- (float-time) start)
                                      (and died 'died))
      (when-let* ((buffer (car-safe follow))
                  ((buffer-live-p buffer)))
        (overblock-run--call :done buffer text))
      ;; The markers of the run go: three of them live in the
      ;; shell, and a pass over a notebook of 200 regions left hundreds
      ;; of them there.
      (overblock-run--release from beg fin (car-safe count) (cdr-safe follow))
      ;; Keep a pass over the whole notebook going, or stop on error.
      ;; Either way the end of a pass takes point home: the last region
      ;; of a pass is sent with the queue already empty, so waiting for
      ;; `overblock-run-next' to find nothing left never happened and
      ;; point stayed on whatever region ran last.
      (cond ((null overblock-run--queue) (overblock-run-go-home))
            ((overblock-run--call :error-p text)
             (setq overblock-run--queue nil)
             (message "%s: stopped at error" (overblock-run--name))
             (overblock-run-go-home))
            (t (overblock-run-next))))))

(defun overblock-run-abort (&optional reason)
  "End the running cell abnormally — its prompt will never return.
A death notice, with the exit status when one is available, follows
the output received so far.  This covers a dead interpreter (the
ticker finds it), a killed shell buffer and a shell restart, which
reinitializes the major mode — hence also on `kill-buffer-hook' and
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

The shell is where the output lands, so the marker that says how much
of it has been copied lives there, in the record of the run."
  (when-let* ((shell (overblock-run-shell)))
    (with-current-buffer shell
      (when overblock-run--state
        (setq overblock-run--state
              (plist-put overblock-run--state :follow
                         (cons buffer
                               (copy-marker (plist-get overblock-run--state :from)))))
        ;; What the cell has printed already, rather than an empty
        ;; buffer until the next tick.
        (overblock-run-follow-tick)))))

(defun overblock-run-follow-tick ()
  "Copy what the region has printed since the last look into its buffer.
Call this in the shell buffer.

Only what is new: the whole output is what the block's own head is
bounded away from reading five times a second, and a cell that prints a
hundred thousand characters would cost that on every tick here as well.

Point at the end of the buffer follows the output, in the buffer and in
every window showing it; point anywhere else stays where the reader put
it."
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
  "Mirror the running region's output and stopwatch into its overlay.
TIMER runs this every `overblock-run-tick' seconds for the shell BUF.
It cancels itself when nothing runs there anymore."
  (if (not (and (buffer-live-p buf)
                (buffer-local-value 'overblock-run--state buf)))
      (cancel-timer timer)
    (with-current-buffer buf
      (if (not (process-live-p (get-buffer-process buf)))
          (overblock-run-abort)
        (pcase-let (((map (:from from) :beg (:end fin) :start) overblock-run--state))
          (let* ((text (or (overblock-run--call :head from) ""))
                 (total (if (string-empty-p text) 0 (overblock-run-total from))))
            (overblock-run-follow-tick)
            (overblock-run-show-in-notebook beg fin text (- (float-time) start)
                                            'running total)))))))

(defun overblock-run--filter (output)
  "Watch OUTPUT for the closing prompt, then end the running region.
The filter stays on `comint-output-filter-functions' and idles while
nothing runs; the live mirroring is the ticker's job."
  (when overblock-run--state
    ;; A chunk boundary can split the prompt, so match a capped tail;
    ;; `ansi-color-filter-apply' drops the escape sequences.
    (let ((tail (concat (plist-get overblock-run--state :tail)
                        (ansi-color-filter-apply output))))
      (setq overblock-run--state
            (plist-put overblock-run--state :tail
                       (string-limit tail 256 t)))
      (when (overblock-run--call :prompt-p tail)
        ;; Copy to the end of the buffer and let the backend's `:clean'
        ;; take the prompt off.  `comint-last-prompt' cannot serve as the
        ;; end: comint calls the last line without a newline a prompt,
        ;; so a chunk that arrives split leaves the marker inside the
        ;; output, and everything after it would be dropped without a
        ;; word.
        (overblock-run--end
         (overblock-run--call
          :clean
          (buffer-substring (plist-get overblock-run--state :from)
                            (point-max))))))))

(defun overblock-run-send (proc start end)
  "Send START..END to PROC as the running region and track it.
Call this with the notebook current, where its backend is: the backend
travels into the shell buffer from here, because the filter and the
ticker run there and read it."
  (let ((beg (copy-marker start))
        (fin (copy-marker end t))
        (backend (overblock-run--must)))
    (with-current-buffer (process-buffer proc)
      (when overblock-run--state
        (user-error "The %s shell is still busy" (overblock-run--name)))
      (setq-local overblock-run-backend backend)
      ;; All idempotent: the filter idles while no cell runs, the
      ;; other two catch the shell going away under a running cell.
      ;; comint-mime renders from the same hook; because our filter
      ;; appends, the copied region already carries the images.
      (add-hook 'comint-output-filter-functions #'overblock-run--filter t t)
      (add-hook 'kill-buffer-hook #'overblock-run-abort nil t)
      (add-hook 'change-major-mode-hook #'overblock-run-abort nil t)
      ;; The ticker receives itself, so it can always self-cancel: the
      ;; variable is bound before the timer is made and set from the
      ;; call that makes it, so the closure has it by the first tick.
      (let (timer)
        (setq timer (run-with-timer
                     overblock-run-tick overblock-run-tick
                     (let ((buffer (current-buffer)))
                       (lambda () (overblock-run--tick buffer timer)))))
        ;; The process mark, and not the end of the buffer.  A render
        ;; comint-mime finishes after the closing prompt sits past the
        ;; mark, and a cell that started from the end of the buffer
        ;; would have had its own output — which comint inserts AT the
        ;; mark, before that render — fall outside its own region.  So
        ;; a late render is still swept into the next cell's result;
        ;; that is a fault of its own and not one to cure here.
        (setq overblock-run--state (list :from (copy-marker (process-mark proc))
                                         :beg beg :end fin :tail ""
                                         :start (float-time) :timer timer
                                         :head nil :count nil))))
    (overblock-run--call :show beg fin "" 0.0 'running nil)
    ;; The bookkeeping above says a region is running, and the send below
    ;; can fail — a signal from the shell, or `C-g' while the region is
    ;; written to its temporary file.  Without this the shell stays busy
    ;; for the rest of the session: the ticker counts up, and every later
    ;; cell is refused.  So a failed send ends the cell as a death, which
    ;; also empties the queue of a run-all.
    (condition-case error
        (overblock-run--call :send proc beg fin)
      ((error quit)
       (with-current-buffer (process-buffer proc)
         (overblock-run--end (propertize (error-message-string error) 'face 'error)
                             t))
       (signal (car error) (cdr error))))))

(defun overblock-run-next ()
  "Run the regions of the shell's queue until one has to wait.
Point follows, so a pass over the whole notebook is visible.  Called
from the shell on its first prompt and from `overblock-run--end' when a
region finishes, so the queue is reached through `overblock-run-shell'
either way.

The backend's `:step' runs whatever point now stands on, and says
whether the walk must wait: a region sent to the shell has to, and one
the notebook answered itself — a markdown cell it rendered — does not,
so the walk goes straight on to the next.

A loop and not a call back into the command that runs one region: that
built a frame for every markdown cell in a row, a hundred of them
reached `max-lisp-eval-depth', and — worse — every frame ran its own
tail on the way out, so the second one sent a code cell while the first
was still running.  `overblock-run-send' refused it from inside the
process filter and that cell, already off the queue, never ran at all."
  (catch 'waiting
    (while t
      (let* ((cells (overblock-run-queued))
             (m (car cells)))
        (unless m
          (overblock-run-go-home)
          (throw 'waiting nil))
        (overblock-run-queue-set (cdr cells))
        (unless (buffer-live-p (marker-buffer m))
          (overblock-run-queue-set nil)
          (throw 'waiting nil))
        (with-current-buffer (marker-buffer m)
          (goto-char m)
          ;; The region goes to the top of every window showing the
          ;; notebook, so the whole of the code that is about to run
          ;; is visible.  `overblock-run-go-home' gives point back when the
          ;; pass ends.
          (dolist (window (get-buffer-window-list nil nil t))
            (set-window-point window m)
            (set-window-start window m))
          (when (overblock-run--call :step)
            (throw 'waiting nil)))))))

(defun overblock-run-on-prompt (cells message)
  "Arm CELLS to run on the shell's first prompt, and say MESSAGE.
For a shell that has not prompted yet: one just started, or one just
restarted.  The pass may only start once the fresh interpreter has
prompted, and the backend's `:arm' is what knows when that is.

The queue is armed after the hook and the shell, not before: the home
belongs to the shell's buffer, and the first pass of a session had
none to put it in, so that pass never brought point back.  A shell that
answers with an error here leaves nothing armed."
  (overblock-run--call :arm #'overblock-run-next)
  (overblock-run-home-set (point-marker))
  (overblock-run-queue-set cells)
  (message "%s" message))

(defun overblock-run--pass (cells message)
  "Put CELLS on the shell's queue and start the pass, saying MESSAGE."
  (overblock-run-home-set (point-marker))
  (overblock-run-queue-set cells)
  (condition-case err
      (overblock-run-next)
    ;; A region that will not start takes the home with it, or the
    ;; marker of a pass that never ran drags point when the region that
    ;; refused it ends.
    (error (overblock-run-queue-set nil)
           (overblock-run-home-set nil)
           (signal (car err) (cdr err))))
  (message "%s" message))

(defun overblock-run-cells (cells message)
  "Run CELLS in order, and say MESSAGE while they run.
Each region goes on the prompt of the one before it, so the queue is
left with the shell and `overblock-run-next' takes the next one off it.
The interpreter starts where there is none: one that is ready at once
begins the pass now, and one that will only prompt later begins it
then.

A region that will not start takes the whole pass with it, which is why
`overblock-run--pass' empties the queue on a signal: the queue was left
armed by a refusal, and the regions ran later without being asked for,
less the one the refusal had already taken off it."
  (overblock-run--must)
  (if (or (overblock-run--call :process) (overblock-run--call :start))
      (overblock-run--pass cells message)
    (message "%s: starting the interpreter…" (overblock-run--name))
    (overblock-run-on-prompt cells message)))

(defun overblock-run-region (start end)
  "Run START..END, starting the interpreter where there is none.
A region sent while another one runs is refused, with a `user-error'
from `overblock-run-send'.  Where the interpreter is only starting, the
region waits for its first prompt and is sent then."
  (overblock-run--must)
  (if-let* ((proc (overblock-run--call :process)))
      (overblock-run-send proc start end)
    ;; Mark the region here, while its buffer is still current:
    ;; `copy-marker' on a number answers for whatever buffer that is,
    ;; and the thunk below is called in the shell's.  Markers into the
    ;; shell would send its start-up banner as the region.
    (let ((beg (copy-marker start))
          (fin (copy-marker end t)))
      (if-let* ((proc (overblock-run--call :start)))
          (overblock-run-send proc beg fin)
        (overblock-run--call :arm (overblock-run--sender beg fin))
        (message "%s: starting the interpreter…" (overblock-run--name))))))

(defun overblock-run--sender (beg fin)
  "Return a thunk that sends BEG..FIN once the interpreter has prompted.
Unlike `overblock-run-next' this does not move point: the command that
caused the cold start may have moved it on already."
  (lambda ()
    (when (buffer-live-p (marker-buffer beg))
      (with-current-buffer (marker-buffer beg)
        (when-let* ((proc (overblock-run--call :process)))
          (overblock-run-send proc beg fin))))))

(provide 'overblock-run)
;;; overblock-run.el ends here
