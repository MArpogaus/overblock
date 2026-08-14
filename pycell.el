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
;; `pycell-mode' turns on with `code-cells-mode' in Python buffers.
;; Evaluating a cell sends it to the inferior Python process as usual,
;; so the REPL keeps the full log.  While the cell runs, the result
;; grows below it: a header bar with a spinner, a stopwatch and buttons,
;; and the output as comint-mime rendered it, images included.
;;
;; Markdown cells, the `# %% [markdown]' ones that jupytext writes, are
;; rendered in place.  Their source turns invisible, an external
;; markdown command and shr produce the block, and the formulas that
;; the converter passed through become preview images through the
;; formula machinery of Org mode.
;;
;; Rich output needs an IPython REPL, because comint-mime installs its
;; renderers there; a plain python3 shell yields text only.
;;
;; Blocks are display strings on a single buffer line, and Emacs cannot
;; place point inside one.  The mouse wheel scrolls through a block a
;; pixel at a time, but `next-line' and `previous-line' cross it in one
;; step, because a window can only start at a buffer position.

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

(defgroup pycell nil "Inline results for Python code cells." :group 'python)

(defface pycell-header '((t :inherit code-cells-header-line))
  "Face for the header bar above a result.
It inherits the cell boundary face, so results match the cells.")

(defface pycell-output '((t :inherit shadow :extend t))
  "Face for the body of a result.")

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
Large display blocks make redisplay and scrolling slow.  Use
`pycell-pop-output' to see the full result."
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
  "Return the first of CANDIDATES that this frame has a glyph for.
The last candidate is the answer when none of them has one.
`char-displayable-p' answers for the character set and not for the
font, so it says yes to characters that then draw as a hex box."
  (or (and (display-graphic-p)
           (seq-find (lambda (c) (internal-char-font nil (aref c 0)))
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
`string-width' counts, and (N) in the display spec means N pixels."
  (pycell--faced
   (concat left
           (propertize " " 'display
                       `(space :align-to
                               (- right (,(string-pixel-width
                                           (propertize icons 'face
                                                       'pycell-header))))))
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
nest, a display string would swallow the images."
  (let ((image (and body (text-property-not-all 0 (length body)
                                                'display nil body)))
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
A click also selects its window and moves point there."
  (when-let* (((consp event))
              (posn (event-start event))
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
    ;; matches the empty string.
    (while (and (string-match (concat "\\`[ \t\n]*" rx) text)
                (> (match-end 0) 0))
      (setq text (substring text (match-end 0))))
    (while (string-match (concat "\n[ \t]*" rx "[ \t\n]*\\'") text)
      (setq text (substring text 0 (match-beginning 0)))))
  (setq text (replace-regexp-in-string "^Out\\[[0-9]+\\]: " "" text))
  (let* ((beg 0)
         (end (length text))
         (blank (lambda (i) (and (memq (aref text i) '(?\s ?\t ?\n ?\r))
                                 (not (get-text-property i 'display text))))))
    (while (and (< beg end) (funcall blank beg)) (setq beg (1+ beg)))
    (while (and (< beg end) (funcall blank (1- end))) (setq end (1- end)))
    (substring text beg end)))

(defun pycell--body-lines (lines)
  "Return the leading LINES that show inline.
At most `pycell-max-lines', and nothing after the first line that
carries an image: more inline figures would grow the block, and the
scroll jump with it, without bound."
  (let (shown stop)
    (while (and lines (not stop) (< (length shown) pycell-max-lines))
      (let ((l (pop lines)))
        (push l shown)
        (when (text-property-not-all 0 (length l) 'display nil l)
          (setq stop t))))
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
                   (pycell--button "↗" "Show this result in its own buffer"
                                      #'pycell-pop-output))
                 (pycell--button "✕" "Discard this result"
                                    #'pycell-discard-output)))
         ;; The stopwatch drives the spinner: one frame for each tick.
         (state (cond ((eq running t)
                       (string ?\s (aref "⠋⠙⠹⠸⠼⠴⠦⠧⠇⠏"
                                         (mod (truncate runtime 0.2) 10))))
                      ((eq running 'died) " ⚠")
                      ;; A single line can still be tall: one image is
                      ;; one line, and that is the block worth folding.
                      ((> total 0)
                       (pycell--button (if folded
                                           (pycell--glyph " ▸" " ▶" " >")
                                         (pycell--glyph " ▾" " ▼" " v"))
                                          "Fold or unfold this result"
                                          #'pycell-toggle-output))
                      ((zerop total) " ✓")
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
  (pcase-let ((`(,folded ,text ,runtime ,running) (overlay-get ov 'pycell)))
    (let* ((lines (unless (string-empty-p text) (split-string text "\n")))
           (shown (pycell--body-lines lines)))
      (pycell--attach
       ov
       (concat (unless (eq (char-before (overlay-end ov)) ?\n) "\n")
               (pycell--header folded (length lines) (length shown)
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

(defun pycell--show (beg end text runtime &optional running)
  "Show TEXT as the result of the cell BEG..END.
RUNTIME is the time in seconds since the cell started.  RUNNING is
non-nil while the cell runs.  Empty TEXT gets a header that says
\"no output\", so the cell is recognizable as evaluated.  The fold
state of a replaced result is kept."
  (let ((folded (when-let* ((old (car (pycell--overlays beg end))))
                  (car (overlay-get old 'pycell)))))
    (pycell-remove-overlays beg end)
    ;; The newline that ends the cell carries the result; give the
    ;; last cell of the buffer one.
    (when (and (= end (point-max)) (not (eq (char-before end) ?\n)))
      (save-excursion (goto-char end) (insert "\n")))
    (let ((ov (pycell--make-overlay beg end)))
      (overlay-put ov 'pycell (list folded text runtime running))
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

(defun pycell-copy-output (&optional event)
  "Copy the result at point, or the one clicked in EVENT.
The copy keeps its text properties, so images survive a yank."
  (interactive (list last-input-event))
  (kill-new (cadr (overlay-get (pycell--overlay event) 'pycell)))
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
  (let* ((text (cadr (overlay-get (pycell--overlay event) 'pycell)))
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
         (text (cadr (overlay-get ov 'pycell)))
         (name (format "*pycell: %s:%d*" (buffer-name)
                       (line-number-at-pos (overlay-start ov)))))
    (with-current-buffer (get-buffer-create name)
      (special-mode)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert text))
      (goto-char (point-min))
      (pop-to-buffer (current-buffer)))))

;;;; Markdown cells

(defun pycell--md-head (pos)
  "Return the start of the =# %% [markdown]= line above POS, or nil.
A non-nil value marks POS as the body of a markdown cell."
  (save-excursion
    (goto-char pos)
    (forward-line -1)
    (and (looking-at-p "# %% \\[markdown\\]") (point))))

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
    (when-let* ((bov (overlay-get ov 'pycell-body)))
      (if flag
          (unless (overlay-get ov 'pycell-folded)
            (overlay-put ov 'pycell-folded
                         (list (overlay-get ov 'after-string)
                               (overlay-get bov 'display)
                               (overlay-get bov 'after-string)))
            (overlay-put ov 'after-string nil)
            (overlay-put bov 'display nil)
            (overlay-put bov 'after-string nil))
        (when-let* ((saved (overlay-get ov 'pycell-folded)))
          (overlay-put ov 'after-string (nth 0 saved))
          (overlay-put bov 'display (nth 1 saved))
          (overlay-put bov 'after-string (nth 2 saved))
          (overlay-put ov 'pycell-folded nil))))))

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
fragment that fails to render here stays plain."
  (replace-regexp-in-string
   pycell--md-math-regexp
   (lambda (frag)
     ;; `replace-regexp-in-string' uses the match data after the
     ;; replacement function returns; rendering must not touch it.
     (save-match-data
       (if-let* ((img (pycell--md-latex-image frag)))
           (propertize frag 'display img)
         frag)))
   text t t))

(defun pycell--md-program ()
  "Return the markdown converter as a list of program and arguments.
The first candidate of `pycell-markdown-command' that is installed
wins; the result is nil when none of them is."
  (seq-some (lambda (command)
              (let ((argv (split-string-shell-command command)))
                (and (executable-find (car argv)) argv)))
            (ensure-list pycell-markdown-command)))

(defun pycell--md-rendered (md)
  "Render the markdown MD to a propertized string.
`pycell-markdown-command' produces HTML, shr renders it, and LaTeX
fragments become preview images."
  (require 'shr)
  (let* ((program (pycell--md-program))
         (dom (with-temp-buffer
                (insert md)
                ;; Send standard error nowhere: pandoc warns about math
                ;; it cannot convert, and the text would land in the HTML.
                (let ((status (apply #'call-process-region
                                     (point-min) (point-max) (car program)
                                     t '(t nil) nil (cdr program))))
                  (unless (eq status 0)
                    (error "%s exited with status %s" (car program) status)))
                (libxml-parse-html-region (point-min) (point-max)))))
    (with-temp-buffer
      (shr-insert-document dom)
      (pycell--md-mathify (string-trim (buffer-string))))))

(defvar-keymap pycell-md-map
  :doc "Keymap on rendered markdown cells."
  "RET" #'pycell-md-edit
  "<mouse-2>" #'pycell-md-edit
  "<mouse-1>" #'pycell-md-raw)

(defun pycell--md-show (beg end)
  "Show the markdown cell body BEG..END rendered, in place.
The block is built exactly like a result block — the overlay just
grows by one character, the boundary line's newline, and hides its
text.  The invisible run must start at the end of a visible line:
`scroll-down' fails with a beginning-of-buffer error when it has to
move the window start over a run that begins at a line start, which
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
                    (buffer-substring-no-properties beg end)))
                  'default)
                 'keymap pycell-md-map)
                'help-echo help))
         (head (pycell--bar
                "markdown"
                (pycell--icons
                 (pycell--button
                  "↗" "Edit this markdown cell in its own buffer"
                  #'pycell-md-edit)
                 (pycell--button "✕" "Show the plain source"
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
    (overlay-put ov 'invisible t)
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
    (pycell--attach ov "" text)))

;;;###autoload
(defun pycell-md-render-all ()
  "Render every markdown cell in the buffer.
A markdown cell is one whose boundary line reads \"# %% [markdown]\"."
  (interactive)
  (let ((program (pycell--md-program))
        missed)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "^# %% \\[markdown\\]" nil t)
        (forward-line 1)
        (let ((beg (point))
              (end (if (re-search-forward code-cells-boundary-regexp nil t)
                       (pos-bol)
                     (point-max))))
          (when (< beg end)
            (if program (pycell--md-show beg end) (setq missed t)))
          (goto-char end))))
    (when missed
      (message "pycell: no markdown converter found (%s), cells stay plain"
               (string-join (ensure-list pycell-markdown-command) ", ")))))

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
               (buf (get-buffer-create
                     (format "*pycell md: %s*" (buffer-name)))))
    (with-current-buffer buf
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
        (goto-char beg)
        (delete-region beg end)
        (insert (pycell--md-comment md) "\n"))
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
A list (FROM BEG END TAIL START TIMER): FROM marks where the output
starts here, BEG and END delimit the cell in its own buffer, TAIL
accumulates recent output for the prompt detection, START is the
`float-time' of the send and TIMER the ticker.")

(defvar-local pycell--cold-cell nil
  "Cell (BEG . END markers) that waits for this shell's first prompt.")

(defun pycell--output-so-far (from)
  "Return the running cell's output after FROM, cleaned.
An incomplete escape sequence at the end is dropped: comint-mime
renders it only when it is complete."
  (pycell--clean
   (replace-regexp-in-string
    "\e\\][^\e]*\\'" "" (buffer-substring from (point-max)))))

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
          (let ((text (pycell--output-so-far from)))
            (when (buffer-live-p (marker-buffer beg))
              (with-current-buffer (marker-buffer beg)
                (pycell--show beg fin text (- (float-time) start)
                                 t)))))))))

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
        (pycell--end
         (pycell--clean
          (buffer-substring (car pycell--run)
                            (if comint-last-prompt
                                (car comint-last-prompt)
                              (point-max)))))))))

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
                                   beg fin "" (float-time) timer))))
    (pycell--show beg fin "" 0.0 t)
    ;; `python-shell-send-region' pads the code, so traceback line
    ;; numbers match the buffer.
    (python-shell-send-region beg fin)))

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
      (run-python)
      (with-current-buffer
          (process-buffer (python-shell-get-process-or-error))
        (setq pycell--cold-cell (cons (copy-marker start)
                                         (copy-marker end t)))
        (add-hook 'python-shell-first-prompt-hook
                  #'pycell--run-cold 90 t))
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
  "Enable `pycell-mode' in Python cell buffers."
  (when (derived-mode-p 'python-base-mode)
    (pycell-mode)))

;; Keyed on the minor mode: with it off, code-cells falls through to its
;; stock python entry, `python-shell-send-region'.
(setf (alist-get 'pycell-mode code-cells-eval-region-commands)
      #'pycell-eval-region)

;;;###autoload
(add-hook 'code-cells-mode-hook #'pycell-mode-maybe)

(provide 'pycell)
;;; pycell.el ends here
