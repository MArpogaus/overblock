;;; record.el --- record the demonstrations of the overblock family -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Marcel Arpogaus

;; This file is not part of GNU Emacs.

;;; Commentary:

;; The animations in the README, recorded from a real Emacs session:
;;
;;     OB_GIF=md emacs -Q -l demo/init.el -l demo/record.el
;;     sh demo/assemble.sh md
;;
;; `demo/init.el' is the whole setup — a built-in theme, one font, the
;; five packages — so what you record is what the README shows.  There
;; is no screen recorder: Emacs exports its own frame with
;; `x-export-frames', which is why the pictures carry no mouse pointer
;; and no window decoration.
;;
;; A scenario drives one package: it does what a reader does and
;; photographs the result, saying how long each picture is held (in
;; hundredths of a second, as GIF counts).  OB_GIF names the scenario —
;; md, pydoc, pycell or rmd — and OB_GIF_OUT where the frames go.
;;
;; `demo/README.org' says what each one needs installed.

;;; Code:

(require 'cl-lib)

(defconst ob-gif-scenario (or (getenv "OB_GIF") "pydoc")
  "Which demonstration to record: md, pydoc, pycell or rmd.")
(defconst ob-gif-dir
  (file-name-as-directory
   (or (getenv "OB_GIF_OUT")
       (expand-file-name (concat "overblock-demo/" ob-gif-scenario)
                         temporary-file-directory)))
  "Where the frames and their holds are written.")
(defconst ob-gif-src
  (file-name-directory (or load-file-name buffer-file-name))
  "Where the files a scenario opens live, which is this directory.")
(defconst ob-gif-width 900)
(defconst ob-gif-height 480)

(defvar ob-gif-n 0)
(defvar ob-gif-holds nil)

(defun ob-gif-say (format &rest args)
  "Write FORMAT with ARGS to the log of this recording."
  (make-directory ob-gif-dir t)
  (write-region (concat (apply #'format format args) "\n") nil
                (expand-file-name "record.log" ob-gif-dir) 'append 'quiet))

(defun ob-gif-frame (hold)
  "Photograph the frame and hold the picture HOLD hundredths of a second.
The window the demonstration is in and nothing else: a fresh profile
has a warning to show and every one of them opened a window."
  (unless (window-minibuffer-p)
    (ignore-errors (window-toggle-side-windows))
    ;; The demonstration's own windows stay — an edit opens one, and the
    ;; picture is of that.  A fresh profile's warnings do not.
    (dolist (window (window-list nil 'no-mini))
      (unless (or (eq window (selected-window))
                  (string-match-p (rx (or "pydoc:" "cell" "markdown" "chunk"))
                                  (buffer-name (window-buffer window))))
        (ignore-errors (delete-window window)))))
  (message nil)
  (redisplay t)
  (let ((coding-system-for-write 'binary)
        (path (format "%s%03d.png" ob-gif-dir (cl-incf ob-gif-n))))
    (write-region (x-export-frames nil 'png) nil path nil 'quiet)
    (push hold ob-gif-holds)
    (ob-gif-say "frame %03d hold %s %s" ob-gif-n hold
                (file-attribute-size (file-attributes path)))))

(defun ob-gif-wait (seconds predicate)
  "Wait up to SECONDS for PREDICATE, letting timers and processes run.
`post-command-hook' is run as well: the live cycle of every mode here
renders what point has left once the reader stops, and a scenario moves
point by calling a function rather than by running a command."
  (let ((end (+ (float-time) seconds)))
    (run-hooks 'post-command-hook)
    (while (and (< (float-time) end) (not (funcall predicate)))
      ;; `sit-for' and not `accept-process-output': the rendering is
      ;; asked for by an idle timer, and idle timers run only while
      ;; Emacs waits like this.
      (sit-for 0.05)
      (redisplay t))
    (funcall predicate)))

(defun ob-gif-settle (&optional seconds)
  "Run what a command runs and let the idle timers fire.
A block reveals its source when point walks into it and renders it
again when point leaves, and both happen on `post-command-hook\': point
moved by a program moves through no command at all."
  (run-hooks 'post-command-hook)
  (sit-for (or seconds 0.4))
  (redisplay t))

(defun ob-gif-type (string &optional hold)
  "Insert STRING as if typed, photographing it as it grows."
  (dolist (chunk (seq-partition (append string nil) 6))
    (dolist (char chunk) (insert char))
    (ob-gif-frame (or hold 8))))

(defun ob-gif-file (name _content)
  "Return the file NAME of this directory, opened by a scenario.
The second argument is what the file used to be written from, kept so
that a scenario reads as it always did; the files live in the
repository now, beside this one, so that a reader sees exactly what was
recorded."
  (expand-file-name name ob-gif-src))

(defun ob-gif-setup ()
  "Empty the output directory and set the frame up.
The configuration is `demo/init.el\', which the command line loads
before this file: an animation in a README has to be reproducible by
whoever reads it."
  (delete-directory ob-gif-dir t)
  (make-directory ob-gif-dir t)
  (when (fboundp 'demo-set-font) (demo-set-font))
  (set-frame-size (selected-frame) ob-gif-width ob-gif-height t)
  (delete-other-windows)
  (dolist (name '("*Warnings*" "*Messages*" "*scratch*" "*Compile-Log*"))
    (when-let* ((buffer (get-buffer name))) (kill-buffer buffer)))
  (redisplay t)
  (ob-gif-say "== %s, frame %sx%s, font %s, theme %s" ob-gif-scenario
              (frame-pixel-width) (frame-pixel-height)
              (frame-parameter nil 'font) custom-enabled-themes))

(defun ob-gif-finish ()
  (with-temp-file (concat ob-gif-dir "holds.txt")
    (dolist (hold (reverse ob-gif-holds)) (insert (format "%d\n" hold))))
  (ob-gif-say "== %s frames" ob-gif-n)
  (kill-emacs 0))


;;;; The scenarios

(defun ob-gif-pydoc ()
  "A module of doc strings, rendered where they stand, by both renderers.
The class doc string is reStructuredText with everything in it a doc
string carries — a field list, math, a bullet list, a table and a code
block — so that the two renderers can be told apart: the converter
lays all of that out with shr, and `fontify\' paints the source where
the writer left it."
  (find-file (ob-gif-file "shapes.py" nil))
  (setq overblock-pydoc-renderer 'converter)
  (goto-char (point-min))
  (ob-gif-frame 250)
  (overblock-pydoc-mode 1)
  (ob-gif-wait 40 #'ob-gif-pydoc--settled)
  (ob-gif-frame 400)
  ;; the same doc strings, painted rather than converted
  (setq overblock-pydoc-renderer 'fontify)
  (overblock-pydoc--redraw)
  (ob-gif-wait 20 #'ob-gif-pydoc--settled)
  (ob-gif-frame 400)
  (setq overblock-pydoc-renderer 'converter)
  (overblock-pydoc--redraw)
  (ob-gif-wait 30 #'ob-gif-pydoc--settled)
  (ob-gif-frame 250)
  ;; and one of them opened, edited and put back
  (goto-char (point-min))
  (when-let* ((blocks (overblock-in (point-min) (point-max) 'pydoc)))
    (goto-char (apply #'min (mapcar #'overlay-start blocks))))
  (ob-gif-frame 80)
  (overblock-pydoc-edit)
  (ob-gif-wait 5 (lambda () (bound-and-true-p overblock-edit-mode)))
  (ob-gif-frame 250)
  (goto-char (point-min))
  (when (search-forward "immutable" nil t)
    (ob-gif-type " and cheap to copy"))
  (ob-gif-frame 150)
  (overblock-edit-commit)
  ;; Point out of the doc string it edited: point inside one shows its
  ;; source, which is the whole idea, and the last picture is of prose.
  (goto-char (point-min))
  (ob-gif-wait 30 #'ob-gif-pydoc--settled)
  (ob-gif-frame 350))

(defun ob-gif-pydoc--settled ()
  (let ((blocks (overblock-in (point-min) (point-max) 'pydoc)))
    (and (= (length blocks)
            (length (overblock-pydoc--strings (point-min) (point-max))))
         (seq-every-p (lambda (block)
                        (eql (overlay-get block 'overblock-pydoc-columns)
                             (overblock-window-columns)))
                      blocks))))

(defun ob-gif-md ()
  "A markdown file, each line rendered where it stands."
  (find-file (ob-gif-file "notes.md" "\
# The overblock family

A **block** is a rendering that sits over the text it came from.  The
text is untouched: it is still there, and the buffer still saves as
what it always was.

## What is in the family

| package | what it renders |
|---------|-----------------|
| `overblock-md` | markdown, line by line |
| `overblock-pydoc` | the doc strings of Python |
| `overblock-pycell` | the output of a notebook cell |

Click a line to see its source again:

- a list keeps its bullet
- `code` keeps its face
- [a link](https://example.com) is a link

```python
def hello(name):
    return f\"hello, {name}\"
```
"))
  ;; `demo/init.el' turns the mode on with the major mode, so the
  ;; picture of the source has to ask for the source.
  (overblock-md-preview-mode -1)
  (goto-char (point-min))
  (ob-gif-frame 250)
  ;; Point on the blank line under the heading before the mode goes on:
  ;; the paragraph point is in is left as source, which is the whole
  ;; idea, and a blank line belongs to no paragraph.
  (goto-char (point-min))
  (forward-line 1)
  (overblock-md-preview-mode 1)
  (ob-gif-wait 60 #'ob-gif-md--settled)
  (ob-gif-settle)
  (ob-gif-frame 400)
  ;; a click on a line shows what that line is made of
  (goto-char (point-min))
  (search-forward "overblock-pycell" nil t)
  (overblock-live-edit)
  ;; Photographed at once: the source is the reader's for as long as
  ;; they are in it, and the mode renders again a fifth of a second
  ;; after they stop.
  (ob-gif-frame 400)
  ;; and it reads as prose again once the reader has moved on
  (goto-char (point-min))
  (forward-line 1)
  (ob-gif-wait 30 #'ob-gif-md--settled)
  (ob-gif-settle)
  (ob-gif-frame 250)
  ;; a line written now is rendered where it stands
  (goto-char (point-max))
  (insert "\n")
  (ob-gif-type "## Every line, as it is written" 10)
  (forward-line 0)
  (forward-line -1)
  (ob-gif-wait 30 #'ob-gif-md--settled)
  (ob-gif-settle 0.8)
  (recenter -2)
  (ob-gif-frame 400))

(defun ob-gif-md--settled ()
  "Return non-nil where every region of the buffer carries a rendering."
  (= (length (overblock-in (point-min) (point-max) 'md-preview))
     (length (funcall overblock-md-preview-regions-function
                      (point-min) (point-max)))))

(defun ob-gif-pycell ()
  "A Python file as a notebook: cells, output, a figure."
  (find-file (ob-gif-file "demo.py" "\
# %% [markdown]
# # A notebook that is a Python file
# The cells are comments, so the file runs as a script as well.

# %%
import numpy as np

grid = np.linspace(0, 2 * np.pi, 9)
np.round(np.sin(grid), 3)

# %%
import matplotlib.pyplot as plt

fig, ax = plt.subplots(figsize=(5, 1.8))
ax.plot(grid, np.sin(grid), marker='o')
ax.set_title('sin over a turn')
fig.tight_layout()
plt.show()
"))
  (goto-char (point-min))
  (ob-gif-frame 250)
  (overblock-pycell-mode 1)
  (ob-gif-wait 30 (lambda () (overblock-in (point-min) (point-max) 'md-preview)))
  (ob-gif-frame 200)
  ;; the first cell that is code: run on a markdown cell renders it
  (goto-char (point-min))
  (search-forward "import numpy" nil t)
  (call-interactively #'overblock-pycell-run-cell)
  ;; The cell while it runs: a spinner, a stopwatch and a stop button.
  (ob-gif-wait 120 (lambda () (overblock-in (point-min) (point-max) 'result)))
  (ob-gif-frame 200)
  ;; and what it answered
  (ob-gif-wait 60 #'ob-gif-pycell--idle)
  (ob-gif-settle 0.4)
  (ob-gif-frame 350)
  ;; and the figure of the next one, drawn in the buffer
  (goto-char (point-min))
  (search-forward "plt.subplots" nil t)
  (call-interactively #'overblock-pycell-run-cell)
  (ob-gif-wait 120
               (lambda ()
                 (seq-some (lambda (block)
                             (overblock-image-in
                              (or (overblock-get block :body) "")))
                           (overblock-in (point-min) (point-max) 'result))))
  (ob-gif-settle 0.6)
  ;; The cell and its figure in one picture.
  (goto-char (point-min))
  (search-forward "import matplotlib" nil t)
  (recenter 2)
  (ob-gif-frame 450)
  ;; an output folded away to its bar
  (when-let* ((block (car (overblock-in (point-min) (point-max) 'result))))
    (goto-char (overlay-start block))
    (overblock-pycell-toggle-output)
    (ob-gif-settle 0.3)
    (ob-gif-frame 350)))

(defun ob-gif-pycell--idle ()
  "Return non-nil where the notebook\'s shell has finished its cell."
  (when-let* ((shell (overblock-run-shell)))
    (not (buffer-local-value 'overblock-run--state shell))))

(defun ob-gif-rmd ()
  "An R Markdown file: the prose rendered, the chunks run in place."
  (find-file (ob-gif-file "report.Rmd" "\
## Speed and stopping distance

The chunk below is **run where it stands**, and what R answers is a
block over the text — the file itself is untouched.

```{r summary}
summary(cars)
```

```{r head}
head(cars, 4)
```
"))
  ;; Markdown for the prose, where it is installed; the mode itself does
  ;; not care which major mode a chunk sits in.
  (if (require 'markdown-mode nil t) (markdown-mode) (text-mode))
  ;; ESS asks where to start R unless it is told; a picture cannot answer.
  (setq ess-ask-for-ess-directory nil
        ess-eval-visibly 'nowait
        inferior-R-args "--no-save --no-restore-data")
  (goto-char (point-min))
  (ob-gif-frame 250)
  (overblock-rmd-mode 1)
  ;; On a blank line: the line point is on shows its source, and a blank
  ;; line carries no rendering to give back.
  (goto-char (point-min))
  (forward-line 1)
  (ob-gif-wait 40 (lambda () (overblock-in (point-min) (point-max) 'md-preview)))
  (ob-gif-settle)
  (ob-gif-frame 350)
  ;; the first chunk, run in the R that ESS starts for it
  (goto-char (point-min))
  (search-forward "summary(cars" nil t)
  (call-interactively #'overblock-rmd-run-chunk)
  (ob-gif-wait 60 (lambda () (overblock-in (point-min) (point-max) 'result)))
  (ob-gif-say "shell=%S ess=%S buffers=%S"
              (ignore-errors (overblock-run-shell))
              (bound-and-true-p ess-local-process-name)
              (seq-filter (lambda (name) (string-match-p "R\\|ESS" name))
                          (mapcar #'buffer-name (buffer-list))))
  (when-let* ((shell (ignore-errors (overblock-run-shell))))
    (ob-gif-say "R tail: %S"
                (with-current-buffer shell
                  (buffer-substring-no-properties
                   (max (point-min) (- (point-max) 500)) (point-max)))))
  (ob-gif-settle 0.5)
  (ob-gif-frame 400)
  ;; and the second, which keeps what the first knew
  (ob-gif-wait 60 #'ob-gif-pycell--idle)
  (goto-char (point-min))
  (search-forward "head(cars" nil t)
  (call-interactively #'overblock-rmd-run-chunk)
  (ob-gif-wait 150
               (lambda ()
                 (>= (length (overblock-in (point-min) (point-max) 'result))
                     2)))
  (ob-gif-settle 0.6)
  (goto-char (point-min))
  (ob-gif-frame 450))

(condition-case err
    (progn
      (ob-gif-setup)
      (funcall (intern (concat "ob-gif-" ob-gif-scenario)))
      (ob-gif-finish))
  (error (ob-gif-say "error: %S" err) (kill-emacs 1)))

;;; record.el ends here
