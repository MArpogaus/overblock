;;; overblock-sh.el --- Cells of a shell script run in place  -*- lexical-binding: t; -*-

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

;; An example, and no package: the smallest notebook mode on
;; overblock-run.  A `# %%' line starts a cell of a shell script, and
;; each cell gets a bar with three run buttons.  A run sends the cell
;; to a bash of its own and shows the output under the cell.  A pass
;; over the cells stops at the first that fails.
;;
;; To try it, evaluate this file with M-x load-file, open a shell
;; script with `# %%' lines, and turn on M-x overblock-sh-mode.  Then
;; click the run button of a bar, or call `overblock-run-this'.
;;
;; It has the sections of every notebook mode: Options, Regions, Bars,
;; Backend, Mode.  The backend is a plist of functions, and
;; `overblock-run-attach' gives it to the runner, which holds the
;; queue, the results and every command.  docs/custom-mode.org walks
;; through this file.

;;; Code:

(require 'overblock)
(require 'overblock-run)
(require 'overblock-repl)
(require 'comint)

;;;; Options

(defgroup overblock-sh nil
  "Cells of a shell script run in place."
  :group 'overblock
  :prefix "overblock-sh-")

(defcustom overblock-sh-bar-buttons (overblock-run-bar-buttons "cell")
  "The buttons on the bar of a cell, left to right.
An entry has the shape `overblock-buttons' reads."
  :type overblock-button-type
  :set #'overblock-run-set-and-redraw)

(defcustom overblock-sh-result-buttons
  (overblock-run-result-buttons "cell" "image")
  "The buttons on the header of a result, left to right.
An entry has the shape `overblock-buttons' reads."
  :type overblock-button-type
  :set #'overblock-run-set-and-redraw)

;;;; Regions

(defconst overblock-sh--boundary "^#[[:blank:]]*%%"
  "What the line that starts a cell looks like.")

(defun overblock-sh--region-at ()
  "Return the cell point is in as (BEG . END), boundary line included.
This is the `:region-at' of the backend.  The text above the first
boundary line is no cell."
  (save-excursion
    (end-of-line)
    (when (re-search-backward overblock-sh--boundary nil t)
      (cons (point)
            (progn (end-of-line)
                   (if (re-search-forward overblock-sh--boundary nil t)
                       (pos-bol)
                     (point-max)))))))

(defun overblock-sh--code-at ()
  "Return the code of the cell point is in as (BEG . END), or nil.
This is the `:code-at' of the backend: the cell without its boundary
line.  The result hangs on the last newline of the code."
  (when-let* ((cell (overblock-sh--region-at)))
    (cons (save-excursion (goto-char (car cell)) (pos-bol 2))
          (cdr cell))))

(defun overblock-sh--starts ()
  "Return a marker on the boundary line of every cell, in order.
This is the `:starts' of the backend."
  (save-excursion
    (goto-char (point-min))
    (let (starts)
      (while (re-search-forward overblock-sh--boundary nil t)
        (push (copy-marker (pos-bol)) starts))
      (nreverse starts))))

;;;; Bars

(defun overblock-sh--bar ()
  "Draw the bar over the boundary line point is on, and return it.
This is the `:bar' of the backend.  The label is what follows `%%'."
  (looking-at (concat overblock-sh--boundary "[[:blank:]]*\\(.*\\)"))
  (let ((title (string-trim (match-string-no-properties 1))))
    (overblock-bar-line (pos-bol) (pos-eol) 'sh
                        (overblock-glyph "" "$")
                        (if (string-empty-p title) "bash" title)
                        (overblock-buttons overblock-sh-bar-buttons))))

;;;; Backend

(defconst overblock-sh--prompt "overblock-sh$ "
  "The prompt of the bash of a notebook.
Set through the environment, so no start-up file can change it.")

(defun overblock-sh--process ()
  "Return the live bash of this notebook, or nil for none.
This is the `:process' of the backend.  Each notebook has its own."
  (get-buffer-process (format "*overblock-sh: %s*" (buffer-name))))

(defun overblock-sh--start ()
  "Start a bash for this notebook, and return it once it has prompted.
This is the `:start' of the backend.  The process is ready for a cell
when this returns, so the backend needs no `:arm'.

No start-up files, no line editing, which would echo the input, and no
history file.  The prompt comes from the environment, and an empty
second prompt keeps a cell of several lines silent."
  (let* ((process-environment
          (append (list (concat "PS1=" overblock-sh--prompt) "PS2=" "HISTFILE=")
                  process-environment))
         (buffer (make-comint-in-buffer
                  "overblock-sh" (format "*overblock-sh: %s*" (buffer-name))
                  "bash" nil "--norc" "--noprofile" "--noediting" "-i"))
         (proc (get-buffer-process buffer))
         ;; A restart starts in the old buffer, which ends at a prompt.
         (from (marker-position (process-mark proc))))
    (with-current-buffer buffer
      (with-timeout (10 (error "Bash did not prompt in ten seconds"))
        (while (not (string-search overblock-sh--prompt
                                   (buffer-substring from (point-max))))
          (accept-process-output proc 0.1))))
    proc))

(defun overblock-sh--send (proc beg end)
  "Send the cell BEG..END to PROC.
This is the `:send' of the backend.  The cell goes as one group, so
bash prompts once, at its end.  The `:' keeps an empty cell a valid
group, and a cell that fails says its exit status last."
  (comint-send-string
   proc (format "{ :\n%s\n} || echo \"[exit $?]\"\n"
                (buffer-substring-no-properties beg end))))

(defun overblock-sh--prompt-p (tail)
  "Return non-nil where TAIL ends at the prompt.
This is the `:prompt-p' of the backend."
  (string-suffix-p overblock-sh--prompt tail))

(defun overblock-sh--clean (text)
  "Return TEXT as a result block can show it.
This is the `:clean' of the backend.  The prompt goes, and the copy is
cut loose from the shell."
  (overblock-repl-detach
   (overblock-repl-strip-trailing-prompt
    text (regexp-quote overblock-sh--prompt))))

(defun overblock-sh--error-p (text)
  "Return non-nil where TEXT is the output of a cell that failed.
This is the `:error-p' of the backend: the exit status that
`overblock-sh--send' adds, or a syntax error, which runs nothing."
  (string-match-p "^\\[exit [0-9]+\\]\\'\\|^bash: syntax error" text))

(defun overblock-sh--backend ()
  "Return what `overblock-run' needs to drive a bash.
docs/custom-mode.org lists the slots."
  (list :name "overblock-sh"
        :unit "cell"
        :process #'overblock-sh--process
        :start #'overblock-sh--start
        :send #'overblock-sh--send
        :prompt-p #'overblock-sh--prompt-p
        :clean #'overblock-sh--clean
        :error-p #'overblock-sh--error-p
        :region-at #'overblock-sh--region-at
        :code-at #'overblock-sh--code-at
        :starts #'overblock-sh--starts
        :bar #'overblock-sh--bar
        :buttons 'overblock-sh-result-buttons))

;;;; Mode

(define-minor-mode overblock-sh-mode
  "Run the `# %%' cells of this shell script and show their results inline.
Every cell gets a bar with run buttons.  The commands of
`overblock-run' work here, such as `overblock-run-this' and
`overblock-run-restart-and-run-all'.  Turn the mode off to remove the
bars and the results."
  :lighter " ShNb"
  (overblock-only-in 'overblock-sh-mode 'sh-mode)
  (if overblock-sh-mode
      (overblock-run-attach (overblock-sh--backend))
    (overblock-run-detach)))

(provide 'overblock-sh)
;;; overblock-sh.el ends here
