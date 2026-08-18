;;; pycell-test.el --- Tests for pycell -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Marcel Arpogaus

;; Author: Marcel Arpogaus <znepry.necbtnhf@tznvy.pbz>
;; Assisted-by: Claude:claude-opus-5
;; Assisted-by: Claude:claude-fable-5
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

;; Run with: make test
;;
;; The tests cover what works without an inferior Python process: the
;; output clean-up, the block layout and the markdown helpers.

;;; Code:

(require 'ert)
(require 'outline)
(require 'pycell)

(defconst pycell-test--image
  (propertize " " 'display '(image :type png :data "x"))
  "A stand-in for what comint-mime inserts for an image.")

(defmacro pycell-test--with-cells (&rest body)
  "Evaluate BODY in a Python buffer with two code cells."
  (declare (indent 0))
  `(with-temp-buffer
     (insert "# %%\nx = 1\n\n# %%\ny = 2\n")
     (python-mode)
     (code-cells-mode)
     (goto-char (point-min))
     ,@body))

;;;; Output clean-up

(ert-deftest pycell-test-clean-prompts ()
  "Prompts at both ends and Out[n] markers go, whitespace is trimmed."
  (let ((comint-prompt-regexp "^\\(?:>>> \\|In \\[[0-9]+\\]: \\)"))
    (should (equal (pycell--clean ">>> 2\n>>> ") "2"))
    (should (equal (pycell--clean "a\nOut[3]: 42\n") "a\n42"))
    (should (equal (pycell--clean "  x  ") "x"))
    (should (equal (pycell--clean "no prompts") "no prompts"))))

(ert-deftest pycell-test-clean-terminates-on-empty-prompt ()
  "A prompt regexp that matches the empty string must not loop forever."
  (let ((comint-prompt-regexp "^"))
    (should (equal (pycell--clean "text") "text"))))

(ert-deftest pycell-test-clean-keeps-a-leading-image ()
  "A figure that is the whole output survives the prompt strip.
comint-mime renders an image as one space with a display property,
and the run of whitespace before a prompt would take it along, which
left a cell whose only output is a figure with an empty block."
  (let* ((comint-prompt-regexp "^\\(?:>>> \\|In \\[[0-9]+\\]: \\)")
         (result (pycell--clean (concat pycell-test--image "\n\nIn [5]: "))))
    (should (= (length result) 1))
    (should (pycell--image result))
    ;; a prompt with nothing to show before it still goes
    (should (equal (pycell--clean "In [5]: 42") "42"))))

(ert-deftest pycell-test-clean-keeps-images ()
  "Whitespace that carries a display property is part of the result."
  (let* ((comint-prompt-regexp "^>>> ")
         (result (pycell--clean (concat "plot\n" pycell-test--image "\n"))))
    (should (equal result (concat "plot\n" pycell-test--image)))
    (should (get-text-property (1- (length result)) 'display result))))

;;;; Block layout

(ert-deftest pycell-test-body-lines-cap ()
  "At most `pycell-max-lines' lines show inline."
  (let ((pycell-max-lines 3)
        (lines '("1" "2" "3" "4" "5")))
    (should (equal (pycell--body-lines lines) '("1" "2" "3")))))

(ert-deftest pycell-test-body-lines-stop-after-image ()
  "Nothing after the first image line shows inline."
  (cl-letf (((symbol-function 'display-images-p) (lambda (&rest _) t)))
    (let ((pycell-max-lines 10))
      (should (equal (pycell--body-lines (list "text" pycell-test--image "more"))
                     (list "text" pycell-test--image))))))

(ert-deftest pycell-test-body-lines-run-on-without-images ()
  "A display that cannot draw an image has nothing to stop for.
In a terminal the image is the space it rides on, so stopping there
would hide the rest of the output and buy no height back."
  (cl-letf (((symbol-function 'display-images-p) (lambda (&rest _) nil)))
    (let ((pycell-max-lines 10))
      (should (equal (pycell--body-lines (list "before" pycell-test--image "after"))
                     (list "before" pycell-test--image "after"))))))

(ert-deftest pycell-test-image ()
  "The first image of a result is found, and plain text has none."
  (should (eq (car-safe (pycell--image (concat "a\n" pycell-test--image))) 'image))
  (should-not (pycell--image "just text")))

(ert-deftest pycell-test-icons ()
  "The icon group drops the buttons that are not there."
  (should (equal (pycell--icons "a" nil "b") "a  b "))
  (should (equal (pycell--icons nil) " ")))

(ert-deftest pycell-test-glyph ()
  "A candidate without a glyph is skipped, and the last one always answers."
  ;; A batch session has no graphical frame, so the fallback decides.
  (should (equal (pycell--glyph "⤓" "↧" "↓") "↓"))
  (should (equal (pycell--glyph "x") "x")))

(ert-deftest pycell-test-glyph-weighs-every-character ()
  "A leading space must not answer for the glyph behind it.
Several candidates lead with one, and a space is always there, so
asking the first character alone accepted every candidate."
  (cl-letf (((symbol-function 'display-graphic-p) (lambda (&rest _) t))
            ;; a frame with the space and two of the three arrows
            ((symbol-function 'internal-char-font)
             (lambda (_frame ch) (memq ch '(?\s ?▶ ?>)))))
    (should (equal (pycell--glyph " ▸" " ▶" " >") " ▶"))
    (should (equal (pycell--glyph " ▸" " ▴" " >") " >"))))

(ert-deftest pycell-test-faced ()
  "A block string carries a base face, so it inherits none."
  (let ((s (pycell--faced (copy-sequence "text") 'pycell-output)))
    (should (memq 'pycell-output (ensure-list (get-text-property 0 'face s))))))

(ert-deftest pycell-test-show-text-result ()
  "A text result rides the newline, and the buffer text stays as it was."
  (pycell-test--with-cells
    (let ((before (buffer-substring-no-properties (point-min) (point-max))))
      (pcase-let ((`(,beg ,end) (code-cells--bounds nil nil t)))
        (pycell--show beg end "42" 0.5)
        (let ((ov (car (pycell--overlays (point-min) (point-max)))))
          (should ov)
          (should (overlay-get ov 'after-string))
          ;; the body is a display property on the carrier overlay
          (should (overlay-get (overlay-get ov 'pycell-body) 'display))
          (should (string-match-p "42"
                                  (overlay-get (overlay-get ov 'pycell-body)
                                               'display)))))
      (should (equal (buffer-substring-no-properties (point-min) (point-max))
                     before)))))

(ert-deftest pycell-test-show-image-result ()
  "An image result rides the after-string, where display properties work."
  (pycell-test--with-cells
    (pcase-let ((`(,beg ,end) (code-cells--bounds nil nil t)))
      (pycell--show beg end (concat "plot\n" pycell-test--image) 0.5)
      (let* ((ov (car (pycell--overlays (point-min) (point-max))))
             (bov (overlay-get ov 'pycell-body)))
        (should (pycell--image (overlay-get ov 'after-string)))
        (should-not (overlay-get bov 'display))))))

(ert-deftest pycell-test-raised-text-is-not-an-image ()
  "Superscripts do not push a result onto the string path.
shr raises a superscript with a display property, and inline math is
full of those; only a real image belongs in the after-string."
  (pycell-test--with-cells
    (pcase-let ((`(,beg ,end) (code-cells--bounds nil nil t)))
      (pycell--show beg end
                       (concat "E = mc"
                               (propertize "2" 'display '(raise 0.2)))
                       0.1)
      (let* ((ov (car (pycell--overlays (point-min) (point-max))))
             (bov (overlay-get ov 'pycell-body)))
        ;; the cheap path: the body rides the newline
        (should (overlay-get bov 'display))
        (should (string-match-p "mc" (overlay-get bov 'display)))))))

(ert-deftest pycell-test-body-lines-keep-raised-text ()
  "Raised text does not cut the inline part short; an image does."
  (let ((pycell-max-lines 10))
    (should (equal (pycell--body-lines
                    (list "x" (propertize "2" 'display '(raise 0.2)) "y"))
                   (list "x" (propertize "2" 'display '(raise 0.2)) "y")))))

(ert-deftest pycell-test-show-keeps-fold-state ()
  "Replacing a result keeps whether it was folded."
  (pycell-test--with-cells
    (pcase-let ((`(,beg ,end) (code-cells--bounds nil nil t)))
      (pycell--show beg end "a\nb" 0.1)
      (let ((ov (car (pycell--overlays (point-min) (point-max)))))
        (setcar (overlay-get ov 'pycell) t))
      (pycell--show beg end "c\nd" 0.2)
      (let ((ov (car (pycell--overlays (point-min) (point-max)))))
        (should (car (overlay-get ov 'pycell)))))))

(ert-deftest pycell-test-remove-overlays ()
  "Removing results takes the helper overlays with them."
  (pycell-test--with-cells
    (pcase-let ((`(,beg ,end) (code-cells--bounds nil nil t)))
      (pycell--show beg end "42" 0.1))
    (let ((bov (overlay-get (car (pycell--overlays (point-min) (point-max)))
                            'pycell-body)))
      (pycell-remove-overlays)
      (should-not (pycell--overlays (point-min) (point-max)))
      (should-not (overlay-buffer bov)))))

(ert-deftest pycell-test-fold-keeps-result ()
  "An outline fold hides the code and leaves the result in place.
The block below the fold keeps its own fold button, so the two fold
separately."
  (pycell-test--with-cells
    (pcase-let ((`(,beg ,end) (code-cells--bounds nil nil t)))
      (pycell--show beg end "a\nb" 0.1)
      (let* ((ov (car (pycell--overlays (point-min) (point-max))))
             (bov (overlay-get ov 'pycell-body))
             (head (overlay-get ov 'after-string))
             (body (overlay-get bov 'display)))
        (outline-flag-region beg (1- end) t)
        (should (equal (overlay-get ov 'after-string) head))
        (should (equal (overlay-get bov 'display) body))
        (outline-flag-region beg (1- end) nil)
        (should (equal (overlay-get bov 'display) body))))))

(ert-deftest pycell-test-fold-shrinks-at-buffer-end ()
  "A fold to the end of the buffer stops before the block's newline.
Only there does `outline-flag-region' cover it; mid-buffer it stops
one character short on its own."
  (with-temp-buffer
    (insert "# %%\nx = 1\ny = x + 1\n")
    (python-mode)
    (code-cells-mode)
    (pcase-let ((`(,beg ,end) (progn (goto-char (point-min))
                                     (code-cells--bounds nil nil t))))
      (pycell--show beg end "42" 0.1)
      (let ((bov (overlay-get (car (pycell--overlays (point-min) (point-max)))
                              'pycell-body)))
        (outline-flag-region beg (point-max) t)
        (should-not
         (seq-some (lambda (o) (and (eq (overlay-get o 'invisible) 'outline)
                                    (> (overlay-end o) (overlay-start bov))))
                   (overlays-in (overlay-start bov) (overlay-end bov))))))))

(ert-deftest pycell-test-fold-md-round-trip ()
  "An outline fold takes a markdown block along, and gives it back."
  (skip-unless (pycell--md-program))
  (with-temp-buffer
    (insert "# %% [markdown]\n# ## A\n#\n# Text here.\n\n# %%\ny = 2\n")
    (python-mode)
    (code-cells-mode)
    (pycell-md-render-all)
    (goto-char (point-min))
    (let* ((ov (car (pycell--overlays (point-min) (point-max) 'pycell-body)))
           (parts (overlay-get ov 'pycell-parts))
           (shown (lambda ()
                    (seq-some (lambda (p) (not (overlay-get p 'invisible)))
                              parts))))
      (should parts)
      (should (funcall shown))
      (outline-flag-region (pos-eol) (overlay-end ov) t)
      (should-not (funcall shown))
      (outline-flag-region (pos-eol) (overlay-end ov) nil)
      (should (funcall shown)))))

(ert-deftest pycell-test-md-keeps-its-lines ()
  "A rendered markdown cell stays as many lines as its source.
One display string for the whole cell would collapse it to one line,
and every scroll event would then lay the whole thing out again."
  (skip-unless (pycell--md-program))
  (with-temp-buffer
    (insert "# %% [markdown]\n# ## A\n#\n# Text here.\n\n# %%\ny = 2\n")
    (python-mode)
    (code-cells-mode)
    (pycell-md-render-all)
    (let* ((ov (car (pycell--overlays (point-min) (point-max) 'pycell-body)))
           (parts (overlay-get ov 'pycell-parts)))
      (should (> (length parts) 1))
      (should-not (overlay-get ov 'invisible))
      (pcase-let ((`(,beg . ,_) (overlay-get ov 'pycell-md)))
        (should (= (overlay-start (car parts)) (marker-position beg))))
      ;; Every piece shows text; what is left over is cloaked.
      (should (seq-every-p (lambda (p) (or (stringp (overlay-get p 'display))
                                           (overlay-get p 'pycell-cloak)))
                           parts)))))

(ert-deftest pycell-test-md-parts-lose-no-line ()
  "The pieces together show the rendering, whole and in order.
A cell has as many lines as its author wrote and the rendering has as
many as it needs, so the two rarely match either way."
  (let ((shown (lambda (parts)
                 (mapconcat (lambda (p) (overlay-get p 'display))
                            (seq-remove (lambda (p) (overlay-get p 'pycell-cloak))
                                        parts)
                            "\n"))))
    ;; more rendering than lines to put it on
    (with-temp-buffer
      (insert "aaa\nbbb\n")
      (let ((text "one\ntwo\nthree\nfour\nfive"))
        (should (equal (funcall shown (pycell--md-parts (point-min) (point-max) text))
                       text))))
    ;; more lines than rendering
    (with-temp-buffer
      (insert "aaa\nbbb\nccc\nddd\neee\n")
      (let* ((text "one\ntwo")
             (parts (pycell--md-parts (point-min) (point-max) text)))
        (should (equal (funcall shown parts) text))
        (should (seq-some (lambda (p) (overlay-get p 'pycell-cloak)) parts))))))

(ert-deftest pycell-test-md-cloak-starts-on-a-newline ()
  "A hidden run starts at the end of a visible line, never at a start.
`scroll-down' answers a run that begins a line with a
beginning-of-buffer error, and a piece with nothing to show would
leave a line of no height, which stops scrolling up the same way."
  (skip-unless (pycell--md-program))
  (with-temp-buffer
    (insert "# %% [markdown]\n# ## A\n#\n#\n#\n# Text here.\n#\n#\n\n# %%\ny = 2\n")
    (python-mode)
    (code-cells-mode)
    (pycell-md-render-all)
    (let* ((ov (car (pycell--overlays (point-min) (point-max) 'pycell-body)))
           (parts (overlay-get ov 'pycell-parts))
           (cloaks (seq-filter (lambda (p) (overlay-get p 'pycell-cloak)) parts)))
      (should parts)
      (should cloaks)
      (dolist (part parts)
        (if (overlay-get part 'pycell-cloak)
            (should (eq (char-after (overlay-start part)) ?\n))
          ;; A piece covers the text of its line and nothing else, so
          ;; the line keeps its own newline and its height with it.
          (should-not (string-search "\n" (buffer-substring
                                           (overlay-start part)
                                           (overlay-end part)))))))))

;;;; Markdown cells

(ert-deftest pycell-test-md-comment-round-trip ()
  "Commenting and uncommenting a markdown cell is lossless."
  (let ((md "# Title\n\nSome *text*.\n\nMore."))
    (should (equal (pycell--md-uncomment (pycell--md-comment md)) md))
    (should (equal (pycell--md-comment "a\n\nb") "# a\n#\n# b"))))

(ert-deftest pycell-test-md-head ()
  "A markdown cell is recognized by the boundary line above its body."
  (with-temp-buffer
    (insert "# %% [markdown]\n# Title\n")
    (goto-char (point-min))
    (forward-line 1)
    (should (pycell--md-head (point)))
    (should (= (pycell--md-head (point)) (point-min))))
  (with-temp-buffer
    (insert "# %%\nx = 1\n")
    (goto-char (point-min))
    (forward-line 1)
    (should-not (pycell--md-head (point)))))

(ert-deftest pycell-test-md-program ()
  "A string and a list of candidates both resolve to a program.
Only where there is a parser to read the converter's HTML with: see
`pycell-test-md-program-needs-libxml' for the other direction."
  (skip-unless (fboundp 'libxml-parse-html-region))
  (let ((pycell-markdown-command "definitely-not-installed-xyz"))
    (should-not (pycell--md-program)))
  (let ((pycell-markdown-command (list "definitely-not-installed-xyz"
                                       (concat (car (split-string-shell-command
                                                     "emacs"))
                                               " --version"))))
    (should (equal (car (pycell--md-program)) "emacs"))))

(ert-deftest pycell-test-md-rendered ()
  "Markdown becomes text, when a converter is installed.
Pixel filling needs font metrics, which a batch session has none of."
  (skip-unless (pycell--md-program))
  (let* ((shr-use-fonts nil)
         (shr-width 60)
         (out (pycell--md-rendered "# Title\n\nSome *text* here.\n")))
    (should (string-match-p "Title" out))
    (should (string-match-p "Some text here" out))))

(ert-deftest pycell-test-md-fill-prop ()
  "Properties are filled in only where the string carries none.
The rendered markdown keeps the keymap that shr gave its links."
  (let ((s (concat "plain" (propertize "link" 'keymap 'shr-map))))
    (pycell--fill-prop s 'keymap 'pycell-md-map)
    (should (eq (get-text-property 0 'keymap s) 'pycell-md-map))
    (should (eq (get-text-property 6 'keymap s) 'shr-map))))

(ert-deftest pycell-test-cold-cell-belongs-to-its-buffer ()
  "The cell that waits for a cold interpreter is marked in its own buffer.
`copy-marker' on a number answers for the current buffer, so marking
the cell inside the shell's buffer would remember a stretch of the
shell instead, and the start-up banner would go to Python as the
cell."
  (let* ((shell (generate-new-buffer "*pycell test shell*"))
         (proc (make-pipe-process :name "pycell test" :buffer shell
                                  :noquery t :filter #'ignore)))
    (unwind-protect
        (let ((notebook (current-buffer)))
          (pycell-test--with-cells
            (setq notebook (current-buffer))
            (with-current-buffer shell
              (insert "Python 3.14.6 | packaged by conda-forge\nIn [1]: "))
            (pcase-let ((`(,beg ,end) (code-cells--bounds nil nil t)))
              (cl-letf (((symbol-function 'python-shell-get-process)
                         (lambda (&rest _) nil))
                        ((symbol-function 'python-shell-get-process-or-error)
                         (lambda (&rest _) proc))
                        ((symbol-function 'run-python) (lambda (&rest _) shell)))
                (pycell-eval-region beg end)))
            (let ((cell (buffer-local-value 'pycell--cold-cell shell)))
              (should cell)
              (should (eq (marker-buffer (car cell)) notebook))
              (should (eq (marker-buffer (cdr cell)) notebook)))))
      (delete-process proc)
      (kill-buffer shell))))

(ert-deftest pycell-test-clean-strips-a-prompt-on-the-same-line ()
  "A prompt that follows output on one line goes too.
Output that ends without a newline leaves the shell's prompt on the
same line, and `comint-prompt-regexp' anchors to the start of one."
  (with-temp-buffer
    (setq-local comint-prompt-regexp "^\\(>>> \\|In \\[[0-9]+\\]: \\)")
    (should (equal (pycell--clean "abc>>> ") "abc"))
    (should (equal (pycell--clean "a\nb\n\nIn [9]: ") "a\nb"))
    (should (equal (pycell--clean ">>> ") ""))
    ;; nothing to take off
    (should (equal (pycell--clean "a\nb") "a\nb"))))

(ert-deftest pycell-test-filter-copies-all-the-output ()
  "The finished cell gets everything the shell printed.
`comint-last-prompt' is no use as the end of the region: comint calls
the last line without a newline a prompt, so a chunk that arrives
split leaves it inside the output, and the rest would be dropped
without a word."
  (let ((shell (generate-new-buffer "*pycell test shell*"))
        ended)
    (unwind-protect
        (with-current-buffer shell
          (setq-local comint-prompt-regexp "^\\(>>> \\|In \\[[0-9]+\\]: \\)")
          (insert "In [1]: ")
          (let ((start (point-max-marker)))
            (insert "line 0\nline 1\nline 2\n\nIn [2]: ")
            ;; as comint leaves it after a split chunk: inside the output
            (setq-local comint-last-prompt
                        (cons (copy-marker (+ start 7)) (copy-marker (+ start 13))))
            (setq pycell--run (list start nil nil "" (float-time) nil))
            (cl-letf (((symbol-function 'python-shell-comint-end-of-output-p)
                       (lambda (&rest _) t))
                      ((symbol-function 'pycell--end)
                       (lambda (text &rest _) (setq ended text))))
              (pycell--filter "\nIn [2]: "))))
      (kill-buffer shell))
    (should (equal (substring-no-properties (or ended ""))
                   "line 0\nline 1\nline 2"))))

(ert-deftest pycell-test-at-point-survives-a-frame-switch ()
  "An event that carries no place leaves point alone instead of failing.
The commands read their event from `last-input-event', so it can be
any event at all.  A `switch-frame' is a cons like a click, but its
start is a frame, and asking that for a position signals."
  (pycell-test--with-cells
    (pcase-let ((`(,beg ,end) (code-cells--bounds nil nil t)))
      (pycell--show beg end "42" 0.1))
    (goto-char (point-min))
    (forward-line 1)
    (let ((here (point)))
      (should (eq (pycell--at-point (list 'switch-frame (selected-frame)) 'pycell)
                  (pycell--at-point nil 'pycell)))
      (should (= (point) here)))))

(defun pycell-test--ipython-syntax-p (code)
  "Return non-nil when CODE would take the IPython road."
  (with-temp-buffer
    (insert code)
    (python-mode)
    (pycell--ipython-syntax-p (point-min) (point-max))))

(ert-deftest pycell-test-ipython-syntax ()
  "Magics, shell escapes and help are told apart from plain Python.
Where the character means something to Python it stays Python: a
shell without IPython would answer the other road with a NameError."
  (let ((ipython #'pycell-test--ipython-syntax-p))
    (should (funcall ipython "%matplotlib inline"))
    (should (funcall ipython "x = 1\n%time f()"))
    (should (funcall ipython "%%time\nsum(range(10))"))
    (should (funcall ipython "!echo hi"))
    (should (funcall ipython "print?"))
    (should (funcall ipython "  %cd /tmp"))
    (should-not (funcall ipython "print('plain')"))
    (should-not (funcall ipython "x = a % b"))
    (should-not (funcall ipython "if a != b:\n    pass"))
    (should-not (funcall ipython "print('what?')"))
    ;; a comment may ask a question
    (should-not (funcall ipython "# is this right?\nprint('yes')"))
    ;; a continuation line may begin with a modulo
    (should-not (funcall ipython "total = (1\n         % 2)"))
    ;; and a docstring may do either
    (should-not (funcall ipython "s = \"\"\"why?\nmore\"\"\"\n"))
    (should-not (funcall ipython "s = \"\"\"a\n% b\n\"\"\"\n"))))

(ert-deftest pycell-test-send-to-ipython-carries-the-source ()
  "The cell reaches `run_cell' as it was written, quotes and all.
It travels base64 encoded for exactly that reason, and the trailing
None keeps the result object out of the block."
  (let ((code "%time f('a\"b')\nx = 1\n")
        sent)
    (cl-letf (((symbol-function 'python-shell-send-string)
               (lambda (string &rest _) (setq sent string))))
      (pycell--send-to-ipython nil code))
    (should (string-match "b64decode(\"\\([^\"]+\\)\")" sent))
    (should (equal (decode-coding-string
                    (base64-decode-string (match-string 1 sent)) 'utf-8)
                   code))
    (should (string-suffix-p "None\n" sent))))

(ert-deftest pycell-test-md-commit-keeps-the-gap ()
  "Committing an edit that changed nothing leaves the file alone.
A cell reaches to the next boundary line, so the blank line jupytext
writes between cells belongs to it and has to be written back."
  (skip-unless (pycell--md-program))
  (let ((text "# %% [markdown]\n# ## Heading\n#\n# The prose.\n\n# %%\nx = 1\n")
        (notebook (generate-new-buffer "*pycell test notebook*"))
        edit)
    (unwind-protect
        (progn
          (with-current-buffer notebook
            (insert text)
            (python-mode)
            (code-cells-mode)
            (pycell-md-render-all)
            (goto-char (point-min))
            (forward-line 1)
            (let ((prefix (format "*pycell md: %s:" (buffer-name))))
              ;; `pycell-md-edit' pops to its buffer, which leaves that
              ;; buffer current for the rest of this form.  The buffer
              ;; is named after the cell, so look it up by its prefix.
              (save-window-excursion (pycell-md-edit))
              (setq edit (seq-find (lambda (b)
                                     (string-prefix-p prefix (buffer-name b)))
                                   (buffer-list)))))
          (should edit)
          ;; `pycell-md-commit' ends by quitting its window, and the
          ;; edit buffer is not displayed here, so that would kill
          ;; whatever the selected window holds — and the next test
          ;; would find a marker into a dead buffer.  The round trip
          ;; is what this checks.
          (cl-letf (((symbol-function 'quit-window) #'ignore))
            (with-current-buffer edit (pycell-md-commit)))
          (with-current-buffer notebook
            (should (equal (buffer-substring-no-properties (point-min) (point-max))
                           text))))
      (when (buffer-live-p edit) (kill-buffer edit))
      (kill-buffer notebook))))

(ert-deftest pycell-test-body-lines-cut-a-long-line ()
  "A line longer than the cap is cut and the cut is marked.
One long line is one line, so the line cap does not bound it, and the
block costs what it holds: a hundred thousand characters on one line
were a fifth of a second a wheel event."
  (let ((pycell-max-lines 12)
        (pycell-max-line-length 10))
    (should (equal (pycell--body-lines (list "short" (make-string 30 ?x)))
                   (list "short" (concat (make-string 10 ?x)
                                         (pycell--glyph "…" "...")))))
    ;; a line with an image on it keeps every character: the image may
    ;; sit past the cut.  Only where the display can draw one — in a
    ;; terminal it is a space like any other and the line is cut.
    (cl-letf (((symbol-function 'display-images-p) (lambda (&rest _) t)))
      (let ((line (concat (make-string 30 ?x) pycell-test--image)))
        (should (equal (pycell--body-lines (list line)) (list line))))))
  ;; zero cuts nothing
  (let ((pycell-max-lines 12)
        (pycell-max-line-length 0))
    (should (equal (pycell--body-lines (list (make-string 30 ?x)))
                   (list (make-string 30 ?x))))))

(ert-deftest pycell-test-md-program-needs-libxml ()
  "Without the parser there is no converter worth naming.
shr reads the converter's HTML with `libxml-parse-html-region', which
an Emacs built without libxml2 does not have.  Rendering signalled a
void function there, on every markdown cell, instead of saying so and
leaving the cells as text."
  (cl-letf (((symbol-function 'libxml-parse-html-region) nil))
    (should-not (fboundp 'libxml-parse-html-region))
    (should-not (pycell--md-program))
    ;; and rendering says so instead of failing
    (with-temp-buffer
      (insert "# %% [markdown]\n# ## Heading\n#\n# Prose.\n\n# %%\nx = 1\n")
      (python-mode)
      (code-cells-mode)
      (let ((before (buffer-string))
            said)
        (cl-letf (((symbol-function 'message)
                   (lambda (fmt &rest args)
                     (setq said (and fmt (apply #'format fmt args))))))
          (pycell-md-render-all))
        (should (string-match-p "libxml" said))
        (should (equal before (buffer-string)))
        (should-not (pycell--overlays (point-min) (point-max) 'pycell-md))))))

(ert-deftest pycell-test-fold-md-image-at-buffer-end ()
  "A cell with an image folds even where the buffer ends without one.
The pieces of such a cell hang on its source lines, and the fold
covers those lines but stops one character short of the last newline,
so the last piece has to be hidden along.  A cell at the end of a
buffer that ends without a newline is the case that used to keep its
figure on screen below the fold."
  (skip-unless (pycell--md-program))
  (dolist (trailing '("\n" ""))
    (with-temp-buffer
      (insert "# %% [markdown]\n# ## Prose\n#\n# Words.\n\n"
              "# %%\nx = 1\n\n"
              "# %% [markdown]\n# ## A figure\n#\n# ![pic](pic.png)" trailing)
      (python-mode)
      (code-cells-mode)
      (pycell-md-render-all)
      ;; the cell overlays in order; the last one holds the figure
      (let* ((cells (seq-filter (lambda (o) (overlay-get o 'pycell-main))
                                (pycell--overlays (point-min) (point-max)
                                                  'pycell-md)))
             (last (overlay-get (car (last cells)) 'pycell-main))
             ;; what the cell shows: its pieces, or the one string of a
             ;; cell that renders to nothing
             (shown (lambda ()
                      (if-let* ((parts (overlay-get last 'pycell-parts)))
                          (seq-count (lambda (p)
                                       (and (not (overlay-get p 'pycell-cloak))
                                            (not (overlay-get p 'invisible))))
                                     parts)
                        (length (or (overlay-get last 'after-string) ""))))))
        (should last)
        (should (> (funcall shown) 0))
        (outline-flag-region (point-min) (point-max) t)
        (should (= (funcall shown) 0))
        (outline-flag-region (point-min) (point-max) nil)
        (should (> (funcall shown) 0))))))

(ert-deftest pycell-test-show-gives-the-last-cell-a-newline ()
  "The block needs a newline to hang on, and the last cell may lack one.
This is the one change the package makes to a buffer, so it is worth
holding to: one newline, only where there is none, and only at the
end of the buffer."
  (with-temp-buffer
    (insert "# %%
x = 1")                ; no newline at the end
    (python-mode)
    (code-cells-mode)
    (goto-char (point-min))
    (pcase-let ((`(,beg ,end) (code-cells--bounds nil nil t)))
      (pycell--show beg end "42" 0.1))
    (should (equal (buffer-string) "# %%
x = 1
"))
    (should (pycell--overlays (point-min) (point-max))))
  ;; a buffer that ends with one is left alone
  (with-temp-buffer
    (insert "# %%
x = 1
")
    (python-mode)
    (code-cells-mode)
    (goto-char (point-min))
    (pcase-let ((`(,beg ,end) (code-cells--bounds nil nil t)))
      (pycell--show beg end "42" 0.1))
    (should (equal (buffer-string) "# %%
x = 1
"))))

(ert-deftest pycell-test-fit-caps-an-image ()
  "An image drawn inline is capped to a share of the window.
A block taller than the window bounces the wheel backwards off itself
and cannot be scrolled past at all."
  (let ((buffer (get-buffer-create "*pycell test fit*")))
    (unwind-protect
        (with-current-buffer buffer
          (set-window-buffer (selected-window) buffer)
          (let ((line (concat "x" pycell-test--image)))
            (let* ((pycell-max-image-height 0.5)
                   (fitted (pycell--fit line)))
              (should (= (plist-get (cdr (pycell--image fitted)) :max-height)
                         (round (* 0.5 (window-body-height
                                        (selected-window) t)))))
              ;; the line kept for the popup is not touched
              (should-not (plist-get (cdr (pycell--image line)) :max-height)))
            ;; zero draws it at its own size
            (let* ((pycell-max-image-height 0)
                   (fitted (pycell--fit line)))
              (should-not (plist-get (cdr (pycell--image fitted))
                                     :max-height)))))
      (kill-buffer buffer))))

(ert-deftest pycell-test-fit-caps-a-buried-notebook ()
  "A cell that finishes while its notebook is elsewhere is capped too.
A run of all cells works down the notebook while the user reads
something else, and no window at all would leave the figure at full
size, which is the block the wheel cannot get past."
  (let ((elsewhere (get-buffer-create "*pycell test elsewhere*"))
        (notebook (get-buffer-create "*pycell test notebook*")))
    (unwind-protect
        (progn
          (set-window-buffer (selected-window) elsewhere)
          (with-current-buffer notebook
            (let* ((pycell-max-image-height 0.5)
                   (line (concat "x" pycell-test--image))
                   (fitted (pycell--fit line)))
              (should-not (get-buffer-window notebook t))
              (should (= (plist-get (cdr (pycell--image fitted)) :max-height)
                         (round (* 0.5 (window-body-height
                                        (selected-window) t))))))))
      (kill-buffer elsewhere)
      (kill-buffer notebook))))

(ert-deftest pycell-test-md-commit-keeps-an-empty-cell-empty ()
  "Committing an empty cell writes nothing where there was nothing.
An empty text has no line to comment, and `pycell--md-comment' turns
it into a bare #, so a commit that changed nothing changed the file."
  (skip-unless (pycell--md-program))
  (let ((text "# %% [markdown]\n\n# %%\nx = 1\n")
        (notebook (generate-new-buffer "*pycell test notebook*"))
        edit)
    (unwind-protect
        (progn
          (with-current-buffer notebook
            (insert text)
            (python-mode)
            (code-cells-mode)
            (pycell-md-render-all)
            (goto-char (point-min))
            (forward-line 1)
            (let ((prefix (format "*pycell md: %s:" (buffer-name))))
              (save-window-excursion (pycell-md-edit))
              (setq edit (seq-find (lambda (b)
                                     (string-prefix-p prefix (buffer-name b)))
                                   (buffer-list)))))
          (should edit)
          ;; `pycell-md-commit' ends by quitting its window, and the
          ;; edit buffer is not displayed here, so that would kill
          ;; whatever the selected window holds — and the next test
          ;; would find a marker into a dead buffer.  The round trip
          ;; is what this checks.
          (cl-letf (((symbol-function 'quit-window) #'ignore))
            (with-current-buffer edit (pycell-md-commit)))
          (with-current-buffer notebook
            (should (equal (buffer-substring-no-properties (point-min) (point-max))
                           text))))
      (when (buffer-live-p edit) (kill-buffer edit))
      (kill-buffer notebook))))

(ert-deftest pycell-test-md-edit-keeps-another-cell-s-edit ()
  "Opening a second cell\='s edit leaves the first one\='s text alone.
One edit buffer for the whole file wrote the cell opened second over
the cell opened first, and unsaved writing went with it."
  (skip-unless (pycell--md-program))
  (let ((notebook (generate-new-buffer "*pycell test notebook*"))
        first second)
    (unwind-protect
        (with-current-buffer notebook
          (insert "# %% [markdown]\n# First cell.\n\n"
                  "# %% [markdown]\n# Second cell.\n")
          (python-mode)
          (code-cells-mode)
          (pycell-md-render-all)
          (goto-char (point-min))
          (forward-line 1)
          (save-window-excursion (pycell-md-edit))
          (setq first (format "*pycell md: %s:2*" (buffer-name)))
          (setq second (format "*pycell md: %s:5*" (buffer-name)))
          (with-current-buffer first
            (goto-char (point-max))
            (insert "An hour of unsaved writing."))
          (goto-char (point-min))
          (forward-line 4)
          (save-window-excursion (pycell-md-edit))
          (should (get-buffer second))
          (should (string-match-p "Second cell"
                                  (with-current-buffer second (buffer-string))))
          (should (string-match-p "An hour of unsaved writing"
                                  (with-current-buffer first (buffer-string))))
          ;; and coming back to a cell being edited returns the edit,
          ;; rather than the text the file still holds
          (goto-char (point-min))
          (forward-line 1)
          (save-window-excursion (pycell-md-edit))
          (should (string-match-p "An hour of unsaved writing"
                                  (with-current-buffer first (buffer-string)))))
      (dolist (name (list first second))
        (when (and name (get-buffer name)) (kill-buffer name)))
      (kill-buffer notebook))))

(ert-deftest pycell-test-mirror-reads-only-what-it-shows ()
  "The live mirror reads the head of the output, not all of it.
Reading everything again on every tick is a pass over the whole
output five times a second, which grows with the cell: 25ms a tick at
the start of a sixty thousand line cell and 101ms at its end,
measured against ipython, where the bounded mirror stays at 1ms
throughout."
  (let ((pycell-max-lines 4))
    (with-temp-buffer
      (setq-local comint-prompt-regexp "^In \\[[0-9]+\\]: ")
      (let ((from (point-max-marker)))
        (setq-local pycell--run (list from nil nil "" 0.0 nil nil nil))
        (insert (mapconcat (lambda (i) (format "line %d" i))
                           (number-sequence 1 200) "\n")
                "\n")
        ;; the head holds what shows and a little slack, not the rest
        (let ((head (pycell--head from)))
          (should (string-prefix-p "line 1\nline 2" head))
          (should-not (string-match-p "line 100" head))
          (should (< (length head) 100)))
        ;; and it is kept, so the ticks that follow read nothing
        (should (equal (nth 6 pycell--run) (pycell--head from)))
        ;; the count is the whole output all the same, and counted
        ;; only where it arrives: the position it reached is kept
        (should (= (pycell--total from) 200))
        (should (= (car (nth 7 pycell--run)) (point-max)))
        (insert "line 201\nline 202")
        (should (= (pycell--total from) 202))))))

(ert-deftest pycell-test-mirror-keeps-nothing-while-it-has-nothing ()
  "An empty head is not kept, so the text can still arrive.
An escape sequence that has not arrived in full swallows everything
after it until it does, and comint-mime renders it only when it is
complete."
  (let ((pycell-max-lines 2))
    (with-temp-buffer
      (setq-local comint-prompt-regexp "^In \\[[0-9]+\\]: ")
      (let ((from (point-max-marker)))
        (setq-local pycell--run (list from nil nil "" 0.0 nil nil nil))
        (insert "\e]5151;{\"image/png\"\n")
        (insert (mapconcat (lambda (i) (format "line %d" i))
                           (number-sequence 1 20) "\n")
                "\n")
        (should (equal (pycell--head from) ""))
        (should-not (nth 6 pycell--run))))))

(ert-deftest pycell-test-md-htmls-gives-one-piece-per-cell ()
  "The cells come back from one converter call, one piece each."
  (skip-unless (pycell--md-program))
  (let ((htmls (pycell--md-htmls '("# One\n\nfirst" "second" "*third*"))))
    (should (= (length htmls) 3))
    (should (string-match-p "first" (nth 0 htmls)))
    (should (string-match-p "second" (nth 1 htmls)))
    (should (string-match-p "third" (nth 2 htmls)))
    ;; nothing of one cell leaks into the next
    (should-not (string-match-p "second" (nth 0 htmls))))
  ;; a cell that holds the marker sends everyone the ordinary way
  (should-not (pycell--md-htmls (list "text" pycell--md-marker))))

(ert-deftest pycell-test-md-htmls-gives-up-when-the-marker-changes ()
  "A converter that reshapes the marker sends every cell its own way.
The batch is only safe while the pieces come back one to a cell, and
nothing but their number says whether they did."
  (cl-letf (((symbol-function 'pycell--md-html)
             (lambda (_md) "<h1>one</h1>\n<h1>two</h1>")))
    (should-not (pycell--md-htmls '("one" "two")))))

(ert-deftest pycell-test-md-render-all-matches-one-by-one ()
  "Converting the buffer at once renders what one call per cell does."
  (skip-unless (pycell--md-program))
  (let ((buffer (generate-new-buffer "*pycell test notebook*"))
        (displays (lambda ()
                    (mapcar (lambda (ov)
                              (mapconcat (lambda (part)
                                           (or (overlay-get part 'display) ""))
                                         (overlay-get ov 'pycell-parts) "|"))
                            (seq-filter (lambda (ov) (overlay-get ov 'pycell-parts))
                                        (pycell--overlays (point-min) (point-max)
                                                          'pycell-md))))))
    (unwind-protect
        (with-current-buffer buffer
          (dotimes (i 3)
            (insert (format "# %%%% [markdown]\n# ## Section %d\n#\n# Prose *here*.\n\n# %%%%\nx%d = %d\n\n" i i i)))
          (python-mode)
          (code-cells-mode)
          (pycell-md-render-all)
          (let ((batched (funcall displays)))
            (should (= (length batched) 3))
            (pycell-md-unrender)
            ;; the same buffer with the batch turned down
            (cl-letf (((symbol-function 'pycell--md-htmls) (lambda (_texts) nil)))
              (pycell-md-render-all))
            (should (equal batched (funcall displays)))))
      (kill-buffer buffer))))

(ert-deftest pycell-test-md-boundary-shapes ()
  "Every boundary `code-cells' takes as markdown is taken as markdown.
VS Code and Spyder write =#%% [markdown]= where jupytext writes
=# %% [markdown]=, and a tag list or a title may follow either."
  (with-temp-buffer
    (python-mode)
    (dolist (line '("# %% [markdown]"
                    "#%% [markdown]"
                    "## %% [markdown]"
                    "#  %%  [markdown]"
                    "# %% [markdown] tags=[\"note\"]"
                    "# %% [markdown] The heading of the cell"))
      (erase-buffer)
      (insert line "\n# prose\n")
      (goto-char (point-min))
      (forward-line 1)
      (should (pycell--md-head (point))))
    ;; and a code cell is not one, whatever it is called
    (dolist (line '("# %%" "#%%" "# %% A title" "# %% tags=[\"parameters\"]"))
      (erase-buffer)
      (insert line "\nx = 1\n")
      (goto-char (point-min))
      (forward-line 1)
      (should-not (pycell--md-head (point))))))

(ert-deftest pycell-test-md-no-previews-without-images ()
  "A display that cannot draw images gets no preview substitution.
One image in the rendered text costs the cell its piece-per-line
scrolling, and a terminal cannot even show it — so where
`display-images-p\=' says no, the fragments stay text, untouched."
  (cl-letf (((symbol-function 'display-images-p) #'ignore)
            ;; A LaTeX that would succeed, to prove it is never asked.
            ((symbol-function 'pycell--md-latex-image)
             (lambda (&rest _) (error "the terminal asked for an image"))))
    (let ((text "before $x^2$ after"))
      (should (equal (pycell--md-mathify text) text)))))

(ert-deftest pycell-test-md-verbatim-math-keeps-lines ()
  "Display math keeps its line structure, whatever the display draws.
shr fills paragraphs, so a $$ block is wrapped in <pre> before the
converter.  It is wrapped on a display that draws images as well: a
frame can draw one and still have no LaTeX to make it with, and a
fragment LaTeX cannot compile stays text anywhere."
  (let ((md "prose\n$$\na &= b \\\\\nc &= d\n$$\nmore"))
    (dolist (images (list #'ignore (lambda (&rest _) t)))
      (cl-letf (((symbol-function 'display-images-p) images))
        (should (string-search "<pre>$$\na &= b"
                               (pycell--md-verbatim-math md)))))))

(ert-deftest pycell-test-md-a-wrapped-block-still-gets-its-preview ()
  "A block that keeps its lines is still replaced by one preview.
The fragment is matched across its lines, so the wrapping in <pre>
costs the preview nothing."
  (skip-unless (pycell--md-program))
  (cl-letf (((symbol-function 'display-images-p) (lambda (&rest _) t))
            ((symbol-function 'pycell--md-latex-image)
             (lambda (&rest _) '(image :type png :data "x"))))
    (let ((rendered (pycell--md-rendered "prose\n\n$$\na = b\n$$\n")))
      (should (string-match-p "a = b" (substring-no-properties rendered)))
      ;; one image over the whole block, and the prose untouched
      (should (eq (car-safe (get-text-property
                             (string-match "\\$\\$" rendered) 'display
                             rendered))
                  'image)))))

(ert-deftest pycell-test-md-table-columns-are-literal ()
  "A rendered table aligns with real spaces, not display specs.
shr\='s `:align-to\=' counts from the line\='s visual start, and a cell is
shown indented, so only literal columns survive.  The second row\='s
cells must start where the header\='s do."
  (skip-unless (pycell--md-program))
  (let* ((rendered (pycell--md-rendered
                    "| node | form |\n|------|------|\n| X1 | h1 |\n"))
         (lines (split-string (substring-no-properties rendered) "\n"))
         (header (seq-find (lambda (l) (string-search "node" l)) lines))
         (row (seq-find (lambda (l) (string-search "X1" l)) lines)))
    (should header)
    (should row)
    (should-not (text-property-not-all 0 (length rendered)
                                       'display nil rendered))
    (should (= (string-search "form" header)
               (string-search "h1" row)))))

(ert-deftest pycell-test-bar-slack-on-a-terminal ()
  "The bar leaves a terminal one column of slack.
A bar that runs into the last column makes the line a continuation,
and the final icon wraps onto a line of its own."
  (cl-letf (((symbol-function 'display-graphic-p) #'ignore))
    (let* ((bar (pycell--bar "label" "^  x "))
           (spec (get-text-property
                  (next-single-property-change 0 'display bar)
                  'display bar)))
      (should (equal spec
                     `(space :align-to
                             (- right (,(+ (string-pixel-width
                                            (propertize "^  x " 'face
                                                        'pycell-header))
                                           1)))))))))

(ert-deftest pycell-test-md-parts-carry-an-image ()
  "A piece with an image rides the after-string, the others a display.
Display properties do not nest, so a piece with an image in a display
string would lose it.  Hiding the line with a display string of
nothing and hanging the piece on the after-string keeps the image and
the line, and a cell with a preview then scrolls a line at a time
like any other."
  (with-temp-buffer
    (insert "one\ntwo\nthree\n")
    (let* ((image '(image :type png :data "x"))
           (text (concat "plain piece\n"
                         "piece with " (propertize " " 'display image) "\n"
                         "plain again"))
           (parts (pycell--md-parts (point-min) (point-max) text))
           (specs (mapcar (lambda (ov)
                            (list (overlay-get ov 'display)
                                  (overlay-get ov 'after-string)))
                          parts)))
      (should (= (length parts) 3))
      ;; the first and the last carry their text as a display string
      (should (equal (nth 0 specs) '("plain piece" nil)))
      (should (equal (nth 2 specs) '("plain again" nil)))
      ;; the middle one hides its line and shows the image beside it
      (should (equal (car (nth 1 specs)) ""))
      (should (pycell--image (cadr (nth 1 specs)))))))

(ert-deftest pycell-test-md-a-header-cell-and-code-have-a-face ()
  "A header cell is bold and inline code wears the face of code.
shr has no function for a =th=, and it draws code in a fixed pitch
face, which says nothing where the rendering runs with
`shr-use-fonts\=' nil."
  (skip-unless (pycell--md-program))
  (let* ((rendered (pycell--md-rendered
                    "| head | x |\n|------|---|\n| `code_here` | y |\n"))
         (faces (lambda (word)
                  (let ((at (string-search word rendered)))
                    (and at (get-text-property at 'face rendered))))))
    (should (memq 'bold (ensure-list (funcall faces "head"))))
    (should (memq 'pycell-md-code (ensure-list (funcall faces "code_here"))))))

(ert-deftest pycell-test-md-math-in-a-table-stays-text ()
  "A formula in a table cell keeps its text, so the columns hold.
A preview image is never as wide as the text it replaces, and a table
is padded for the text.  Outside a table the same formula becomes an
image."
  (skip-unless (pycell--md-program))
  (cl-letf (((symbol-function 'display-images-p) (lambda (&rest _) t))
            ((symbol-function 'pycell--md-latex-image)
             (lambda (_frag) '(image :type png :data "x"))))
    ;; A formula the converter cannot render itself is the one that
    ;; reaches this package: pandoc renders simple math as text.
    (let ((in-table (pycell--md-rendered
                     "| a | b |\n|---|---|\n| $\\frac{a}{b}$ | y |\n"))
          (outside (pycell--md-rendered
                    "The formula $\\frac{a}{b}$ stands alone.\n")))
      (should-not (pycell--image in-table))
      (should (pycell--image outside)))))

(ert-deftest pycell-test-space-columns ()
  "A space stretch answers with the columns it covers.
vtable, which is how comint-mime shows a DataFrame, sets the width of
a stretch; shr says where it ends.  A list counts pixels, a bare
number characters."
  (cl-letf (((symbol-function 'frame-char-width) (lambda (&rest _) 8)))
    ;; a width in pixels, and one that is not a whole character
    (should (= (pycell--space-columns '(space :width (16)) 0) 2))
    (should (= (pycell--space-columns '(space :width (5.5)) 0) 1))
    ;; a width in characters
    (should (= (pycell--space-columns '(space :width 3) 0) 3))
    ;; a target counts from where the line starts
    (should (= (pycell--space-columns '(space :align-to (104)) 3) 10))
    ;; nothing to say about a stretch of another kind
    (should-not (pycell--space-columns '(space :relative-width 2) 0))))

(ert-deftest pycell-test-clean-flattens-a-copied-table ()
  "A copied vtable gets literal columns and no dead bindings.
comint-mime renders a DataFrame as a vtable in the shell buffer, which
aligns with pixel targets measured for that window and carries the
keymap of a live table.  The block shows a copy: the targets land
elsewhere, and no binding can find a table to sort."
  (let* ((comint-prompt-regexp "^In \\[[0-9]+\\]: ")
         (cell (propertize "alpha" 'keymap (make-sparse-keymap)
                           'mouse-face 'highlight
                           'help-echo "Click to sort"))
         (gap (propertize " " 'display '(space :align-to (104))))
         (clean (pycell--clean (concat cell gap "beta"))))
    ;; the stretch is gone, and real spaces stand in its place
    (should-not (text-property-not-all 0 (length clean) 'display nil clean))
    (should (string-match-p "\\`alpha +beta\\'" (substring-no-properties clean)))
    ;; and nothing promises a click any more
    (dolist (prop '(keymap local-map mouse-face help-echo))
      (should-not (text-property-not-all 0 (length clean) prop nil clean)))))

(ert-deftest pycell-test-buttons-come-from-the-option ()
  "The header shows the buttons of the option, in its order.
A descriptor whose WHEN is `image\=' or `lines\=' waits for those."
  (let ((descriptors '((one ("1") "first" ignore t)
                       (two ("2") "second" ignore lines)
                       (three ("3") "third" ignore image))))
    (should (equal (substring-no-properties
                    (pycell--buttons descriptors nil 0))
                   "1 "))
    (should (equal (substring-no-properties
                    (pycell--buttons descriptors nil 3))
                   "1  2 "))
    (should (equal (substring-no-properties
                    (pycell--buttons descriptors t 3))
                   "1  2  3 "))
    ;; the order is the order of the list
    (should (equal (substring-no-properties
                    (pycell--buttons (reverse descriptors) t 3))
                   "3  2  1 "))
    ;; and a button carries its command and its tooltip
    (let ((row (pycell--buttons descriptors nil 0)))
      (should (equal (get-text-property 0 'help-echo row) "first")))))

(ert-deftest pycell-test-move-cell-carries-its-result ()
  "A cell that moves takes its result with it, and point comes along.
`transpose-regions' leaves an overlay where the text used to be, so
the block of one cell would end up under the other."
  (with-temp-buffer
    (insert "# %%\nfirst = 1\n\n# %%\nsecond = 2\n\n# %%\nthird = 3\n")
    (python-mode)
    (code-cells-mode)
    ;; a result on the first two cells
    (goto-char (point-min))
    (pcase-let ((`(,beg ,end) (code-cells--bounds nil nil t)))
      (pycell--show beg end "one" 0.1))
    (goto-char (point-min))
    (forward-line 3)
    (pcase-let ((`(,beg ,end) (code-cells--bounds nil nil t)))
      (pycell--show beg end "two" 0.2))
    ;; move the first cell down, from inside it
    (goto-char (point-min))
    (forward-line 1)
    (let ((column (- (point) (pos-bol))))
      (pycell-move-cell-down 1)
      ;; the text swapped
      (should (string-match-p "\\`# %%\nsecond = 2\n\n# %%\nfirst = 1\n"
                              (buffer-substring-no-properties (point-min)
                                                              (point-max))))
      ;; point is in the cell that moved, at the same offset
      (should (string-prefix-p "first = 1"
                              (buffer-substring-no-properties
                               (pos-bol) (pos-eol))))
      (should (= (- (point) (pos-bol)) column)))
    ;; each result is on its own cell again
    (let ((texts (mapcar #'pycell--text
                         (pycell--overlays (point-min) (point-max)))))
      (should (equal (sort (copy-sequence texts) #'string<) '("one" "two")))
      (goto-char (point-min))
      (pcase-let ((`(,beg ,end) (code-cells--bounds nil nil t)))
        (should (equal (pycell--text (car (pycell--overlays beg end)))
                       "two"))))))

(ert-deftest pycell-test-move-cell-keeps-a-rendered-markdown-cell ()
  "A rendered markdown cell moves whole, marker and rendering.
Its pieces hang on its source lines, and the lines move under them."
  (skip-unless (pycell--md-program))
  (with-temp-buffer
    (insert "# %%\nx = 1\n\n# %% [markdown]\n# ## Prose\n#\n# Words here.\n")
    (python-mode)
    (code-cells-mode)
    (pycell-md-render-all)
    ;; the markdown cell is the second one; move it up
    (goto-char (point-min))
    (forward-line 4)
    (pycell-move-cell-up 1)
    (let ((text (buffer-substring-no-properties (point-min) (point-max))))
      ;; the marker travelled with the cell
      (should (string-prefix-p "# %% [markdown]\n# ## Prose\n" text))
      (should (string-match-p "# %%\nx = 1\n" text)))
    ;; the rendering sits on the cell, which is now the first one
    (let ((rendered (pycell--overlays (point-min) (point-max) 'pycell-md)))
      (should rendered)
      (should (< (overlay-start (car rendered))
                 (save-excursion (goto-char (point-min))
                                 (forward-line 4)
                                 (point)))))))

(ert-deftest pycell-test-move-cell-stops-at-the-ends ()
  "The first cell cannot move up and the last cannot move down.
`code-cells-move-cell-down' says so, and nothing is taken off before
it has said it."
  (with-temp-buffer
    (insert "# %%\nfirst = 1\n\n# %%\nsecond = 2\n")
    (python-mode)
    (code-cells-mode)
    (goto-char (point-min))
    (pcase-let ((`(,beg ,end) (code-cells--bounds nil nil t)))
      (pycell--show beg end "one" 0.1))
    (let ((before (buffer-substring-no-properties (point-min) (point-max))))
      (goto-char (point-min))
      (forward-line 1)
      (should-error (pycell-move-cell-up 1) :type 'user-error)
      ;; the buffer and the result are untouched
      (should (equal (buffer-substring-no-properties (point-min) (point-max))
                     before))
      (goto-char (point-min))
      (pcase-let ((`(,beg ,end) (code-cells--bounds nil nil t)))
        (should (equal (pycell--text (car (pycell--overlays beg end)))
                       "one"))))))

(ert-deftest pycell-test-md-parts-keep-a-multiline-image-whole ()
  "An image run that covers several lines becomes one piece.
Display math renders as three lines under one image run, and a piece
for each of them drew the same image three times."
  (let* ((image '(image :type png :data "x"))
         (block (propertize "$$\na = b\n$$" 'display image))
         (text (concat "before\n" block "\nafter")))
    ;; three pieces: the prose, the whole block, the prose
    (should (equal (mapcar #'substring-no-properties (pycell--md-lines text))
                   '("before" "$$\na = b\n$$" "after")))
    (with-temp-buffer
      (insert "one\ntwo\nthree\nfour\nfive\n")
      (let* ((parts (pycell--md-parts (point-min) (point-max) text))
             (withimage (seq-filter
                         (lambda (ov)
                           (pycell--image (or (overlay-get ov 'after-string)
                                              (overlay-get ov 'display) "")))
                         parts)))
        ;; the image is on one piece, and only one
        (should (= (length withimage) 1))
        (should (equal (substring-no-properties
                        (overlay-get (car withimage) 'after-string))
                       "$$\na = b\n$$"))))))

(defun pycell-test--vtable-text ()
  "Return the text of a vtable, as comint-mime leaves one in the shell."
  (with-temp-buffer
    (make-vtable
     :use-header-line nil
     :columns (mapcar (lambda (name) (list :name name
                                           :min-width (length name)
                                           :align 'right))
                      '("alpha" "beta_longer" "gamma"))
     :objects '(("1" "22" "333") ("4444" "5" "66") ("7" "888" "9999")))
    (buffer-string)))

(ert-deftest pycell-test-table-is-laid-out-in-characters ()
  "A copied table gets columns that no face can move.
A vtable aligns with stretches of pixels measured in the window that
drew it, and it measures a header cell in the face of a header: a copy
shown in another face had the header squashed and the rows apart."
  (skip-unless (fboundp 'make-vtable))
  (let* ((comint-prompt-regexp "^In \\[[0-9]+\\]: ")
         (clean (pycell--clean (pycell-test--vtable-text)))
         (lines (split-string (substring-no-properties clean) "\n")))
    ;; the header and one column start at the same place on every row
    (should (= (length lines) 4))
    (let ((column (string-search "beta_longer" (car lines))))
      (should column)
      (dolist (line (cdr lines))
        (should (eq (string-match-p "[0-9]" line column) column))))
    ;; the names of the columns stand out
    (should (memq 'bold (ensure-list (get-text-property 0 'face clean))))
    ;; and no stretch is left to drift
    (should-not (text-property-not-all 0 (length clean) 'display nil clean))))

(ert-deftest pycell-test-table-pops-as-a-live-table ()
  "The pop of a table gives a table that sorts, not a picture of one.
A copy carries the table object, and vtable draws it again for the
window it lands in."
  (skip-unless (fboundp 'make-vtable))
  (pycell-test--with-cells
    (let* ((comint-prompt-regexp "^In \\[[0-9]+\\]: ")
           (text (pycell--clean (pycell-test--vtable-text))))
      (pcase-let ((`(,beg ,end) (code-cells--bounds nil nil t)))
        (pycell--show beg end text 0.4))
      (let ((ov (car (pycell--overlays (point-min) (point-max)))))
        (goto-char (overlay-start ov))
        (save-window-excursion (pycell-pop-output))
        (with-current-buffer (pycell--cell-buffer-name nil (overlay-start ov))
          (goto-char (point-min))
          (should (vtable-current-table))
          (should (equal (mapcar #'vtable-column-name
                                 (vtable-columns (vtable-current-table)))
                         '("alpha" "beta_longer" "gamma")))
          ;; A copy carries the table object as well, so the object says
          ;; nothing about whether the table works.  A drawn table knows
          ;; which column is under point and can sort by it; a copy of
          ;; the text of one cannot.
          (forward-line 1)
          (should (vtable-current-column))
          (should (equal (vtable-current-object) '("1" "22" "333")))
          (let ((inhibit-read-only t))
            (vtable-sort-by-current-column))
          (goto-char (point-min))
          (forward-line 1)
          (should (equal (vtable-current-object) '("1" "22" "333"))))))))

(provide 'pycell-test)
;;; pycell-test.el ends here
