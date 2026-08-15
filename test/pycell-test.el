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
            (let ((name (format "*pycell md: %s*" (buffer-name))))
              ;; `pycell-md-edit' pops to its buffer, which leaves that
              ;; buffer current for the rest of this form.
              (save-window-excursion (pycell-md-edit))
              (setq edit (get-buffer name))))
          (should edit)
          (with-current-buffer edit (pycell-md-commit))
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
The block of such a cell is a string on the main overlay, and only a
cell followed by a newline has a body overlay as well.  Asking for
the body first skipped the last cell of a buffer that ends without a
newline, and its figure stayed on screen below the fold."
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
             (last (overlay-get (car (last cells)) 'pycell-main)))
        (should last)
        ;; the image took the string road, so there is one to hide
        (should (> (length (or (overlay-get last 'after-string) "")) 0))
        (outline-flag-region (point-min) (point-max) t)
        (should (= (length (or (overlay-get last 'after-string) "")) 0))
        (outline-flag-region (point-min) (point-max) nil)
        (should (> (length (or (overlay-get last 'after-string) "")) 0))))))

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

(provide 'pycell-test)
;;; pycell-test.el ends here
