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

(defmacro pycell-test--with-notebook (text &rest body)
  "Evaluate BODY in a Python buffer holding TEXT, with the mode on.
The buffer is shown in a window: a bar is cut to the width of the
windows that show it, and a command that follows a click selects one."
  (declare (indent 1))
  `(with-temp-buffer
     (insert ,text)
     (python-mode)
     (set-window-buffer nil (current-buffer))
     (code-cells-mode)
     (pycell-mode)
     (goto-char (point-min))
     (unwind-protect (progn ,@body)
       (pycell-mode -1))))

(defun pycell-test--bar-texts ()
  "Return the whole text of every code cell bar of the buffer, in order."
  (mapcar (lambda (ov)
            (substring-no-properties (or (overlay-get ov 'overblock-bar-text)
                                         "")))
          (sort (seq-filter (lambda (ov)
                              (eq (overlay-get ov 'overblock-bar) 'code))
                            (overblock-bars))
                (lambda (a b) (< (overlay-start a) (overlay-start b))))))

(defun pycell-test--bar-labels ()
  "Return the label of every code cell bar of the buffer, in order.
The bars of rendered markdown cells are left out: whether a cell renders
at all depends on a converter being installed.
The label is what stands before the stretch that holds the icons out at
the window edge, less the glyph that leads it."
  (mapcar (lambda (ov)
            (let* ((text (or (overlay-get ov 'overblock-bar-text) ""))
                   (stretch (text-property-not-all 0 (length text)
                                                   'display nil text))
                   (label (substring-no-properties text 0 stretch)))
              (string-trim (substring label (1+ (string-search " " label))))))
          ;; Not `sort' with keywords: that calling convention is
          ;; Emacs 30, and this package declares 29.1.
          (sort (seq-filter (lambda (ov)
                              (eq (overlay-get ov 'overblock-bar) 'code))
                            (overblock-bars))
                (lambda (a b) (< (overlay-start a) (overlay-start b))))))

;;;; Helpers

(defun overblock-bar-on-line ()
  "Return the bar overlay of the line point is on, or nil.
A helper of these tests: the package itself asks `overblock-bar-in'
for a region, and shipping this as public API gave it a name nothing
outside the suite ever called."
  (overblock-bar-in (pos-bol) (min (point-max) (1+ (pos-eol)))))

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
    (should (overblock-image-in result))
    ;; a prompt with nothing to show before it still goes
    (should (equal (pycell--clean "In [5]: 42") "42"))))

(ert-deftest pycell-test-clean-keeps-images ()
  "Whitespace that carries a display property is part of the result."
  (let* ((comint-prompt-regexp "^>>> ")
         (result (pycell--clean (concat "plot\n" pycell-test--image "\n"))))
    (should (equal result (concat "plot\n" pycell-test--image)))
    (should (get-text-property (1- (length result)) 'display result))))

;;;; Tests

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
would hide the rest of the output and buy no height back.  The image is
named where it cannot be drawn: the space alone was a blank row, and a
reader could not tell it from a result with no output."
  (cl-letf (((symbol-function 'display-images-p) (lambda (&rest _) nil)))
    (let ((pycell-max-lines 10))
      (should (equal (pycell--body-lines
                      (list "before" pycell-test--image "after"))
                     (list "before" "[figure]" "after"))))))

(ert-deftest pycell-test-md-an-edit-takes-the-bar-with-it ()
  "An edit of a rendered cell removes the rendering and its bar.
The block evaporates with the text it covers, and the bar sits on the
boundary line above, where no edit of the cell reaches it: without the
modification hook it stayed behind, and `pycell-md-commit' drew a
second bar beside it."
  (skip-unless (overblock-md-program))
  (with-temp-buffer
    (insert "# %% [markdown]\n# ## A\n#\n# Text.\n\n# %%\nx = 1\n")
    (python-mode)
    (code-cells-mode)
    (let ((bars (lambda ()
                  (seq-count (lambda (ov) (overlay-get ov 'pycell-main))
                             (overlays-in (point-min) (point-max))))))
      (pycell-md-render-all)
      (should (= (funcall bars) 1))
      (pcase-let* ((block (car (overblock-in (point-min) (point-max)
                                             'markdown)))
                   (`(,beg . ,end) (overblock-get block :data)))
        (goto-char beg)
        (delete-region beg end)
        (insert "# ## A\n#\n# Text and more.\n\n")
        (should-not (overblock-in (point-min) (point-max) 'markdown))
        (should (= (funcall bars) 0))
        ;; and rendering again leaves one bar, not two
        (pycell--md-show beg (point))
        (should (= (funcall bars) 1))))))

(ert-deftest pycell-test-show-text-result ()
  "A text result rides the newline, and the buffer text stays as it was.
The body is a display string on the newline that ends the cell, where
plain text costs least; the header is a string on the anchor, because a
bar puts its icons at the window edge and a display property cannot."
  (pycell-test--with-cells
    (let ((before (buffer-substring-no-properties (point-min) (point-max))))
      (pcase-let ((`(,beg ,end) (code-cells--bounds nil nil t)))
        (pycell--show beg end "42" 0.5)
        (let* ((block (car (overblock-in (point-min) (point-max) 'result)))
               (nl (overblock-get block :newline)))
          (should block)
          ;; the body is the display string of the newline, the cheap slot
          (should (string-match-p "42" (overlay-get nl 'display)))
          ;; the header is a string on the anchor: a bar draws its icons
          ;; at the window edge, which a display property cannot, and it
          ;; costs less there than on the newline
          (should (string-match-p "line" (overlay-get block 'after-string)))))
      (should (equal (buffer-substring-no-properties (point-min) (point-max))
                     before)))))

(ert-deftest pycell-test-show-image-result ()
  "An image result rides the after-string of the anchor, images and all.
A display string swallows an image, so those rows go into a string; the
newline keeps its own character, and a wheel can pass the block.

Only a display that draws images gets that far, so this says so: on a
terminal the figure is named instead and the body takes the cheap
display property, which is the whole point of asking."
  (cl-letf (((symbol-function 'display-images-p) (lambda (&rest _) t)))
    (pycell-test--with-cells
      (pcase-let ((`(,beg ,end) (code-cells--bounds nil nil t)))
        (pycell--show beg end (concat "plot\n" pycell-test--image) 0.5)
        (let* ((block (car (overblock-in (point-min) (point-max) 'result)))
               (nl (overblock-get block :newline)))
          (should (overblock-image-in (overlay-get block 'after-string)))
          (should-not (overlay-get nl 'display)))))))

(ert-deftest pycell-test-a-figure-is-named-in-a-terminal ()
  "A figure a terminal cannot draw is named, in the block and out of it.
comint-mime sends one space carrying the image, and a display that
draws none shows the space: the row was blank, and `pycell-pop-output'
gave a buffer holding that one space."
  (cl-letf (((symbol-function 'display-images-p) (lambda (&rest _) nil)))
    (pycell-test--with-cells
      (pcase-let ((`(,beg ,end) (code-cells--bounds nil nil t)))
        (pycell--show beg end (concat "plot\n" pycell-test--image) 0.5)
        (let ((block (car (overblock-in (point-min) (point-max) 'result))))
          (should (string-match-p
                   "\\[figure\\]"
                   (concat (overlay-get block 'after-string)
                           (overlay-get (overblock-get block :newline)
                                        'display)))))))))

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
      (let* ((block (car (overblock-in (point-min) (point-max) 'result)))
             (nl (overblock-get block :newline)))
        ;; the cheap path: the rows ride the newline as one display string
        (should (overlay-get nl 'display))
        (should (string-match-p "mc" (overlay-get nl 'display)))))))

(ert-deftest pycell-test-body-lines-keep-raised-text ()
  "Raised text does not cut the inline part short; an image does."
  (let ((pycell-max-lines 10))
    (should (equal (pycell--body-lines
                    (list "x" (propertize "2" 'display '(raise 0.2)) "y"))
                   (list "x" (propertize "2" 'display '(raise 0.2)) "y")))))

(ert-deftest pycell-test-a-finished-result-keeps-its-count ()
  "A result counts its lines once and keeps the number.
A finished cell arrives without one, and a fold would otherwise scan the
whole output again on every keypress: measured, four folds of a result of
ten thousand lines cost 12.9 milliseconds against 0.6."
  (pycell-test--with-cells
    (pcase-let ((`(,beg ,end) (code-cells--bounds nil nil t)))
      (pycell--show beg end "one\ntwo\nthree" 0.1)
      (let ((block (car (overblock-in (point-min) (point-max) 'result))))
        ;; the count is in the record, where the header reads it
        (should (= (plist-get (overblock-get block :data) :total) 3))
        (should (string-match-p "3 lines"
                                (overlay-get block 'after-string)))
        ;; and a fold keeps it
        (pycell-toggle-output)
        (should (= (plist-get (overblock-get block :data) :total) 3))
        (should (string-match-p "3 lines"
                                (overlay-get block 'after-string)))))))

(ert-deftest pycell-test-show-keeps-fold-state ()
  "Replacing a result keeps whether it was folded."
  (pycell-test--with-cells
    (pcase-let ((`(,beg ,end) (code-cells--bounds nil nil t)))
      (pycell--show beg end "a\nb" 0.1)
      (let ((ov (car (overblock-in (point-min) (point-max) 'result))))
        (overblock-set ov :data (plist-put (overblock-get ov :data)
                                           :folded t)))
      (pycell--show beg end "c\nd" 0.2)
      (let ((ov (car (overblock-in (point-min) (point-max) 'result))))
        (should (plist-get (overblock-get ov :data) :folded))
        ;; and the result that replaced it is the new one
        (should (equal (pycell--text ov) "c\nd"))))))

(ert-deftest pycell-test-remove-overlays ()
  "Removing results takes the helper overlays with them."
  (pycell-test--with-cells
    (pcase-let ((`(,beg ,end) (code-cells--bounds nil nil t)))
      (pycell--show beg end "42" 0.1))
    (let ((bov (overblock-get
                (car (overblock-in (point-min) (point-max) 'result))
                :newline)))
      (pycell-remove-blocks)
      (should-not (overblock-in (point-min) (point-max) 'result))
      (should-not (overlay-buffer bov)))))

(ert-deftest pycell-test-fold-keeps-result ()
  "An outline fold hides the code and leaves the result in place.
The block below the fold keeps its own fold button, so the two fold
separately."
  (pycell-test--with-cells
    (pcase-let ((`(,beg ,end) (code-cells--bounds nil nil t)))
      (pycell--show beg end "a\nb" 0.1)
      (let* ((ov (car (overblock-in (point-min) (point-max) 'result)))
             (bov (overblock-get ov :newline))
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
      (let ((bov (overblock-get
                  (car (overblock-in (point-min) (point-max) 'result))
                  :newline)))
        (outline-flag-region beg (point-max) t)
        (should-not
         (seq-some (lambda (o) (and (eq (overlay-get o 'invisible) 'outline)
                                    (> (overlay-end o) (overlay-start bov))))
                   (overlays-in (overlay-start bov) (overlay-end bov))))))))

(ert-deftest pycell-test-fold-md-round-trip ()
  "An outline fold takes a markdown block along, and gives it back."
  (skip-unless (overblock-md-program))
  (with-temp-buffer
    (insert "# %% [markdown]\n# ## A\n#\n# Text here.\n\n# %%\ny = 2\n")
    (python-mode)
    (code-cells-mode)
    (pycell-md-render-all)
    (goto-char (point-min))
    (let* ((block (car (overblock-in (point-min) (point-max) 'markdown)))
           ;; A fold makes the pieces anew, so they are read again each
           ;; time rather than held on to.
           (shown (lambda ()
                    (seq-some (lambda (p) (not (overlay-get p 'invisible)))
                              (overblock-get block :parts)))))
      (should (overblock-get block :parts))
      (should (funcall shown))
      (outline-flag-region (pos-eol) (overlay-end block) t)
      (should-not (funcall shown))
      (outline-flag-region (pos-eol) (overlay-end block) nil)
      (should (funcall shown)))))

(ert-deftest pycell-test-md-keeps-its-lines ()
  "A rendered markdown cell stays as many lines as its source.
One display string for the whole cell would collapse it to one line,
and every scroll event would then lay the whole thing out again."
  (skip-unless (overblock-md-program))
  (with-temp-buffer
    (insert "# %% [markdown]\n# ## A\n#\n# Text here.\n\n# %%\ny = 2\n")
    (python-mode)
    (code-cells-mode)
    (pycell-md-render-all)
    (let* ((ov (car (overblock-in (point-min) (point-max) 'markdown)))
           (parts (overblock-get ov :parts)))
      (should (> (length parts) 1))
      (should-not (overlay-get ov 'invisible))
      (pcase-let ((`(,beg . ,_) (overblock-get ov :data)))
        (should (= (overlay-start (car parts)) (marker-position beg))))
      ;; Every piece shows text; what is left over is cloaked.
      (should (seq-every-p (lambda (p) (or (stringp (overlay-get p 'display))
                                           (overlay-get p 'overblock-cloak)))
                           parts)))))

(ert-deftest pycell-test-md-cloak-starts-on-a-newline ()
  "A hidden run starts at the end of a visible line, never at a start.
`scroll-down' answers a run that begins a line with a
beginning-of-buffer error, and a piece with nothing to show would
leave a line of no height, which stops scrolling up the same way."
  (skip-unless (overblock-md-program))
  (with-temp-buffer
    (insert "# %% [markdown]\n# ## A\n#\n#\n#\n# Text here.\n#\n#\n\n# %%\ny = 2\n")
    (python-mode)
    (code-cells-mode)
    (pycell-md-render-all)
    (let* ((ov (car (overblock-in (point-min) (point-max) 'markdown)))
           (parts (overblock-get ov :parts))
           (cloaks (seq-filter (lambda (p) (overlay-get p 'overblock-cloak)) parts)))
      (should parts)
      (should cloaks)
      (dolist (part parts)
        (if (overlay-get part 'overblock-cloak)
            (should (eq (char-after (overlay-start part)) ?\n))
          ;; A piece covers the text of its line and nothing else, so
          ;; the line keeps its own newline and its height with it.
          (should-not (string-search "\n" (buffer-substring
                                           (overlay-start part)
                                           (overlay-end part)))))))))


(ert-deftest pycell-test-md-comment-round-trip ()
  "Commenting and uncommenting a markdown cell is lossless."
  (let ((md "# Title\n\nSome *text*.\n\nMore."))
    (should (equal (pycell--md-uncomment (pycell--md-comment md)) md))
    (should (equal (pycell--md-comment "a\n\nb") "# a\n#\n# b"))))

(ert-deftest pycell-test-md-cell-start-needs-the-boundary-line ()
  "A markdown cell is recognized by the boundary line above its body."
  (with-temp-buffer
    (insert "# %% [markdown]\n# Title\n")
    (goto-char (point-min))
    (forward-line 1)
    (should (pycell--md-cell-start (point)))
    (should (= (pycell--md-cell-start (point)) (point-min))))
  (with-temp-buffer
    (insert "# %%\nx = 1\n")
    (goto-char (point-min))
    (forward-line 1)
    (should-not (pycell--md-cell-start (point)))))

(ert-deftest pycell-test-dedicated-asks-no-project-question ()
  "A shell is dedicated as the reader asked, and asks nothing.
`run-python' answers `project' by calling `project-current' with a
prompt, so a file that belongs to no project would stop a queued run
with a question.  `python-shell-get-process-name' names such a shell
the shared one, and so does this."
  (let ((python-shell-dedicated nil))
    (should-not (pycell--dedicated)))
  (let ((python-shell-dedicated 'buffer))
    (should (eq (pycell--dedicated) 'buffer)))
  (let ((python-shell-dedicated 'project))
    ;; inside a project the reader's answer stands
    (cl-letf (((symbol-function 'project-current) (lambda (&rest _) '(vc Git "/tmp/"))))
      (should (eq (pycell--dedicated) 'project)))
    ;; outside one it is the shared shell, not a question
    (cl-letf (((symbol-function 'project-current) #'ignore))
      (should-not (pycell--dedicated)))))

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
            (setq pycell--run (list :from start :tail "" :start (float-time)))
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
      (pycell--goto-event (list 'switch-frame (selected-frame)))
      (should (eq (overblock-at 'result)
                  (progn (pycell--goto-event nil)
                         (overblock-at 'result))))
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
  (skip-unless (overblock-md-program))
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
                                         (overblock-glyph "…" "...")))))
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

(ert-deftest pycell-test-md-without-a-converter-stays-plain ()
  "A markdown cell without a converter stays text, and raises nothing.
`pycell-md-render-all' asks for the program first, but evaluating a
single cell reached the converter through `overblock-md-rendered' and
called nil as a program: \"Invalid argument 3 of operation
`call-process-region'\".  The answer belongs where the program is
called, so every caller gets it."
  (let ((overblock-md-command "definitely-not-installed-42")
        (text "# %% [markdown]\n# ## A heading\n\n# %%\nx = 1\n"))
    (should-not (overblock-md-program))
    (should-not (overblock-md-rendered "## A heading"))
    (with-temp-buffer
      (insert text)
      (python-mode)
      (code-cells-mode)
      (goto-char (point-min))
      (pcase-let ((`(,beg ,end) (code-cells--bounds nil nil t)))
        (pycell-eval-region beg end))
      (should-not (overblock-in (point-min) (point-max)))
      (should (equal (buffer-substring-no-properties (point-min) (point-max))
                     text)))))

(ert-deftest pycell-test-md-program-needs-libxml ()
  "Without the parser there is no converter worth naming.
shr reads the converter's HTML with `libxml-parse-html-region', which
an Emacs built without libxml2 does not have.  Rendering signalled a
void function there, on every markdown cell, instead of saying so and
leaving the cells as text."
  (cl-letf (((symbol-function 'libxml-parse-html-region) nil))
    (should-not (fboundp 'libxml-parse-html-region))
    (should-not (overblock-md-program))
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
        (should-not (overblock-in (point-min) (point-max) 'markdown))))))

(ert-deftest pycell-test-fold-md-image-at-buffer-end ()
  "A cell with an image folds even where the buffer ends without one.
The pieces of such a cell hang on its source lines, and the fold
covers those lines but stops one character short of the last newline,
so the last piece has to be hidden along.  A cell at the end of a
buffer that ends without a newline is the case that used to keep its
figure on screen below the fold."
  (skip-unless (overblock-md-program))
  (dolist (trailing '("\n" ""))
    (with-temp-buffer
      (insert "# %% [markdown]\n# ## Prose\n#\n# Words.\n\n"
              "# %%\nx = 1\n\n"
              "# %% [markdown]\n# ## A figure\n#\n# ![pic](pic.png)" trailing)
      (python-mode)
      (code-cells-mode)
      (pycell-md-render-all)
      ;; the blocks in order; the last one holds the figure
      ;; `sort' takes its key as a keyword from Emacs 30, and this
      ;; package answers for 29 as well.
      (let* ((blocks (sort (overblock-in (point-min) (point-max) 'markdown)
                           (lambda (a b)
                             (< (overlay-start a) (overlay-start b)))))
             (last (car (last blocks)))
             ;; what the cell shows: the pieces that are not cloaks
             (shown (lambda ()
                      (seq-count (lambda (p)
                                   (and (not (overlay-get p 'overblock-cloak))
                                        (not (overlay-get p 'invisible))))
                                 (overblock-get last :parts)))))
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
    (should (overblock-in (point-min) (point-max) 'result)))
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

(ert-deftest pycell-test-md-commit-keeps-an-empty-cell-empty ()
  "Committing an empty cell writes nothing where there was nothing.
An empty text has no line to comment, and `pycell--md-comment' turns
it into a bare #, so a commit that changed nothing changed the file."
  (skip-unless (overblock-md-program))
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
  "Opening a second cell's edit leaves the first one's text alone.
One edit buffer for the whole file wrote the cell opened second over
the cell opened first, and unsaved writing went with it."
  (skip-unless (overblock-md-program))
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

(ert-deftest pycell-test-output-head-stops-at-a-budget ()
  "A cell printing much on few lines reads only what can show.
And not where an escape sequence would be cut in two: comint-mime sends
an image as one, and a cut inside it drops the figure — measured once as
a result of no characters at all, which is why the bound asks first."
  (with-temp-buffer
    (setq-local comint-prompt-regexp "^In \\[[0-9]+\\]: ")
    (insert "In [1]: ")
    (let* ((from (copy-marker (point)))
           ;; what the body can show: the lines it keeps, each cut to
           ;; the length it cuts them to
           (budget (* pycell-max-lines (1+ pycell-max-line-length))))
      (setq-local pycell--run (list :from from :beg (point-min-marker)
                                    :end (point-max-marker) :tail ""
                                    :start (float-time)))
      ;; one line, longer than the budget: the head stops at it
      (insert (make-string (* 3 budget) ?x))
      (should (= (length (pycell--output-head from)) budget))
      ;; the same output with an escape sequence inside the budget: all
      ;; of it is read, so comint-mime's image arrives whole
      (setq pycell--run (plist-put pycell--run :head nil))
      (goto-char (+ from 10))
      (insert "\e]5151;file=x\e\\")
      (should (> (length (pycell--output-head from)) budget)))))

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
        (setq-local pycell--run (list :from from :tail "" :start 0.0))
        (insert (mapconcat (lambda (i) (format "line %d" i))
                           (number-sequence 1 200) "\n")
                "\n")
        ;; the head holds what shows and a little slack, not the rest
        (let ((head (pycell--output-head from)))
          (should (string-prefix-p "line 1\nline 2" head))
          (should-not (string-match-p "line 100" head))
          (should (< (length head) 100)))
        ;; and it is kept, so the ticks that follow read nothing
        (should (equal (plist-get pycell--run :head) (pycell--output-head from)))
        ;; the count is the whole output all the same, and counted
        ;; only where it arrives: the position it reached is kept
        (should (= (pycell--total from) 200))
        (should (= (car (plist-get pycell--run :count)) (point-max)))
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
        (setq-local pycell--run (list :from from :tail "" :start 0.0))
        (insert "\e]5151;{\"image/png\"\n")
        (insert (mapconcat (lambda (i) (format "line %d" i))
                           (number-sequence 1 20) "\n")
                "\n")
        (should (equal (pycell--output-head from) ""))
        (should-not (plist-get pycell--run :head))))))

(ert-deftest pycell-test-md-render-all-matches-one-by-one ()
  "Converting the buffer at once renders what one call per cell does."
  (skip-unless (overblock-md-program))
  (let ((buffer (generate-new-buffer "*pycell test notebook*"))
        (displays (lambda ()
                    (mapcar (lambda (ov)
                              (mapconcat (lambda (part)
                                           (or (overlay-get part 'display) ""))
                                         (overblock-get ov :parts) "|"))
                            (seq-filter (lambda (ov) (overblock-get ov :parts))
                                        (overblock-in (point-min) (point-max)
                                                      'markdown))))))
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
            (cl-letf (((symbol-function 'overblock-md-html-batch) (lambda (_texts) nil)))
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
      (should (pycell--md-cell-start (point))))
    ;; and a code cell is not one, whatever it is called
    (dolist (line '("# %%" "#%%" "# %% A title" "# %% tags=[\"parameters\"]"))
      (erase-buffer)
      (insert line "\nx = 1\n")
      (goto-char (point-min))
      (forward-line 1)
      (should-not (pycell--md-cell-start (point))))))

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
                         (overblock-in (point-min) (point-max) 'result))))
      (should (equal (sort (copy-sequence texts) #'string<) '("one" "two")))
      (goto-char (point-min))
      (pcase-let ((`(,beg ,end) (code-cells--bounds nil nil t)))
        (should (equal (pycell--text (car (overblock-in beg end 'result)))
                       "two"))))))

(ert-deftest pycell-test-move-cell-keeps-a-rendered-markdown-cell ()
  "A rendered markdown cell moves whole, marker and rendering.
Its pieces hang on its source lines, and the lines move under them."
  (skip-unless (overblock-md-program))
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
    (let ((rendered (overblock-in (point-min) (point-max) 'markdown)))
      (should rendered)
      (should (< (overlay-start (car rendered))
                 (save-excursion (goto-char (point-min))
                                 (forward-line 4)
                                 (point)))))))

(ert-deftest pycell-test-move-cell-carries-a-cell-that-holds-a-def ()
  "A cell moves whole, from anywhere inside it, defs and all.
`code-cells-mode' takes the major mode's own headings into
`outline-regexp', so a `def' in a cell is an outline heading too:
`outline-back-to-heading' from inside one finds the def, and the move
tried to move that — it refused with \"Cannot move past superior
level\" and the cell stayed where it was."
  (with-temp-buffer
    (insert "# %%\ndef one():\n    return 1\n\n# %%\nprint(\"two\")\n")
    (python-mode)
    (code-cells-mode)
    ;; point in the body of the def, where a reader would be
    (goto-char (point-min))
    (forward-line 2)
    (pycell-move-cell-down 1)
    (should (equal (buffer-substring-no-properties (point-min) (point-max))
                   "# %%\nprint(\"two\")\n# %%\ndef one():\n    return 1\n\n"))
    ;; and point is still in the cell that moved
    (pcase-let ((`(,beg ,end) (code-cells--bounds)))
      (should (<= beg (point) end))
      (should (string-match-p "def one"
                              (buffer-substring-no-properties beg end))))))

(ert-deftest pycell-test-a-move-keeps-the-results-of-other-cells ()
  "A move takes the two cells it moves, and no others.
The text a move inserts lands at the first character of the cell below
it, which is where that cell's anchor begins: its
`insert-in-front-hooks' ran, and a third cell that had nothing to do
with the move lost its result on every move down."
  (with-temp-buffer
    (insert "# %%\na = 1\n\n# %%\nb = 2\n\n# %%\nc = 3\n")
    (python-mode)
    (code-cells-mode)
    ;; a result on each of the three cells
    (goto-char (point-min))
    (dolist (text '("one" "two" "three"))
      (pcase-let ((`(,beg ,end) (code-cells--bounds nil nil t)))
        (pycell--show beg end text 0.1))
      (code-cells-forward-cell))
    (let ((texts (lambda ()
                   (mapcar (lambda (b)
                             (string-trim
                              (plist-get (overblock-get b :data) :text)))
                           (sort (overblock-in (point-min) (point-max)
                                               'result)
                                 (lambda (a b) (< (overlay-start a)
                                                  (overlay-start b))))))))
      (should (equal (funcall texts) '("one" "two" "three")))
      ;; move the first cell down: the third keeps its result
      (goto-char (point-min))
      (forward-line 1)
      (pycell-move-cell-down 1)
      (should (equal (funcall texts) '("two" "one" "three")))
      ;; and back
      (pycell-move-cell-up 1)
      (should (equal (funcall texts) '("one" "two" "three"))))))

(ert-deftest pycell-test-move-cell-stops-at-the-ends ()
  "The first cell cannot move up and the last cannot move down.
`outline-move-subtree-down' says so — there is no sibling that way —
and nothing is taken off before it has said it."
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
        (should (equal (pycell--text (car (overblock-in beg end 'result)))
                       "one"))))))

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

(ert-deftest pycell-test-table-pops-as-a-live-table ()
  "The pop of a table gives a table that sorts, not a picture of one.
A copy carries the table object, and vtable draws a table of its rows
and columns for the window it lands in.  It draws a copy of it: the
table of a result belongs to the shell that made it, and Emacs 31
refuses to insert one vtable into a second buffer."
  (skip-unless (fboundp 'make-vtable))
  (pycell-test--with-cells
    (let* ((comint-prompt-regexp "^In \\[[0-9]+\\]: ")
           (text (pycell--clean (pycell-test--vtable-text))))
      (pcase-let ((`(,beg ,end) (code-cells--bounds nil nil t)))
        (pycell--show beg end text 0.4))
      (let ((ov (car (overblock-in (point-min) (point-max) 'result))))
        (goto-char (overlay-start ov))
        (save-window-excursion (pycell-pop-output))
        (with-current-buffer (pycell--cell-buffer-name nil (overlay-start ov))
          ;; The table is in there with whatever the cell printed around
          ;; it, so it is looked for rather than assumed to start the
          ;; buffer.
          (goto-char (or (text-property-not-all (point-min) (point-max)
                                                'vtable nil)
                         (point-min)))
          (should (vtable-current-table))
          (should (equal (mapcar #'vtable-column-name
                                 (vtable-columns (vtable-current-table)))
                         '("alpha" "beta_longer" "gamma")))
          ;; the drawn table is not the one the result carries
          (should-not (eq (vtable-current-table)
                          (get-text-property
                           (text-property-not-all 0 (length text)
                                                  'overblock-repl-table nil text)
                           'overblock-repl-table text)))
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

(ert-deftest pycell-test-the-queue-belongs-to-its-shell ()
  "Two notebooks do not share the cells a run-all still has to run.
The queue was one global list, so a run-all in one notebook discarded
another's cells and then fed its own down that notebook's interpreter."
  (let ((one (generate-new-buffer "one.py"))
        (two (generate-new-buffer "two.py"))
        (shell-one (generate-new-buffer "*Python one*"))
        (shell-two (generate-new-buffer "*Python two*")))
    (unwind-protect
        (progn
          (dolist (shell (list shell-one shell-two))
            (with-current-buffer shell (setq major-mode 'inferior-python-mode)))
          ;; Each notebook answers with a shell of its own, as
          ;; `python-shell-dedicated' gives it.
          (cl-letf (((symbol-function 'python-shell-get-process)
                     (lambda (&rest _)
                       (if (eq (current-buffer) one) 'proc-one 'proc-two)))
                    ((symbol-function 'process-buffer)
                     (lambda (proc)
                       (if (eq proc 'proc-one) shell-one shell-two))))
            (with-current-buffer one (pycell--queue-set '(a b c)))
            (with-current-buffer two (pycell--queue-set '(x)))
            (should (equal (with-current-buffer one (pycell--queued)) '(a b c)))
            (should (equal (with-current-buffer two (pycell--queued)) '(x)))
            ;; Stopping one leaves the other running.
            (with-current-buffer two (pycell-stop))
            (should (equal (with-current-buffer one (pycell--queued)) '(a b c)))
            (should-not (with-current-buffer two (pycell--queued)))))
      (mapc #'kill-buffer (list one two shell-one shell-two)))))

(ert-deftest pycell-test-out-label-goes-where-it-begins-a-line ()
  "An Out[N] label goes where it begins a line, and nowhere else.
That is where the shell writes one.  A label that stands in the middle
of a line cannot be told from the same characters inside a value:
unanchored, this took `Out[1]: ' out of a value that held it, and
`pycell-copy-output' yanked the hole with the text.  A label left on
the screen is the cheaper fault."
  (let ((comint-prompt-regexp "^\\(?:>>> \\|In \\[[0-9]+\\]: \\)"))
    ;; the label the shell wrote, at a line start
    (should (equal (pycell--clean "a\nOut[3]: 42\n") "a\n42"))
    (should (equal (pycell--clean "Out[1]: 42") "42"))
    ;; and the characters inside a value, which stay
    (should (equal (pycell--clean "Out[1]: 'it reads Out[1]: here'")
                   "'it reads Out[1]: here'"))
    (should (equal (pycell--clean "a fake label: Out[42]: done")
                   "a fake label: Out[42]: done"))))

(ert-deftest pycell-test-an-edit-at-the-end-of-a-cell-drops-the-block ()
  "Typing on the blank line that ends a cell takes the result with it.
A block's anchor stops one character short of the cell's last newline,
so that line is an insertion at the end of the overlay, which
`modification-hooks' never sees.  The result stayed and showed the
output of text that had changed under it.  The first character of the
cell is the same story from the other end."
  (dolist (where '(end start))
    (pycell-test--with-cells
      (pcase-let ((`(,beg ,end) (code-cells--bounds nil nil t)))
        (pycell--show beg end "old output" 0.1)
        (should (overblock-in (point-min) (point-max) 'result))
        (goto-char (if (eq where 'end) (1- end) beg))
        (insert "print(1)")
        (should-not (overblock-in (point-min) (point-max) 'result))))))

(ert-deftest pycell-test-a-final-newline-does-not-unrender-the-last-cell ()
  "A markdown cell at the end of a file survives the newline on save.
Without a final newline the anchor ends at `point-max', so the newline
`require-final-newline' adds is an insertion at the anchor's end — and
the rendering came off as the reader saved.  A newline there changes
nothing the cell renders, so the block stays.

The render itself writes nothing: appending the newline there signalled
`buffer-read-only' on a read-only notebook, and marked a buffer
modified by the act of visiting it."
  (skip-unless (overblock-md-program))
  (with-temp-buffer
    (insert "# %% [markdown]\n# text")          ; no final newline
    (python-mode)
    (code-cells-mode)
    (let ((size (buffer-size)))
      (pycell-md-render-all)
      (should (overblock-in (point-min) (point-max) 'markdown))
      ;; the render left the text alone
      (should (= (buffer-size) size))
      (set-buffer-modified-p nil)
      ;; the newline a save adds leaves the rendering standing
      (goto-char (point-max))
      (insert "\n")
      (should (overblock-in (point-min) (point-max) 'markdown))
      ;; a second character does not
      (goto-char (point-max))
      (insert "x")
      (should-not (overblock-in (point-min) (point-max) 'markdown)))))

(ert-deftest pycell-test-a-read-only-notebook-renders ()
  "Rendering a markdown cell writes nothing, so a read-only buffer renders.
Appending the final newline at render time signalled `buffer-read-only'
and the cell stayed plain — reachable through `view-file', a read-only
checkout, or any file the reader cannot write."
  (skip-unless (overblock-md-program))
  (with-temp-buffer
    (insert "# %% [markdown]\n# text")          ; no final newline
    (python-mode)
    (code-cells-mode)
    (setq buffer-read-only t)
    (pycell-md-render-all)
    (should (overblock-in (point-min) (point-max) 'markdown))))

(ert-deftest pycell-test-a-key-in-a-rendered-cell-reaches-the-cell ()
  "RET in a rendered markdown cell opens it, and does not insert a newline.
Point never enters a display string, so the keymap that answers a key
is the one on the overlays.  With the rendering left to answer for
itself, RET ran `newline': it split the source line under the
rendering and took the rendering with it.  No test bound a key, which
is how that got through."
  (skip-unless (overblock-md-program))
  (with-temp-buffer
    (insert "# %% [markdown]\n# A [link](https://ctan.org/) in prose.\n\n"
            "# %%\nx = 1\n")
    (python-mode)
    (code-cells-mode)
    (pycell-md-render-all)
    (should (overblock-in (point-min) (point-max) 'markdown))
    ;; point on the rendered cell, where a reader would press RET
    (goto-char (point-min))
    (forward-line 1)
    (should (eq (key-binding (kbd "RET")) #'pycell-md-edit))
    (should (eq (key-binding [mouse-1]) #'pycell-md-raw))
    (should (get-char-property (point) 'help-echo))))

(ert-deftest pycell-test-a-link-can-be-followed-from-the-keyboard ()
  "The links of a rendered cell are reachable without the mouse.
A click is answered by the string it lands on, and follows the link.
Point cannot be put on one — it never enters a display string — so the
cell is asked for its links instead."
  (skip-unless (overblock-md-program))
  (with-temp-buffer
    (insert "# %% [markdown]\n"
            "# A [link](https://ctan.org/) and [another](https://gnu.org/).\n\n"
            "# %%\nx = 1\n")
    (python-mode)
    (code-cells-mode)
    (pycell-md-render-all)
    (goto-char (point-min))
    (forward-line 1)
    (let* ((block (pycell--md-at nil))
           (links (pycell--md-links block)))
      (should (equal (mapcar #'cdr links)
                     '("https://ctan.org/" "https://gnu.org/")))
      (should (equal (mapcar #'car links) '("link" "another")))
      ;; one is followed without asking, several are chosen from
      (let (asked visited)
        (cl-letf (((symbol-function 'browse-url)
                   (lambda (url &rest _) (setq visited url)))
                  ((symbol-function 'completing-read)
                   (lambda (&rest _) (setq asked t) "another")))
          (pycell-md-follow-link)
          (should asked)
          (should (equal visited "https://gnu.org/")))))
    ;; and a cell with no link says so
    (erase-buffer)
    (insert "# %% [markdown]\n# No link here.\n\n# %%\nx = 1\n")
    (pycell-md-render-all)
    (goto-char (point-min))
    (forward-line 1)
    (should-error (pycell-md-follow-link) :type 'user-error)))

(ert-deftest pycell-test-a-link-on-an-image-is-found ()
  "A link around an image is found with the rest.
The piece that holds an image hides its line with an empty display
string and shows the row on the before-string, and reading the display
first lost every link of the cell — the badge a notebook opens with
included.

An Emacs that cannot read a PNG draws no image and keeps no link on
one: `overblock-md--image-file' answers nil for every path there."
  (skip-unless (overblock-md-program))
  (skip-unless (image-type-available-p 'png))
  (with-temp-buffer
    (insert "# %% [markdown]\n"
            "# [![badge](f.png)](https://colab.google/) and "
            "[plain](https://gnu.org/).\n\n# %%\nx = 1\n")
    (python-mode)
    (code-cells-mode)
    (cl-letf (((symbol-function 'display-images-p) (lambda (&rest _) t))
              ((symbol-function 'create-image)
               (lambda (f &rest _) (list 'image :type 'png :file f))))
      (pycell-md-render-all))
    (goto-char (point-min))
    (forward-line 1)
    (should (equal (mapcar #'cdr (pycell--md-links (pycell--md-at nil)))
                   '("https://colab.google/" "https://gnu.org/")))))

(ert-deftest pycell-test-a-pop-out-follows-a-running-cell ()
  "A popped-out result keeps filling while the cell runs.
The block shows `pycell-max-lines' of the output and no more; the
buffer takes the whole of it, so a long run can be followed in a window
of its own.  Only what is new is copied each time, and the cell's end
writes the whole of it again with the prompts off."
  (let ((shell (generate-new-buffer " *pycell-test-shell*"))
        (out (generate-new-buffer " *pycell-test-out*")))
    (unwind-protect
        (with-current-buffer shell
          (insert "one\ntwo\n")
          (setq-local pycell--run
                      (list :from (copy-marker 1)
                            :follow (cons out (copy-marker 1))))
          ;; what has been printed already
          (pycell--follow-tick)
          (should (equal (with-current-buffer out (buffer-string))
                         "one\ntwo\n"))
          ;; and then only what is new
          (goto-char (point-max))
          (insert "three\n")
          (pycell--follow-tick)
          (should (equal (with-current-buffer out (buffer-string))
                         "one\ntwo\nthree\n"))
          ;; point at the end followed the output
          (should (with-current-buffer out (= (point) (point-max))))
          ;; a buffer the reader killed is not written to
          (let ((gone (generate-new-buffer " *pycell-test-gone*")))
            (kill-buffer gone)
            (setq pycell--run (plist-put pycell--run :follow
                                         (cons gone (copy-marker 1))))
            (should-not (pycell--follow-tick)))
          ;; and the end writes the whole of it, cleaned
          (pycell--follow-done (cons out (copy-marker 1)) "one\ntwo\nthree")
          (should (equal (with-current-buffer out (buffer-string))
                         "one\ntwo\nthree")))
      (kill-buffer shell)
      (kill-buffer out))))

(ert-deftest pycell-test-a-restart-sweeps-what-lost-its-anchor ()
  "A restart takes the results and whatever no live block owns.
It swept orphans while it cleared every block; clearing the results
alone left a cloak of a lost block keeping lines of the buffer
invisible, with nothing able to remove it."
  (cl-letf (((symbol-function 'run-python) #'ignore)
            ((symbol-function 'python-shell-get-process) #'ignore)
            ((symbol-function 'pycell--queue-set) #'ignore)
            ((symbol-function 'pycell--dedicated) #'ignore))
    (pycell-test--with-cells
      (let ((orphan (make-overlay (point-min) (1+ (point-min)))))
        (overlay-put orphan 'overblock-part t)
        (overlay-put orphan 'invisible t)
        (pycell-restart)
        (should-not (overlay-buffer orphan))))))

(ert-deftest pycell-test-a-narrower-window-gets-a-new-bar ()
  "A bar is drawn again when the window it was built for has changed width.
`overblock-bar' cuts the label to the room the icons leave, and that
cut is in the string: a window made narrower afterwards — a split, a
side window, a frame resized — kept a label too long for it and the
header took two rows."
  (pycell-test--with-cells
    (pcase-let ((`(,beg ,end) (code-cells--bounds nil nil t)))
      (pycell--show beg end "one line of output" 1.6))
    (let* ((block (car (overblock-in (point-min) (point-max) 'result)))
           (wide (overlay-get block 'after-string)))
      (should wide)
      ;; the same buffer in a window of twenty columns
      (cl-letf (((symbol-function 'window-max-chars-per-line)
                 (lambda (&rest _) 20))
                ((symbol-function 'get-buffer-window-list)
                 (lambda (&rest _) (list (selected-window)))))
        (pycell--rewidth)
        (let ((narrow (overlay-get block 'after-string)))
          (should-not (equal wide narrow))
          (should (string-search "…" narrow))))
      ;; and nothing is redrawn while the width stands still
      (cl-letf (((symbol-function 'window-max-chars-per-line)
                 (lambda (&rest _) 20))
                ((symbol-function 'get-buffer-window-list)
                 (lambda (&rest _) (list (selected-window))))
                ((symbol-function 'pycell--update)
                 (lambda (&rest _) (error "drawn again for nothing"))))
        (pycell--rewidth)))))

(ert-deftest pycell-test-a-pop-out-keeps-what-is-around-a-table ()
  "A pop-out holds the whole result, table and all the rest.
It held the table alone: the lines a cell printed before its DataFrame,
and the lines a follower had already watched go by, were dropped by the
buffer that is meant to hold more than the block."
  (skip-unless (fboundp 'make-vtable))
  (with-temp-buffer
    (let ((text (concat "before the table\n"
                        (let ((comint-prompt-regexp "^In \\[[0-9]+\\]: "))
                          (pycell--clean (pycell-test--vtable-text)))
                        "\nafter the table")))
      (pycell--insert-result text)
      (let ((shown (buffer-string)))
        ;; in the order the cell printed them, which is the point:
        ;; `vtable-insert' leaves point between the header and the rows,
        ;; so what followed a table was inserted into the middle of it
        ;; and the table's body was pushed to the end of the buffer
        (should (string-search "before the table" shown))
        (should (string-search "after the table" shown))
        (should (< (string-search "before the table" shown)
                   (string-search "alpha" shown)))
        (should (< (string-search "alpha" shown)
                   (string-search "after the table" shown)))
        ;; every row of the table is above the text that followed it
        (dolist (cell '("22" "4444" "9999"))
          (should (< (string-search cell shown)
                     (string-search "after the table" shown)))))
      (goto-char (or (text-property-not-all (point-min) (point-max)
                                            'vtable nil)
                     (point-min)))
      (should (vtable-current-table)))))

(ert-deftest pycell-test-a-pop-out-interrupts-its-own-shell ()
  "A popped-out result interrupts the shell it came from.
It is not a Python buffer, so `python-shell-get-process' would answer
with whatever the settings point at — the wrong shell where the
notebook has one of its own.  `i' is the key: the buffer is read-only,
so a plain one is free and answers wherever the reader has bound the
`C-c' prefix."
  (should (eq (keymap-lookup pycell-pop-map "i") #'pycell-interrupt))
  (let* ((shell (generate-new-buffer " *pycell-test-shell*"))
         (notebook (generate-new-buffer " *pycell-test-nb*"))
         ;; A marker whose buffer is gone, made before anything is
         ;; stubbed: `kill-buffer' asks `get-buffer-process' itself.
         (dead (let ((gone (generate-new-buffer " *pycell-test-gone*")))
                 (with-current-buffer gone (insert "y = 2\n"))
                 (prog1 (with-current-buffer gone (copy-marker 1))
                   (kill-buffer gone))))
         asked)
    (unwind-protect
        (cl-letf (((symbol-function 'interrupt-process)
                   (lambda (process) (setq asked process)))
                  ((symbol-function 'get-buffer-process)
                   (lambda (buffer) (list 'process-of buffer)))
                  ((symbol-function 'python-shell-get-process-or-error)
                   (lambda (&rest _) (error "asked for a shell of its own"))))
          (let ((cell (with-current-buffer notebook
                        (insert "x = 1\n")
                        (copy-marker (point-min)))))
            (with-current-buffer shell
              (setq-local pycell--run (list :beg cell)))
            (with-temp-buffer
              (setq-local pycell--shell shell)
              (setq-local pycell--cell cell)
              (pycell-interrupt)
              (should (equal asked (list 'process-of shell)))
              ;; and not another notebook's run, nor a result that ended
              (setq asked nil)
              (with-current-buffer shell
                (setq pycell--run (list :beg (with-current-buffer notebook
                                               (copy-marker (point-max))))))
              (should-error (pycell-interrupt) :type 'user-error)
              (should-not asked)
              (with-current-buffer shell (setq pycell--run nil))
              (should-error (pycell-interrupt) :type 'user-error)
              (should-not asked))
            ;; A killed notebook leaves the run's marker and this
            ;; buffer's — the same object — pointing nowhere.  `eq' on
            ;; two nil buffers passed the test that `=' then signalled
            ;; "Marker does not point anywhere" on, and the cell was
            ;; left running with the pop-out the only place to stop it.
            (progn
              (with-current-buffer shell (setq pycell--run (list :beg dead)))
              (with-temp-buffer
                (setq-local pycell--shell shell)
                (setq-local pycell--cell dead)
                (should-error (pycell-interrupt) :type 'user-error)
                (should-not asked)))
            ;; And a shell that has outlived its process says whose
            ;; process is missing: `interrupt-process' of nil takes the
            ;; current buffer's, which is not this buffer's business.
            (with-current-buffer shell (setq pycell--run (list :beg cell)))
            (cl-letf (((symbol-function 'get-buffer-process) #'ignore))
              (with-temp-buffer
                (setq-local pycell--shell shell)
                (setq-local pycell--cell cell)
                (should-error (pycell-interrupt) :type 'user-error)
                (should-not asked)))))
      (kill-buffer shell)
      (kill-buffer notebook))))

(ert-deftest pycell-test-a-restart-keeps-the-renderings ()
  "A restart takes the results down and leaves the markdown standing.
It took both, and `pycell-restart-and-run-all' puts a rendering back
only when the pass reaches its cell — so a pass that stopped at an
error, or on `pycell-stop', left every cell after that point plain.  A
rendering has nothing to do with the interpreter."
  (cl-letf (((symbol-function 'run-python) #'ignore)
            ((symbol-function 'python-shell-get-process) #'ignore)
            ((symbol-function 'pycell--queue-set) #'ignore)
            ((symbol-function 'pycell--dedicated) #'ignore))
    (pycell-test--with-cells
      (pcase-let ((`(,beg ,end) (code-cells--bounds nil nil t)))
        (pycell--show beg end "output" 0.1))
      (goto-char (point-max))
      (let ((beg (point)))
        (insert "# %% [markdown]\n# text\n")
        (overblock-show (+ beg 16) (point-max) :kind 'markdown :over "text"))
      (should (overblock-in (point-min) (point-max) 'result))
      (should (overblock-in (point-min) (point-max) 'markdown))
      (pycell-restart)
      (should-not (overblock-in (point-min) (point-max) 'result))
      (should (overblock-in (point-min) (point-max) 'markdown)))))

(ert-deftest pycell-test-unrender-keeps-the-results ()
  "`pycell-md-unrender' takes the renderings and nothing else.
It swept orphaned parts with a bare `overblock-clear', which with no
argument deletes every live block of every kind first: a reader who
unrendered markdown lost the result of a five-minute cell."
  (pycell-test--with-cells
    (pcase-let ((`(,beg ,end) (code-cells--bounds nil nil t)))
      (pycell--show beg end "output" 0.1))
    ;; a rendering, made by hand so no converter is needed
    (goto-char (point-max))
    (insert "# %% [markdown]\n# text\n")
    (let ((block (overblock-show (- (point-max) 7) (point-max)
                                 :kind 'markdown :over "text")))
      (should block))
    ;; and an orphan: a part whose anchor is gone
    (let ((orphan (make-overlay (point-min) (1+ (point-min)))))
      (overlay-put orphan 'overblock-part t)
      (pycell-md-unrender)
      (should (overblock-in (point-min) (point-max) 'result))
      (should-not (overblock-in (point-min) (point-max) 'markdown))
      (should-not (overlay-buffer orphan)))))

(ert-deftest pycell-test-the-queue-walks-markdown-cells-in-one-frame ()
  "A run-all pass crosses markdown cells without building a frame each.
`pycell--run-next' used to call `pycell-eval-region', which called it
back: two markdown cells in a row made two frames, and each frame ran
its own tail on the way out — the second sending a code cell while the
first was still running.  `pycell--send' refused that one from inside
the process filter, and the cell, already off the queue, never ran."
  (with-temp-buffer
    (insert "# %% [markdown]\n# one\n\n# %% [markdown]\n# two\n\n"
            "# %%\nx = 1\n\n# %%\ny = 2\n")
    (python-mode)
    (code-cells-mode)
    (let ((notebook (current-buffer))
          (shell (generate-new-buffer " *pycell-test-shell*"))
          (sent nil)
          (depth 0)
          (deepest 0))
      (unwind-protect
          (cl-letf* (((symbol-function 'pycell--queue-buffer)
                      (lambda (&rest _) shell))
                     ((symbol-function 'overblock-md-rendered)
                      (lambda (md &rest _) md))
                     ;; A code cell is where the walk has to stop.
                     ((symbol-function 'python-shell-get-process)
                      (lambda (&rest _) 'process))
                     (send (symbol-function 'pycell--send))
                     ((symbol-function 'pycell--send)
                      (lambda (_proc beg _end)
                        (ignore send)
                        (setq depth (1+ depth)
                              deepest (max deepest depth))
                        (push beg sent)
                        (setq depth (1- depth)))))
            ;; The markers belong to the notebook: `copy-marker' made
            ;; in the shell buffer points into the shell.
            (let ((cells (with-current-buffer notebook
                           (save-excursion
                             (goto-char (point-min))
                             (let ((marks (list (point-marker))))
                               (dotimes (_ 3)
                                 (code-cells-forward-cell)
                                 (push (point-marker) marks))
                               (nreverse marks))))))
              (with-current-buffer shell
                (setq-local pycell--queue cells)))
            (pycell--run-next)
            ;; The two markdown cells are rendered, the first code cell
            ;; is sent, and the walk stops there: the second code cell
            ;; waits for the prompt of the first.  The old shape sent
            ;; both, and the second was refused and lost.
            (should (= (length (overblock-in (point-min) (point-max)
                                             'markdown))
                       2))
            (should (= (length sent) 1))
            (should (= deepest 1))
            (should (= (length (pycell--queued)) 1)))
        (kill-buffer shell)))))

(ert-deftest pycell-test-copy-output-keeps-what-the-result-holds ()
  "The copy carries the text properties, so an image survives a yank.
It is the result the click landed on, not the one point is in.
The buffer has to be in a window: a click carries the window it landed
in, and the commands select it."
  (pycell-test--with-notebook "# %%\nx = 1\n\n# %%\ny = 2\n"
    (let ((kill-ring nil)
          (in-the-first-cell nil))
      (pcase-let ((`(,beg ,end) (code-cells--bounds nil nil t)))
        (setq in-the-first-cell beg)
        (pycell--show beg end (concat "a line\n" pycell-test--image) 0.1))
      ;; point in the other cell: the click decides which result is copied
      (goto-char (point-max))
      (pycell-copy-output (list 'mouse-1 (list (selected-window)
                                               in-the-first-cell
                                               (cons 0 0) 0)))
      (should (string-prefix-p "a line" (current-kill 0)))
      (should (overblock-image-in (current-kill 0))))))

(ert-deftest pycell-test-discard-output-takes-one-result ()
  "The result of the cell that was clicked goes, and no other."
  (pycell-test--with-notebook "# %%\nx = 1\n\n# %%\ny = 2\n"
    (dolist (which '(1 -1))
      (goto-char (if (> which 0) (point-min) (point-max)))
      (pcase-let ((`(,beg ,end) (code-cells--bounds nil nil t)))
        (pycell--show beg end "out" 0.1)))
    (should (= 2 (length (overblock-in (point-min) (point-max) 'result))))
    (goto-char (point-min))
    (pycell-discard-output)
    (should (= 1 (length (overblock-in (point-min) (point-max) 'result))))
    ;; the one left is the second cell's
    (should (> (overlay-start (car (overblock-in (point-min) (point-max)
                                                 'result)))
               (point-min)))
    ;; and asking again where there is none says so rather than guessing
    (should-error (pycell-discard-output) :type 'user-error)))

(ert-deftest pycell-test-a-markdown-edit-can-be-abandoned ()
  "`pycell-md-abort' leaves the source as it was, and the window with it."
  (let ((buffer (get-buffer-create " *pycell test md abort*")))
    (unwind-protect
        (with-current-buffer buffer
          (setq-local pycell--md-source (list (current-buffer) 1 2))
          (should (commandp 'pycell-md-abort))
          (cl-letf (((symbol-function 'quit-window)
                     (lambda (&optional kill _window)
                       (should kill)
                       (throw 'quit t))))
            (should (catch 'quit (pycell-md-abort) nil))))
      (kill-buffer buffer))))

(ert-deftest pycell-test-save-image-writes-the-bytes-it-was-given ()
  "The file holds the data of the image, and its type names it.
A result with no image says so rather than writing an empty file."
  (pycell-test--with-notebook "# %%\nx = 1\n\n# %%\ny = 2\n"
    (let* ((png (propertize " " 'display '(image :type png :data "\211PNG!")))
           ;; A name with nothing at it: the command hands `write-region'
           ;; a MUSTBENEW of t, which asks before it overwrites, and a
           ;; batch Emacs has nobody to ask.
           (file (expand-file-name (make-temp-name "pycell-image-")
                                   temporary-file-directory)))
      (unwind-protect
          (progn
            (pcase-let ((`(,beg ,end) (code-cells--bounds nil nil t)))
              (pycell--show beg end (concat "a figure\n" png) 0.1))
            (cl-letf (((symbol-function 'read-file-name)
                       (lambda (_prompt &rest _) file)))
              (pycell-save-image))
            (should (file-exists-p file))
            (should (equal (with-temp-buffer
                             (set-buffer-multibyte nil)
                             (insert-file-contents-literally file)
                             (buffer-string))
                           "\211PNG!"))
            ;; the default name follows the type of the image
            (delete-file file)
            (let (offered)
              (cl-letf (((symbol-function 'read-file-name)
                         (lambda (_prompt _dir _default _mustmatch initial)
                           (setq offered initial)
                           file)))
                (pycell-save-image))
              (should (equal offered "figure.png")))
            ;; and a result without an image is not a file
            (goto-char (point-max))
            (pcase-let ((`(,beg ,end) (code-cells--bounds nil nil t)))
              (pycell--show beg end "no figure here" 0.1))
            (should-error (pycell-save-image) :type 'user-error))
        (when (file-exists-p file) (delete-file file))))))

;;;; The bar over a boundary line

(ert-deftest pycell-test-the-title-is-what-follows-the-marker ()
  "The text after the marker names the cell, and a tag list is not text."
  (with-temp-buffer
    (insert "# %%\n# %% A title\n# %% [markdown]\n# %% [markdown] Notes\n")
    (python-mode)
    (code-cells-mode)
    (should (equal (mapcar (lambda (line)
                             (goto-char (point-min))
                             (forward-line (1- line))
                             (pycell--cell-title (pos-bol) (pos-eol)))
                           '(1 2 3 4))
                   '(nil "A title" nil "Notes")))))

(ert-deftest pycell-test-a-bar-over-every-code-cell ()
  "Every code cell boundary line carries a bar, and the line is hidden.
A markdown boundary line is left to the rendering, which brings its own."
  (pycell-test--with-notebook
      "# %%\nx = 1\n\n# %% Titled\ny = 2\n\n# %% [markdown]\n# text\n"
    (should (equal (pycell-test--bar-labels) '("python" "Titled")))
    ;; and nothing of this kind on the markdown line
    (should-not (seq-find (lambda (ov)
                            (eq (overlay-get ov 'overblock-bar) 'code))
                          (overlays-in (save-excursion
                                         (goto-char (point-min))
                                         (re-search-forward "\\[markdown\\]")
                                         (pos-bol))
                                       (point-max))))
    (let ((bar (car (sort (overblock-bars)
                          (lambda (a b)
                            (< (overlay-start a) (overlay-start b)))))))
      (should (equal (overlay-get bar 'overblock-bar) 'code))
      ;; The bar rides the before-string; the line's own text draws as
      ;; the bar's last glyph, which carries `cursor' so the caret has
      ;; somewhere to be.  Never the after-string: a rendering's cloak
      ;; covers the place a string after the overlay would draw in.
      (should (overlay-get bar 'before-string))
      (should (eq (get-text-property 0 'cursor (overlay-get bar 'display)) t))
      (should (= (length (overlay-get bar 'display)) 1))
      (should-not (overlay-get bar 'after-string))
      (should (equal (buffer-substring-no-properties (overlay-start bar)
                                                     (overlay-end bar))
                     "# %%")))))

(ert-deftest pycell-test-a-cell-typed-in-gets-a-bar ()
  "A boundary line written into the buffer is barred as it appears.
And a line that becomes a markdown boundary loses the code bar it had."
  (pycell-test--with-notebook "# %%\nx = 1\n"
    (goto-char (point-max))
    (insert "\n# %% Later\nz = 3\n")
    (should (equal (pycell-test--bar-labels) '("python" "Later")))
    ;; The title is read again when the line is edited.
    (goto-char (point-min))
    (end-of-line)
    (insert " Named")
    (should (equal (pycell-test--bar-labels) '("Named" "Later")))
    ;; And a line rewritten as a markdown boundary loses its code bar.
    (goto-char (point-min))
    (delete-region (pos-bol) (pos-eol))
    (insert "# %% [markdown]")
    (should (equal (pycell-test--bar-labels) '("Later")))))

(ert-deftest pycell-test-a-narrowing-hides-no-cell-from-the-bars ()
  "Every cell is barred, not only the ones the narrowing shows.
A notebook narrowed when the mode goes on — `narrow-to-defun', an
indirect buffer, a source edit — would otherwise have had bars over the
visible cells alone."
  (with-temp-buffer
    (insert "# %% One\nx = 1\n\n# %% Two\ny = 2\n")
    (python-mode)
    (set-window-buffer nil (current-buffer))
    (code-cells-mode)
    (narrow-to-region (point-min) 12)
    (unwind-protect
        (progn
          (pycell-mode)
          (should (= 2 (length (overblock-bars)))))
      (pycell-mode -1))))

(ert-deftest pycell-test-the-mode-takes-its-bars-with-it ()
  "Turning the mode off leaves the buffer as it was."
  (with-temp-buffer
    (insert "# %%\nx = 1\n")
    (python-mode)
    (code-cells-mode)
    (pycell-mode)
    (should (overblock-bars))
    (pycell-mode -1)
    (should-not (overblock-bars))))

(ert-deftest pycell-test-run-above-refuses-the-first-cell ()
  "There is nothing above the first cell, and nothing is started for it.
The cells above are the ones the walk finds before this one begins."
  (pycell-test--with-notebook "# %% One\nx = 1\n\n# %% Two\ny = 2\n"
    (goto-char (point-min))
    (should-error (pycell-run-above) :type 'user-error)
    (goto-char (point-max))
    (should (equal (mapcar #'marker-position
                           (seq-take-while
                            (lambda (m) (< m (car (code-cells--bounds))))
                            (pycell--cell-starts)))
                   (list 1)))))

(ert-deftest pycell-test-a-button-moves-the-cell-it-belongs-to ()
  "A click on the arrow of one cell moves that cell, not the one at point.
The commands read the click, so the cell that moves is the one whose
button was pressed: with point left where it was, the arrow of the
second cell moved the first."
  (pycell-test--with-notebook "# %% One\nx = 1\n\n# %% Two\ny = 2\n"
    (let* ((second (save-excursion
                     (goto-char (point-min))
                     (re-search-forward "^# %% Two")
                     (pos-bol)))
           (click (list 'mouse-1 (list (selected-window) second
                                       (cons 0 0) 0))))
      ;; point in the first cell, the click on the second
      (goto-char (point-min))
      (pycell-move-cell-up 1 click)
      (goto-char (point-min))
      (should (looking-at-p "# %% Two")))))

(ert-deftest pycell-test-a-line-that-stops-being-a-boundary-loses-its-bar ()
  "A bar belongs to a boundary line, and to a line that still is one.
A space typed before the comment, or half of the marker deleted, left
the bar where it was — and its buttons then acted on the cell that now
encloses the line."
  (pycell-test--with-notebook "# %% One\nx = 1\n\n# %% Two\ny = 2\n"
    (should (equal (pycell-test--bar-labels) '("One" "Two")))
    (goto-char (point-min))
    (insert " ")                        ; " # %% One" is no boundary
    (should (equal (pycell-test--bar-labels) '("Two")))
    (goto-char (point-min))
    (delete-char 1)
    (should (equal (pycell-test--bar-labels) '("One" "Two")))
    ;; and half a marker is no marker
    (goto-char (point-min))
    (re-search-forward "%%")
    (delete-char -1)
    (should (equal (pycell-test--bar-labels) '("Two")))))

(ert-deftest pycell-test-a-markdown-cell-showing-its-source-has-a-bar ()
  "A markdown cell that is not rendered is barred too, and can be rendered.
It had no bar at all: writing `[markdown]' on a line took the code bar
off it and nothing put one back until the whole buffer was rendered
again."
  (pycell-test--with-notebook "# %% [markdown]\n# text\n\n# %% Two\ny = 2\n"
    (let ((bar (overblock-bar-in (point-min) (pos-eol))))
      ;; Rendered or not — a converter may be missing — the line has a
      ;; bar, and it is not a code bar.
      (should bar)
      (should (memq (overblock-bar-kind bar) '(source markdown)))
      (should (commandp 'pycell-md-render-cell)))))

(ert-deftest pycell-test-a-boundary-line-keeps-one-bar-through-its-kinds ()
  "Writing and unwriting `[markdown]' leaves one bar, of the right kind.
The bar of a cell showing its source stayed on a line that had stopped
saying =[markdown]=, and the code bar was drawn beside it."
  (pycell-test--with-notebook "# %% One\nx = 1\n\n# %% Two\ny = 2\n"
    (let ((line (lambda ()
                  (save-excursion
                    (goto-char (point-min))
                    (list (overblock-bar-kind (overblock-bar-on-line))
                          (length (seq-filter
                                   #'overblock-bar-kind
                                   (overlays-in (point-min) (pos-eol)))))))))
      (should (equal (funcall line) '(code 1)))
      ;; The tag follows the marker, as jupytext writes it: a
      ;; `[markdown]' at the end of the title is not a markdown cell.
      (goto-char (point-min))
      (delete-region (pos-bol) (pos-eol))
      (insert "# %% [markdown] One")
      (should (equal (funcall line) '(source 1)))
      (goto-char (point-min))
      (delete-region (pos-bol) (pos-eol))
      (insert "# %% One")
      (should (equal (funcall line) '(code 1))))))

(ert-deftest pycell-test-one-glyph-means-one-thing ()
  "No two buttons draw the same glyph, in any row of candidates.
A frame draws whichever row it can: the nerd glyphs, the symbols an
ordinary font has, or the plain characters a terminal falls to.  Two
rounds of this: `^' meant both \"pop this result out\" and \"edit this
cell\" in the last row, and `↗' meant both in the middle one, which is
the row a frame with a font and no nerd glyphs draws."
  (let ((bars (list pycell-result-buttons pycell-markdown-buttons
                    pycell-cell-buttons pycell-source-buttons)))
    (dotimes (row 3)
      ;; No glyph twice on one bar.
      (dolist (buttons bars)
        (let ((glyphs (mapcar (lambda (button) (nth row (nth 1 button)))
                              buttons)))
          (should (equal glyphs (delete-dups (copy-sequence glyphs))))))
      ;; And one glyph, one command, across all of them.
      (let (seen)
        (dolist (buttons bars)
          (dolist (button buttons)
            (let* ((glyph (nth row (nth 1 button)))
                   (command (nth 3 button))
                   (before (assoc glyph seen)))
              (when before
                (should (eq (cdr before) command)))
              (push (cons glyph command) seen))))))
    ;; A frame draws no row whole: `overblock-glyph' answers for one
    ;; button at a time, so a frame with a font that has some of the
    ;; symbols draws those and falls to the plain characters for the
    ;; rest.  Measured in a frame with the nerd font truly absent:
    ;; `u d a ▷ / u d m / u d e s / u d ↓ ◫ ^ x' — two rows at once.  So
    ;; a glyph stands for one command whichever row it comes from.
    (let (seen)
      (dolist (buttons bars)
        (dolist (button buttons)
          (dolist (glyph (nth 1 button))
            (let ((before (assoc glyph seen)))
              (when before
                (should (eq (cdr before) (nth 3 button))))
              (push (cons glyph (nth 3 button)) seen))))))))

(ert-deftest pycell-test-customizing-the-buttons-draws-the-bars-again ()
  "A button list set with `setopt' shows on a notebook already open.
It showed only when something else drew a bar again — a window changing
width, or the file opened afresh — so customizing the buttons of an open
notebook appeared to do nothing."
  (let ((was pycell-cell-buttons))
    (pycell-test--with-notebook "# %% One\nx = 1\n"
      (unwind-protect
          (progn
            (should-not (string-search "ZZ" (car (pycell-test--bar-texts))))
            (setopt pycell-cell-buttons
                    '((only ("ZZ") "The only button" pycell-run-cell t)))
            (should (string-search "ZZ" (car (pycell-test--bar-texts)))))
        (setopt pycell-cell-buttons was)))))

(ert-deftest pycell-test-a-cell-taken-back-to-its-source-keeps-a-bar ()
  "`pycell-md-raw' leaves the bar that carries the button to render again.
Taking a rendering down deletes the bar that belonged to it and changes
no text, so nothing else would draw one: measured in a graphical frame,
the line was left bare, and the button that renders the cell sits on the
bar that was not there."
  (skip-unless (overblock-md-program))
  (pycell-test--with-notebook "# %% [markdown]\n# text\n\n# %% Two\ny = 2\n"
    (let ((kind (lambda ()
                  (save-excursion
                    (goto-char (point-min))
                    (overblock-bar-kind (overblock-bar-on-line))))))
      ;; `should' and not `skip-unless': the kind of that bar is what
      ;; this test measures, and a skip would hide the regression it
      ;; guards.  The converter is asked for above.
      (should (eq (funcall kind) 'markdown))
      (goto-char (point-min))
      (forward-line 1)
      (pycell-md-raw)
      (should (eq (funcall kind) 'source))
      ;; one bar, not two
      (should (= 1 (length (seq-filter
                            #'overblock-bar-kind
                            (overlays-in (point-min)
                                         (save-excursion
                                           (goto-char (point-min))
                                           (pos-eol)))))))
      ;; and the button renders it again
      (goto-char (point-min))
      (forward-line 1)
      (pycell-md-render-cell)
      (should (eq (funcall kind) 'markdown)))))

(ert-deftest pycell-test-a-click-on-a-bar-leaves-the-bar-showing ()
  "Point lands below the bar, not on it, so the button can be pressed again.
The reveal gives a bar way to its line while point is on it, and a click
that put point there took the button out from under the reader between
one press and the next."
  (pycell-test--with-notebook "# %% One\nx = 1\n\n# %% Two\ny = 2\n"
    (let* ((second (save-excursion
                     (goto-char (point-min))
                     (re-search-forward "^# %% Two")
                     (pos-bol)))
           (click (list 'mouse-1 (list (selected-window) second
                                       (cons 0 0) 0))))
      (goto-char (point-min))
      (pycell--goto-event click)
      (should (= (point) second))
      ;; and the cell the commands will find is that one
      (should (equal (car (code-cells--bounds)) second))
      ;; the bar is where it was, whatever point does
      (should (overlay-get (save-excursion (goto-char second)
                                           (overblock-bar-on-line))
                           'before-string)))))

(ert-deftest pycell-test-a-refused-pass-leaves-nothing-queued ()
  "A run-above that cannot start leaves no cells behind to run later.
The queue was armed before the first cell was sent, so a refusal left
the rest of the pass to run unasked, less the cell the refusal had
already taken off it."
  (pycell-test--with-notebook "# %% One\nx = 1\n\n# %% Two\ny = 2\n"
    (let ((shell (generate-new-buffer " *pycell-test-shell*")))
      (unwind-protect
          (cl-letf* (((symbol-function 'python-shell-get-process)
                      (lambda (&rest _) 'a-process))
                     ((symbol-function 'pycell--queue-buffer)
                      (lambda () shell))
                     ;; The shell refuses the cell, as a busy one does.
                     ((symbol-function 'pycell-eval-region)
                      (lambda (&rest _) (user-error "Still busy"))))
            (goto-char (point-max))
            (should-error (pycell-run-above) :type 'user-error)
            (should-not (pycell--queued)))
        (kill-buffer shell)))))

(ert-deftest pycell-test-a-change-leaves-the-search-alone ()
  "A caller's match survives the bars being drawn.
`after-change-functions' runs between a search and what the searcher
does with the match, and the walk that draws the bars searches too:
`replace-match' after a `search-forward' signalled
`args-out-of-range' while the walk had the match data."
  (pycell-test--with-notebook "# %% code BEFORE\nx = 1\n"
    (goto-char (point-min))
    (should (search-forward "BEFORE" nil t))
    (replace-match "AFTER")
    (should (equal (buffer-string) "# %% code AFTER\nx = 1\n"))))

(ert-deftest pycell-test-a-failure-without-a-traceback-stops-a-pass ()
  "Output that names an exception ends a pass, traceback or not.
A `SyntaxError' prints the name of the exception and nothing else, and
a run-all walked happily past a cell holding `x = = 1\\='."
  ;; What ipython prints for a syntax error, in full.
  (should (pycell--error-p "  File <ipython-input-3>:1\n    x = = 1\n        ^\nSyntaxError: invalid syntax\n"))
  (should (pycell--error-p "Traceback (most recent call last)\n  ...\nValueError: boom\n"))
  (should (pycell--error-p "SystemExit: 2"))
  (should (pycell--error-p "KeyboardInterrupt"))
  (should (pycell--error-p "numpy.linalg.LinAlgError: singular matrix"))
  ;; And what is not a failure.
  (should-not (pycell--error-p "42\n"))
  (should-not (pycell--error-p ""))
  (should-not (pycell--error-p "the Error: was printed, not raised\n"))
  (should-not (pycell--error-p "Done\n"))
  ;; A cell that prints the name of an exception it caught: the colon
  ;; is what tells a report from a print, and the two names IPython
  ;; does print alone are the exception to that.
  (should-not (pycell--error-p "ValueError\n"))
  (should-not (pycell--error-p "caught: ZeroDivisionError\n")))

(ert-deftest pycell-test-a-rendered-bar-follows-its-line ()
  "A title typed at the end of a boundary line reaches the bar above it.
The bar's overlay does not grow at its end, so the title fell outside
it: the label was read from the stale region, and the text beyond the
overlay drew after the bar and took a second screen row."
  (skip-unless (overblock-md-program))
  (pycell-test--with-notebook "# %% [markdown] first\n# text\n"
    (pycell-md-render-all)
    (let ((bar (save-excursion (goto-char (point-min))
                               (overblock-bar-on-line))))
      ;; The product under test, so `should': see the same question in
      ;; `pycell-test-md-an-edit-takes-the-bar-with-it'.
      (should (eq (overblock-bar-kind bar) 'markdown))
      (goto-char (pos-eol))
      (insert " and more")
      ;; The bar covers the whole line again, and says so.
      (should (= (overlay-end bar) (pos-eol)))
      (should (string-match-p "first and more"
                              (overlay-get bar 'overblock-bar-text))))))

(ert-deftest pycell-test-a-pass-remembers-where-it-came-from ()
  "The place a pass was asked for is kept with the shell, and given back.
Three faults lived here: the first pass of a session had no shell to
keep the marker in, `pycell-restart-and-run-all' never set one, and a
pass refused by a busy shell left its marker behind to drag point when
an unrelated cell ended."
  (pycell-test--with-notebook "# %% One\nx = 1\n\n# %% Two\ny = 2\n"
    (let ((shell (get-buffer-create " *pycell-test-shell*")))
      (cl-letf (((symbol-function 'pycell--queue-buffer) (lambda () shell)))
        (with-current-buffer shell (setq-local pycell--queue-home nil))
        (goto-char (point-max))
        (pycell--home-set (point-marker))
        (should (= (marker-position
                    (buffer-local-value 'pycell--queue-home shell))
                   (point-max)))
        ;; A refusal takes it away again, so nothing drags point later.
        (pycell--home-set nil)
        (should-not (buffer-local-value 'pycell--queue-home shell))
        ;; And going home with none set is not an error.
        (pycell--go-home))
      (kill-buffer shell))))

(provide 'pycell-test)
;;; pycell-test.el ends here
