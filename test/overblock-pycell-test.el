;;; overblock-pycell-test.el --- Tests for overblock-pycell -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Marcel Arpogaus

;; Author: Marcel Arpogaus <znepry.necbtnhf@tznvy.pbz>
;; Assisted-by: Claude:claude-opus-5
;; Assisted-by: Claude:claude-fable-5
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

;; Run with: make test
;;
;; The tests cover what works without an inferior Python process: the
;; output clean-up, the block layout and the markdown helpers.

;;; Code:

(require 'ert)
(require 'outline)
(require 'overblock-pycell)
(require 'overblock-pydoc)
(require 'overblock-test-common)

(defun overblock-pycell-test--render-all ()
  "Render the markdown cells of the buffer, and wait for them.
The conversion runs in a process that the package does not wait for,
so the test waits.  A rendering is wanted only where the live cycle of
its kind is on, so a buffer without the mode gets the record of the
cycle without the hooks of the mode."
  (unless (assq 'markdown overblock-live--specs)
    (setq-local overblock-live--specs
                (list (list 'markdown #'overblock-pycell-render-buffer))))
  (overblock-pycell-render-buffer)
  (overblock-pycell-test--settle))

(defun overblock-pycell-test--settle ()
  "Wait until no converter process is running."
  (overblock-test-common-converted))

(defmacro overblock-pycell-test--with-cells (&rest body)
  "Evaluate BODY in a Python buffer with two code cells."
  (declare (indent 0))
  `(with-temp-buffer
     (insert "# %%\nx = 1\n\n# %%\ny = 2\n")
     (python-mode)
     (code-cells-mode)
     (setq-local overblock-run-backend (overblock-pycell--backend))
     ;; The backend, not the mode: these tests draw results without the
     ;; hooks of the mode.
     (overblock-run-attach (overblock-pycell--backend))
     (goto-char (point-min))
     ,@body))

(defmacro overblock-pycell-test--with-notebook (text &rest body)
  "Evaluate BODY in a Python buffer holding TEXT, with the mode on.
The buffer is shown in a window: a bar is cut to the width of the
windows that show it, and a command that follows a click selects one."
  (declare (indent 1))
  `(with-temp-buffer
     (insert ,text)
     (python-mode)
     (set-window-buffer nil (current-buffer))
     (code-cells-mode)
     (overblock-pycell-mode)
     (overblock-pycell-test--settle)
     (goto-char (point-min))
     (unwind-protect (progn ,@body)
       (overblock-pycell-mode -1))))

(defmacro overblock-pycell-test--with-mode (&rest body)
  "Evaluate BODY with `overblock-pycell-mode' on, and turn it off afterwards.
The mode adds the advice on `outline-flag-region' that folds a block
with its code, so a test of a fold needs the mode.  The mode goes off
afterwards, because `with-temp-buffer' kills its buffer without
removing the advice."
  (declare (indent 0))
  `(progn (overblock-pycell-mode 1)
          (overblock-pycell-test--settle)
          (unwind-protect (progn ,@body)
            (overblock-pycell-mode -1))))

(defun overblock-pycell-test--bar-texts ()
  "Return the whole text of every code cell bar of the buffer, in order."
  (mapcar (lambda (ov)
            (substring-no-properties (or (overlay-get ov 'overblock-bar-text)
                                         "")))
          (sort (seq-filter (lambda (ov)
                              (eq (overlay-get ov 'overblock-bar) 'code))
                            (overblock-bars))
                (lambda (a b) (< (overlay-start a) (overlay-start b))))))

(defun overblock-pycell-test--bar-labels ()
  "Return the label of every code cell bar of the buffer, in order.
The bars of rendered markdown cells are left out: whether a cell renders
at all depends on a converter being installed.
The label is the text before the stretch that holds the icons at the
window edge, without the leading glyph."
  (mapcar (lambda (ov)
            (let* ((text (or (overlay-get ov 'overblock-bar-text) ""))
                   (stretch (text-property-not-all 0 (length text)
                                                   'display nil text))
                   (label (substring-no-properties text 0 stretch)))
              (string-trim (substring label (1+ (string-search " " label))))))
          ;; Not `sort' with keywords, which is Emacs 30.
          (sort (seq-filter (lambda (ov)
                              (eq (overlay-get ov 'overblock-bar) 'code))
                            (overblock-bars))
                (lambda (a b) (< (overlay-start a) (overlay-start b))))))

;;;; Helpers

(defun overblock-bar-on-line ()
  "Return the bar overlay of the line point is on, or nil.
A helper of these tests; the package asks `overblock-bar-in' for a
region."
  (overblock-bar-in (pos-bol) (min (point-max) (1+ (pos-eol)))))

(defun overblock-run-body-lines-of-pycell (lines)
  "Return the leading LINES of a result, as the runner's limits bound them.
The runner takes the budgets as arguments, so the tests supply them."
  (overblock-run--body-lines
   (overblock-repl-first-lines (string-join lines "\n") overblock-run-max-lines)
   overblock-run-max-line-length))

(defun overblock-run-header-of-pycell (folded total shown runtime state imagep)
  "Return the header bar of a result of the notebook.
FOLDED, TOTAL, SHOWN, RUNTIME, STATE and IMAGEP are the arguments of
`overblock-run-header'; the backend is that of the notebook."
  (let ((overblock-run-backend (overblock-pycell--backend)))
    (overblock-run-header folded total shown runtime state imagep)))

(ert-deftest overblock-pycell-test-clean-prompts ()
  "Prompts at both ends and Out[n] markers go, and the trailing blanks.
The indentation of the first line stays: the columns of an aligned
table, such as a `describe', line up on it."
  (let ((comint-prompt-regexp "^\\(?:>>> \\|In \\[[0-9]+\\]: \\)"))
    (should (equal (overblock-pycell--clean ">>> 2\n>>> ") "2"))
    (should (equal (overblock-pycell--clean "a\nOut[3]: 42\n") "a\n42"))
    (should (equal (overblock-pycell--clean "  x  ") "  x"))
    (should (equal (overblock-pycell--clean "\n\n  x  ") "  x"))
    (should (equal (overblock-pycell--clean "   ") ""))
    (should (equal (overblock-pycell--clean "no prompts") "no prompts"))))

(ert-deftest overblock-pycell-test-clean-terminates-on-empty-prompt ()
  "A prompt regexp that matches the empty string must not loop forever."
  (let ((comint-prompt-regexp "^"))
    (should (equal (overblock-pycell--clean "text") "text"))))

(ert-deftest overblock-pycell-test-clean-keeps-a-leading-image ()
  "A figure that is the whole output survives the prompt strip.
comint-mime renders an image as one space with a display property,
which the whitespace before a prompt must not take along."
  (let* ((comint-prompt-regexp "^\\(?:>>> \\|In \\[[0-9]+\\]: \\)")
         (result (overblock-pycell--clean (concat overblock-test-common-image "\n\nIn [5]: "))))
    (should (= (length result) 1))
    (should (overblock-image-in result))
    ;; A prompt with nothing before it still goes.
    (should (equal (overblock-pycell--clean "In [5]: 42") "42"))))

(ert-deftest overblock-pycell-test-clean-keeps-images ()
  "Whitespace that carries a display property is part of the result."
  (let* ((comint-prompt-regexp "^>>> ")
         (result (overblock-pycell--clean (concat "plot\n" overblock-test-common-image "\n"))))
    (should (equal result (concat "plot\n" overblock-test-common-image)))
    (should (get-text-property (1- (length result)) 'display result))))

;;;; Tests

(ert-deftest overblock-pycell-test-body-lines-cap ()
  "At most `overblock-run-max-lines' lines show inline."
  (let ((overblock-run-max-lines 3)
        (lines '("1" "2" "3" "4" "5")))
    (should (equal (overblock-run-body-lines-of-pycell lines) '("1" "2" "3")))))

(ert-deftest overblock-pycell-test-body-lines-stop-after-image ()
  "Nothing after the first image line shows inline."
  (cl-letf (((symbol-function 'display-images-p) (lambda (&rest _) t)))
    (let ((overblock-run-max-lines 10))
      (should (equal (overblock-run-body-lines-of-pycell (list "text" overblock-test-common-image "more"))
                     (list "text" overblock-test-common-image))))))

(ert-deftest overblock-pycell-test-body-lines-run-on-without-images ()
  "A display that cannot draw an image has nothing to stop for.
In a terminal the image is only a space, so stopping there would hide
the rest of the output and save no height.  The image is named instead,
so the row is not blank."
  (cl-letf (((symbol-function 'display-images-p) (lambda (&rest _) nil)))
    (let ((overblock-run-max-lines 10))
      (should (equal (overblock-run-body-lines-of-pycell
                      (list "before" overblock-test-common-image "after"))
                     (list "before" "[figure]" "after"))))))

(ert-deftest overblock-pycell-test-md-an-empty-cell-renders-nothing ()
  "A markdown cell with no body signals nothing and leaves nothing.
A `# %% [markdown]' line with the next boundary under it is a new
cell.  `overblock-show' returns nil for a region of no length.  An
error here would come from the idle timer or the comint filter, and a
bar left on the line could not be swept."
  (with-temp-buffer
    (insert "# %% [markdown]\n# %%\nprint(1)\n")
    (python-mode)
    (code-cells-mode)
    (should-not (overblock-pycell--md-show 17 17))
    (should-not (seq-filter #'overblock-bar-kind
                            (overlays-in (point-min) (point-max))))))

(ert-deftest overblock-pycell-test-md-a-failed-cell-shows-its-markdown ()
  "A cell the converter fails on shows its markdown, and goes no more."
  (with-temp-buffer
    (insert "# %% [markdown]\n# Some *text*.\n# %%\nprint(1)\n")
    (python-mode)
    (code-cells-mode)
    (cl-letf (((symbol-function 'overblock-md-rendered) #'ignore))
      (let ((block (overblock-pycell--md-show 17 31)))
        (should block)
        (should (string-match-p "Some \\*text\\*"
                                (overblock-get block :over)))))))

(ert-deftest overblock-pycell-test-the-doc-strings-keep-their-setting ()
  "Turning one of the two modes of a notebook off leaves the other's setting."
  (with-temp-buffer
    (insert "# %%\nx = 1\n")
    (python-mode)
    (code-cells-mode)
    (overblock-pycell-test--with-mode
      (overblock-pydoc-mode 1)
      (overblock-pydoc-mode -1)
      (should-not overblock-live-source-at-point)
      (overblock-pydoc-mode 1)
      (overblock-pycell-mode -1)
      (should-not overblock-live-source-at-point)
      (overblock-pydoc-mode -1)
      (should overblock-live-source-at-point))))

(ert-deftest overblock-pycell-test-md-an-anchor-link-finds-a-cell-heading ()
  "A #slug link finds the heading of a markdown cell, not a comment."
  (with-temp-buffer
    (insert "# a comment\nx = 1\n# %% [markdown]\n# # A comment\n")
    (python-mode)
    (code-cells-mode)
    (overblock-pycell-test--with-mode
      (goto-char (point-min))
      (overblock-md-browse "#a-comment")
      (should (looking-at-p "# # A comment")))))

(ert-deftest overblock-pycell-test-md-an-edit-takes-the-bar-with-it ()
  "An edit of a rendered cell removes the rendering and its bar.
The block evaporates with the text it covers, but the bar is on the
boundary line above, which no edit of the cell reaches.  So the
modification hook removes it, and a new rendering draws one bar, not
two."
  (skip-unless (overblock-md-program))
  (with-temp-buffer
    (insert "# %% [markdown]\n# ## A\n#\n# Text.\n\n# %%\nx = 1\n")
    (python-mode)
    (code-cells-mode)
    (setq-local overblock-run-backend (overblock-pycell--backend))
    (let ((bars (lambda ()
                  (seq-count (lambda (ov) (overlay-get ov 'overblock-pycell-main))
                             (overlays-in (point-min) (point-max))))))
      (overblock-pycell-test--render-all)
      (should (= (funcall bars) 1))
      (pcase-let* ((block (car (overblock-in (point-min) (point-max)
                                             'markdown)))
                   (`(,beg . ,end) (overblock-get block :data)))
        (goto-char beg)
        (delete-region beg end)
        (insert "# ## A\n#\n# Text and more.\n\n")
        (should-not (overblock-in (point-min) (point-max) 'markdown))
        (should (= (funcall bars) 0))
        ;; Rendering again leaves one bar, not two.
        (overblock-pycell--md-show beg (point))
        (should (= (funcall bars) 1))))))

(ert-deftest overblock-pycell-test-show-text-result ()
  "A text result rides the newline, and the buffer text stays as it was.
The body is a display string on the newline that ends the cell, the
cheapest place for plain text.  The header is an overlay string on the
anchor, because a bar puts its icons at the window edge, which a
display string cannot."
  (overblock-pycell-test--with-cells
    (let ((before (buffer-substring-no-properties (point-min) (point-max))))
      (pcase-let ((`(,beg ,end) (code-cells--bounds nil nil t)))
        (overblock-run-show beg end "42" 0.5)
        (let* ((block (car (overblock-in (point-min) (point-max) 'result)))
               (nl (overblock-get block :newline)))
          (should block)
          ;; The body is the display string of the newline.
          (should (string-match-p "42" (overlay-get nl 'display)))
          ;; The header is an overlay string on the anchor.
          (should (string-match-p "line" (overlay-get block 'after-string)))))
      (should (equal (buffer-substring-no-properties (point-min) (point-max))
                     before)))))

(ert-deftest overblock-pycell-test-show-image-result ()
  "An image result rides the after-string of the anchor, images and all.
A display string swallows an image, so those rows go into an overlay
string; the newline keeps its own character, and the wheel can pass
the block.

This needs a display that draws images: on a terminal the figure is
named instead and the body is the display property."
  (cl-letf (((symbol-function 'display-images-p) (lambda (&rest _) t)))
    (overblock-pycell-test--with-cells
      (pcase-let ((`(,beg ,end) (code-cells--bounds nil nil t)))
        (overblock-run-show beg end (concat "plot\n" overblock-test-common-image) 0.5)
        (let* ((block (car (overblock-in (point-min) (point-max) 'result)))
               (nl (overblock-get block :newline)))
          (should (overblock-image-in (overlay-get block 'after-string)))
          (should-not (overlay-get nl 'display)))))))

(ert-deftest overblock-pycell-test-a-figure-is-named-in-a-terminal ()
  "A figure a terminal cannot draw is named, in the block and out of it.
comint-mime sends one space that carries the image, and a display
without images shows only the space, in the block and in
`overblock-run-pop-output'."
  (cl-letf (((symbol-function 'display-images-p) (lambda (&rest _) nil)))
    (overblock-pycell-test--with-cells
      (pcase-let ((`(,beg ,end) (code-cells--bounds nil nil t)))
        (overblock-run-show beg end (concat "plot\n" overblock-test-common-image) 0.5)
        (let ((block (car (overblock-in (point-min) (point-max) 'result))))
          (should (string-match-p
                   "\\[figure\\]"
                   (concat (overlay-get block 'after-string)
                           (overlay-get (overblock-get block :newline)
                                        'display)))))))))

(ert-deftest overblock-pycell-test-raised-text-is-not-an-image ()
  "Superscripts do not push a result onto the string path.
shr raises a superscript with a display property, and inline math is
full of those; only a real image belongs in the after-string."
  (overblock-pycell-test--with-cells
    (pcase-let ((`(,beg ,end) (code-cells--bounds nil nil t)))
      (overblock-run-show beg end
                    (concat "E = mc"
                            (propertize "2" 'display '(raise 0.2)))
                    0.1)
      (let* ((block (car (overblock-in (point-min) (point-max) 'result)))
             (nl (overblock-get block :newline)))
        ;; The rows are one display string on the newline.
        (should (overlay-get nl 'display))
        (should (string-match-p "mc" (overlay-get nl 'display)))))))

(ert-deftest overblock-pycell-test-body-lines-keep-raised-text ()
  "Raised text does not cut the inline part short; an image does."
  (let ((overblock-run-max-lines 10))
    (should (equal (overblock-run-body-lines-of-pycell
                    (list "x" (propertize "2" 'display '(raise 0.2)) "y"))
                   (list "x" (propertize "2" 'display '(raise 0.2)) "y")))))

(ert-deftest overblock-pycell-test-a-finished-result-keeps-its-count ()
  "A result counts its lines once and keeps the number.
A finished cell arrives without a count, and a fold must not scan the
whole output again on every keypress."
  (overblock-pycell-test--with-cells
    (pcase-let ((`(,beg ,end) (code-cells--bounds nil nil t)))
      (overblock-run-show beg end "one\ntwo\nthree" 0.1)
      (let ((block (car (overblock-in (point-min) (point-max) 'result))))
        ;; The count is in the record, where the header reads it.
        (should (= (plist-get (overblock-get block :data) :total) 3))
        (should (string-match-p "3 lines"
                                (overlay-get block 'after-string)))
        ;; A fold keeps it.
        (overblock-run-toggle-output)
        (should (= (plist-get (overblock-get block :data) :total) 3))
        (should (string-match-p "3 lines"
                                (overlay-get block 'after-string)))))))

(ert-deftest overblock-pycell-test-show-keeps-fold-state ()
  "Replacing a result keeps whether it was folded."
  (overblock-pycell-test--with-cells
    (pcase-let ((`(,beg ,end) (code-cells--bounds nil nil t)))
      (overblock-run-show beg end "a\nb" 0.1)
      (let ((ov (car (overblock-in (point-min) (point-max) 'result))))
        (overblock-set ov :data (plist-put (overblock-get ov :data)
                                           :folded t)))
      (overblock-run-show beg end "c\nd" 0.2)
      (let ((ov (car (overblock-in (point-min) (point-max) 'result))))
        (should (plist-get (overblock-get ov :data) :folded))
        ;; The result is the new one.
        (should (equal (overblock-run--result-text ov) "c\nd"))))))

(ert-deftest overblock-pycell-test-remove-overlays ()
  "Removing results takes the helper overlays with them."
  (overblock-pycell-test--with-cells
    (pcase-let ((`(,beg ,end) (code-cells--bounds nil nil t)))
      (overblock-run-show beg end "42" 0.1))
    (let ((bov (overblock-get
                (car (overblock-in (point-min) (point-max) 'result))
                :newline)))
      (overblock-run-clear-results)
      (should-not (overblock-in (point-min) (point-max) 'result))
      (should-not (overlay-buffer bov)))))

(ert-deftest overblock-pycell-test-fold-keeps-result ()
  "An outline fold hides the code and leaves the result in place.
The block below the fold keeps its own fold button, so the two fold
separately."
  (overblock-pycell-test--with-cells
    (overblock-pycell-test--with-mode
      (pcase-let ((`(,beg ,end) (code-cells--bounds nil nil t)))
        (overblock-run-show beg end "a\nb" 0.1)
        (let* ((ov (car (overblock-in (point-min) (point-max) 'result)))
               (bov (overblock-get ov :newline))
               (head (overlay-get ov 'after-string))
               (body (overlay-get bov 'display)))
          (outline-flag-region beg (1- end) t)
          (should (equal (overlay-get ov 'after-string) head))
          (should (equal (overlay-get bov 'display) body))
          (outline-flag-region beg (1- end) nil)
          (should (equal (overlay-get bov 'display) body)))))))

(ert-deftest overblock-pycell-test-fold-shrinks-at-buffer-end ()
  "A fold to the end of the buffer stops before the newline of the block.
Only there does `outline-flag-region' cover it; in the middle of the
buffer it stops one character short by itself."
  (with-temp-buffer
    (insert "# %%\nx = 1\ny = x + 1\n")
    (python-mode)
    (code-cells-mode)
    (setq-local overblock-run-backend (overblock-pycell--backend))
    (overblock-pycell-test--with-mode
      (pcase-let ((`(,beg ,end) (progn (goto-char (point-min))
                                       (code-cells--bounds nil nil t))))
        (overblock-run-show beg end "42" 0.1)
        (let ((bov (overblock-get
                    (car (overblock-in (point-min) (point-max) 'result))
                    :newline)))
          (outline-flag-region beg (point-max) t)
          (should-not
           (seq-some (lambda (o) (and (eq (overlay-get o 'invisible) 'outline)
                                      (> (overlay-end o) (overlay-start bov))))
                     (overlays-in (overlay-start bov)
                                  (overlay-end bov)))))))))

(ert-deftest overblock-pycell-test-fold-md-round-trip ()
  "An outline fold takes a markdown block along, and gives it back."
  (skip-unless (overblock-md-program))
  (with-temp-buffer
    (insert "# %% [markdown]\n# ## A\n#\n# Text here.\n\n# %%\ny = 2\n")
    (python-mode)
    (code-cells-mode)
    (setq-local overblock-run-backend (overblock-pycell--backend))
    (overblock-pycell-test--with-mode
      (goto-char (point-min))
      (let* ((block (car (overblock-in (point-min) (point-max) 'markdown)))
             ;; A fold makes new pieces, so they are read each time.
             (shown (lambda ()
                      (seq-some (lambda (p) (not (overlay-get p 'invisible)))
                                (overblock-get block :parts)))))
        (should (overblock-get block :parts))
        (should (funcall shown))
        (outline-flag-region (pos-eol) (overlay-end block) t)
        (should-not (funcall shown))
        (outline-flag-region (pos-eol) (overlay-end block) nil)
        (should (funcall shown))))))

(ert-deftest overblock-pycell-test-md-keeps-its-lines ()
  "A rendered markdown cell stays as many lines as its source.
One display string for the whole cell would make it one line, laid out
whole on every scroll event."
  (skip-unless (overblock-md-program))
  (with-temp-buffer
    (insert "# %% [markdown]\n# ## A\n#\n# Text here.\n\n# %%\ny = 2\n")
    (python-mode)
    (code-cells-mode)
    (setq-local overblock-run-backend (overblock-pycell--backend))
    (overblock-pycell-test--render-all)
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

(ert-deftest overblock-pycell-test-md-cloak-starts-on-a-newline ()
  "A hidden run starts at the end of a visible line, never at a start.
`scroll-down' signals a beginning-of-buffer error over a run that
starts a line, and a piece with nothing to show would leave a line of
no height, with the same effect."
  (skip-unless (overblock-md-program))
  (with-temp-buffer
    (insert "# %% [markdown]\n# ## A\n#\n#\n#\n# Text here.\n#\n#\n\n# %%\ny = 2\n")
    (python-mode)
    (code-cells-mode)
    (setq-local overblock-run-backend (overblock-pycell--backend))
    (overblock-pycell-test--render-all)
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


(ert-deftest overblock-pycell-test-md-comment-round-trip ()
  "Commenting and uncommenting a markdown cell is lossless."
  (let ((md "# Title\n\nSome *text*.\n\nMore."))
    (should (equal (overblock-pycell--md-uncomment (overblock-pycell--md-comment md)) md))
    (should (equal (overblock-pycell--md-comment "a\n\nb") "# a\n#\n# b"))))

(ert-deftest overblock-pycell-test-md-cell-start-needs-the-boundary-line ()
  "A markdown cell is recognized by the boundary line above its body."
  (with-temp-buffer
    (insert "# %% [markdown]\n# Title\n")
    (goto-char (point-min))
    (forward-line 1)
    (should (overblock-pycell--md-cell-start (point)))
    (should (= (overblock-pycell--md-cell-start (point)) (point-min))))
  (with-temp-buffer
    (insert "# %%\nx = 1\n")
    (goto-char (point-min))
    (forward-line 1)
    (should-not (overblock-pycell--md-cell-start (point)))))

(ert-deftest overblock-pycell-test-dedicated-asks-no-project-question ()
  "A shell is dedicated as the reader asked, and asks nothing.
For `project', `run-python' calls `project-current' with a prompt, so
a file outside a project would stop a queued run with a question.
`python-shell-get-process-name' names such a shell the shared one, and
so does this."
  (let ((python-shell-dedicated nil))
    (should-not (overblock-pycell--dedicated)))
  (let ((python-shell-dedicated 'buffer))
    (should (eq (overblock-pycell--dedicated) 'buffer)))
  (let ((python-shell-dedicated 'project))
    ;; Inside a project the setting stands.
    (cl-letf (((symbol-function 'project-current) (lambda (&rest _) '(vc Git "/tmp/"))))
      (should (eq (overblock-pycell--dedicated) 'project)))
    ;; Outside one it is the shared shell, not a question.
    (cl-letf (((symbol-function 'project-current) #'ignore))
      (should-not (overblock-pycell--dedicated)))))

(ert-deftest overblock-pycell-test-cold-cell-belongs-to-its-buffer ()
  "The cell that waits for a cold interpreter is marked in its own buffer.
`copy-marker' of a number uses the current buffer, so markers made in
the shell buffer would send its start-up banner as the cell."
  (let* ((shell (generate-new-buffer "*overblock-pycell test shell*"))
         (proc (make-pipe-process :name "overblock-pycell test" :buffer shell
                                  :noquery t :filter #'ignore))
         notebook sent)
    (unwind-protect
        (overblock-pycell-test--with-cells
          (overblock-pycell-test--with-mode
            (setq notebook (current-buffer))
            (with-current-buffer shell
              (insert "Python 3.14.6 | packaged by conda-forge\nIn [1]: "))
            (pcase-let ((`(,beg ,end) (code-cells--bounds nil nil t)))
              (cl-letf (((symbol-function 'python-shell-get-process)
                         (lambda (&rest _) nil))
                        ((symbol-function 'python-shell-get-process-or-error)
                         (lambda (&rest _) proc))
                        ((symbol-function 'run-python) (lambda (&rest _) shell)))
                (overblock-pycell-eval-region beg end)))
            ;; The cell waits for the first prompt of the shell. Let it
            ;; arrive.
            (should (buffer-local-value 'python-shell-first-prompt-hook shell))
            (cl-letf (((symbol-function 'python-shell-get-process)
                       (lambda (&rest _) proc))
                      ((symbol-function 'overblock-run--send)
                       (lambda (_proc beg end)
                         (setq sent (list (marker-buffer beg)
                                          (marker-buffer end))))))
              ;; Only what the package armed: the members of python.el
              ;; talk to the interpreter, which here is a pipe that
              ;; ignores them, and they would wait for ever on Emacs 29.
              (with-current-buffer shell
                (mapc #'funcall (remq t python-shell-first-prompt-hook))))
            (should (equal sent (list notebook notebook)))))
      (delete-process proc)
      (kill-buffer shell))))

(ert-deftest overblock-pycell-test-clean-strips-a-prompt-on-the-same-line ()
  "A prompt that follows output on one line goes too.
Output that ends without a newline leaves the prompt of the shell on
the same line, and `comint-prompt-regexp' anchors to a line start."
  (with-temp-buffer
    (setq-local comint-prompt-regexp "^\\(>>> \\|In \\[[0-9]+\\]: \\)")
    (should (equal (overblock-pycell--clean "abc>>> ") "abc"))
    (should (equal (overblock-pycell--clean "a\nb\n\nIn [9]: ") "a\nb"))
    (should (equal (overblock-pycell--clean ">>> ") ""))
    ;; Nothing to take off.
    (should (equal (overblock-pycell--clean "a\nb") "a\nb"))))

(ert-deftest overblock-pycell-test-filter-copies-all-the-output ()
  "The finished cell gets everything the shell printed.
`comint-last-prompt' cannot be the end of the region: comint calls any
last line without a newline a prompt, so after a split chunk it is
inside the output."
  (let ((shell (generate-new-buffer "*overblock-pycell test shell*"))
        ended)
    (unwind-protect
        (with-current-buffer shell
          (setq-local comint-prompt-regexp "^\\(>>> \\|In \\[[0-9]+\\]: \\)")
          (insert "In [1]: ")
          (let ((start (point-max-marker)))
            (insert "line 0\nline 1\nline 2\n\nIn [2]: ")
            ;; As comint leaves it after a split chunk: inside the output.
            (setq-local comint-last-prompt
                        (cons (copy-marker (+ start 7)) (copy-marker (+ start 13))))
            (setq overblock-run--state (list :from start :tail "" :start (float-time)))
            ;; The filter reads the backend of the shell.
            (setq-local overblock-run-backend (overblock-pycell--backend))
            (cl-letf (((symbol-function 'python-shell-comint-end-of-output-p)
                       (lambda (&rest _) t))
                      ((symbol-function 'overblock-run--end)
                       (lambda (text &rest _) (setq ended text))))
              (overblock-run--filter "\nIn [2]: "))))
      (kill-buffer shell))
    (should (equal (substring-no-properties (or ended ""))
                   "line 0\nline 1\nline 2"))))

(ert-deftest overblock-pycell-test-at-point-survives-a-frame-switch ()
  "An event that carries no place leaves point alone instead of failing.
The commands read their event from `last-input-event', so it can be
any event.  A `switch-frame' is a cons like a click, but its start is
a frame, which has no position."
  (overblock-pycell-test--with-cells
    (pcase-let ((`(,beg ,end) (code-cells--bounds nil nil t)))
      (overblock-run-show beg end "42" 0.1))
    (goto-char (point-min))
    (forward-line 1)
    (let ((here (point)))
      (overblock-goto-event (list 'switch-frame (selected-frame)))
      (should (eq (overblock-at 'result)
                  (progn (overblock-goto-event nil)
                         (overblock-at 'result))))
      (should (= (point) here)))))

(defun overblock-pycell-test--ipython-syntax-p (code)
  "Return non-nil when CODE would go to the reader of IPython."
  (with-temp-buffer
    (insert code)
    (python-mode)
    (overblock-pycell--ipython-syntax-p (point-min) (point-max))))

(ert-deftest overblock-pycell-test-ipython-syntax ()
  "Magics, shell escapes and help are told apart from plain Python.
Where the character means something to Python, the cell stays Python:
a shell without IPython would answer with a NameError."
  (let ((ipython #'overblock-pycell-test--ipython-syntax-p))
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
    ;; A comment can ask a question.
    (should-not (funcall ipython "# is this right?\nprint('yes')"))
    ;; A continuation line can start with a modulo.
    (should-not (funcall ipython "total = (1\n         % 2)"))
    ;; A docstring can do either.
    (should-not (funcall ipython "s = \"\"\"why?\nmore\"\"\"\n"))
    (should-not (funcall ipython "s = \"\"\"a\n% b\n\"\"\"\n"))))

(ert-deftest overblock-pycell-test-send-to-ipython-carries-the-source ()
  "The cell reaches `run_cell' as it was written, quotes and all.
It travels base64 encoded for that reason, and the trailing None keeps
the result object out of the block."
  (let ((code "%time f('a\"b')\nx = 1\n")
        sent)
    (cl-letf (((symbol-function 'python-shell-send-string)
               (lambda (string &rest _) (setq sent string))))
      (overblock-pycell--send-to-ipython nil code))
    (should (string-match "b64decode(\"\\([^\"]+\\)\")" sent))
    (should (equal (decode-coding-string
                    (base64-decode-string (match-string 1 sent)) 'utf-8)
                   code))
    (should (string-suffix-p "None\n" sent))))

(ert-deftest overblock-pycell-test-md-commit-keeps-the-gap ()
  "Committing an edit that changed nothing leaves the file alone.
A cell reaches to the next boundary line, so the blank line jupytext
writes between cells belongs to it and has to be written back."
  (skip-unless (overblock-md-program))
  (let ((text "# %% [markdown]\n# ## Heading\n#\n# The prose.\n\n# %%\nx = 1\n")
        (notebook (generate-new-buffer "*overblock-pycell test notebook*"))
        edit)
    (unwind-protect
        (progn
          (with-current-buffer notebook
            (insert text)
            (python-mode)
            (code-cells-mode)
            (setq-local overblock-run-backend (overblock-pycell--backend))
            (overblock-pycell-test--render-all)
            (goto-char (point-min))
            (forward-line 1)
            (let ((prefix (format "*overblock-pycell md: %s:" (buffer-name))))
              ;; The edit buffer is named after the cell, so it is found
              ;; by its prefix.
              (save-window-excursion (overblock-pycell-md-edit))
              (setq edit (seq-find (lambda (b)
                                     (string-prefix-p prefix (buffer-name b)))
                                   (buffer-list)))))
          (should edit)
          ;; `overblock-edit-commit' quits its window, and the edit
          ;; buffer is not shown here, so that would kill the buffer of
          ;; the selected window.
          (cl-letf (((symbol-function 'quit-window) #'ignore))
            (with-current-buffer edit (overblock-edit-commit)))
          (with-current-buffer notebook
            (should (equal (buffer-substring-no-properties (point-min) (point-max))
                           text))
            ;; and the commit renders the cell again
            (overblock-pycell-test--settle)
            (should (overblock-in (point-min) (point-max) 'markdown))))
      (when (buffer-live-p edit) (kill-buffer edit))
      (kill-buffer notebook))))

(ert-deftest overblock-pycell-test-body-lines-cut-a-long-line ()
  "A line longer than the cap is cut and the cut is marked.
The line cap does not bound one long line, and a block costs what it
holds on every redisplay."
  (let ((overblock-run-max-lines 12)
        (overblock-run-max-line-length 10))
    (should (equal (overblock-run-body-lines-of-pycell (list "short" (make-string 30 ?x)))
                   (list "short" (concat (make-string 10 ?x)
                                         (overblock-glyph "…" "...")))))
    ;; A line with an image keeps every character: the image can be past
    ;; the cut. Only where the display draws images; in a terminal the
    ;; image is a space and the line is cut.
    (cl-letf (((symbol-function 'display-images-p) (lambda (&rest _) t)))
      (let ((line (concat (make-string 30 ?x) overblock-test-common-image)))
        (should (equal (overblock-run-body-lines-of-pycell (list line)) (list line))))))
  ;; Zero cuts nothing.
  (let ((overblock-run-max-lines 12)
        (overblock-run-max-line-length 0))
    (should (equal (overblock-run-body-lines-of-pycell (list (make-string 30 ?x)))
                   (list (make-string 30 ?x))))))

(ert-deftest overblock-pycell-test-md-without-a-converter-stays-plain ()
  "A markdown cell without a converter stays text, and raises nothing.
Evaluating a single cell reaches the converter through
`overblock-md-rendered', which checks for the program itself, so every
caller gets the check."
  (let ((overblock-md-command "definitely-not-installed-42")
        (text "# %% [markdown]\n# ## A heading\n\n# %%\nx = 1\n"))
    (should-not (overblock-md-program))
    (should-not (overblock-md-rendered "## A heading"))
    (with-temp-buffer
      (insert text)
      (python-mode)
      (code-cells-mode)
      (setq-local overblock-run-backend (overblock-pycell--backend))
      (goto-char (point-min))
      (pcase-let ((`(,beg ,end) (code-cells--bounds nil nil t)))
        (overblock-pycell-eval-region beg end))
      (should-not (overblock-in (point-min) (point-max)))
      (should (equal (buffer-substring-no-properties (point-min) (point-max))
                     text)))))

(ert-deftest overblock-pycell-test-md-program-needs-libxml ()
  "Without the parser there is no converter worth naming.
shr reads the HTML of the converter with `libxml-parse-html-region',
which an Emacs built without libxml2 does not have.  The mode then
says so and leaves the cells as text."
  (cl-letf (((symbol-function 'libxml-parse-html-region) nil))
    (should-not (fboundp 'libxml-parse-html-region))
    (should-not (overblock-md-program))
    ;; Rendering says so instead of failing.
    (with-temp-buffer
      (insert "# %% [markdown]\n# ## Heading\n#\n# Prose.\n\n# %%\nx = 1\n")
      (python-mode)
      (code-cells-mode)
      (setq-local overblock-run-backend (overblock-pycell--backend))
      (let ((before (buffer-string))
            said)
        (cl-letf (((symbol-function 'message)
                   (lambda (fmt &rest args)
                     (setq said (and fmt (apply #'format fmt args))))))
          (overblock-pycell-mode 1)
          (overblock-pycell-mode -1))
        (should (string-match-p "libxml" said))
        (should (equal before (buffer-string)))
        (should-not (overblock-in (point-min) (point-max) 'markdown))))))

(ert-deftest overblock-pycell-test-fold-md-image-at-buffer-end ()
  "A cell with an image folds even where the buffer ends without one.
The pieces of such a cell hang on its source lines, and the fold stops
one character short of the last newline, so the last piece must be
hidden too.  This includes a cell at the end of a buffer without a
final newline."
  (skip-unless (overblock-md-program))
  (dolist (trailing '("\n" ""))
    (with-temp-buffer
      (insert "# %% [markdown]\n# ## Prose\n#\n# Words.\n\n"
              "# %%\nx = 1\n\n"
              "# %% [markdown]\n# ## A figure\n#\n# ![pic](pic.png)" trailing)
      (python-mode)
      (code-cells-mode)
      (setq-local overblock-run-backend (overblock-pycell--backend))
      (overblock-pycell-test--with-mode
        ;; The blocks in order; the last one holds the figure. Not
        ;; `sort' with keywords, which is Emacs 30.
        (let* ((blocks (sort (overblock-in (point-min) (point-max) 'markdown)
                             (lambda (a b)
                               (< (overlay-start a) (overlay-start b)))))
               (last (car (last blocks)))
               ;; What the cell shows: the pieces that are not cloaks.
               (shown (lambda ()
                        (seq-count
                         (lambda (p)
                           (and (not (overlay-get p 'overblock-cloak))
                                (not (overlay-get p 'invisible))))
                         (overblock-get last :parts)))))
          (should last)
          (should (> (funcall shown) 0))
          (outline-flag-region (point-min) (point-max) t)
          (should (= (funcall shown) 0))
          (outline-flag-region (point-min) (point-max) nil)
          (should (> (funcall shown) 0)))))))

(ert-deftest overblock-pycell-test-show-gives-the-last-cell-a-newline ()
  "The block needs a newline to hang on, and the last cell may lack one.
This is the only change the package makes to a buffer: one newline,
only where there is none, and only at the end of the buffer."
  (with-temp-buffer
    (insert "# %%
x = 1")                ; no newline at the end
    (python-mode)
    (code-cells-mode)
    (setq-local overblock-run-backend (overblock-pycell--backend))
    (goto-char (point-min))
    (pcase-let ((`(,beg ,end) (code-cells--bounds nil nil t)))
      (overblock-run-show beg end "42" 0.1))
    (should (equal (buffer-string) "# %%
x = 1
"))
    (should (overblock-in (point-min) (point-max) 'result)))
  ;; A buffer that ends with one is left alone.
  (with-temp-buffer
    (insert "# %%
x = 1
")
    (python-mode)
    (code-cells-mode)
    (setq-local overblock-run-backend (overblock-pycell--backend))
    (goto-char (point-min))
    (pcase-let ((`(,beg ,end) (code-cells--bounds nil nil t)))
      (overblock-run-show beg end "42" 0.1))
    (should (equal (buffer-string) "# %%
x = 1
"))))

(ert-deftest overblock-pycell-test-md-commit-keeps-an-empty-cell-empty ()
  "Committing an empty cell writes nothing where there was nothing.
An empty text has no line to comment, and `overblock-pycell--md-comment'
would write a bare #."
  (skip-unless (overblock-md-program))
  (let ((text "# %% [markdown]\n\n# %%\nx = 1\n")
        (notebook (generate-new-buffer "*overblock-pycell test notebook*"))
        edit)
    (unwind-protect
        (progn
          (with-current-buffer notebook
            (insert text)
            (python-mode)
            (code-cells-mode)
            (setq-local overblock-run-backend (overblock-pycell--backend))
            (overblock-pycell-test--render-all)
            (goto-char (point-min))
            (forward-line 1)
            (let ((prefix (format "*overblock-pycell md: %s:" (buffer-name))))
              (save-window-excursion (overblock-pycell-md-edit))
              (setq edit (seq-find (lambda (b)
                                     (string-prefix-p prefix (buffer-name b)))
                                   (buffer-list)))))
          (should edit)
          ;; `overblock-edit-commit' quits its window, and the edit
          ;; buffer is not shown here, so that would kill the buffer of
          ;; the selected window.
          (cl-letf (((symbol-function 'quit-window) #'ignore))
            (with-current-buffer edit (overblock-edit-commit)))
          (with-current-buffer notebook
            (should (equal (buffer-substring-no-properties (point-min) (point-max))
                           text))))
      (when (buffer-live-p edit) (kill-buffer edit))
      (kill-buffer notebook))))

(ert-deftest overblock-pycell-test-md-edit-keeps-another-cell-s-edit ()
  "Opening the edit of a second cell leaves the edit of the first alone.
Each region has its own edit buffer, so unsaved text stays."
  (skip-unless (overblock-md-program))
  (let ((notebook (generate-new-buffer "*overblock-pycell test notebook*"))
        first second)
    (unwind-protect
        (with-current-buffer notebook
          (insert "# %% [markdown]\n# First cell.\n\n"
                  "# %% [markdown]\n# Second cell.\n")
          (python-mode)
          (code-cells-mode)
          (setq-local overblock-run-backend (overblock-pycell--backend))
          (goto-char (point-min))
          (overblock-pycell-test--render-all)
          (forward-line 1)
          (save-window-excursion (overblock-pycell-md-edit))
          (setq first (format "*overblock-pycell md: %s:2*" (buffer-name)))
          (setq second (format "*overblock-pycell md: %s:5*" (buffer-name)))
          (with-current-buffer first
            (goto-char (point-max))
            (insert "An hour of unsaved writing."))
          (goto-char (point-min))
          (forward-line 4)
          (save-window-excursion (overblock-pycell-md-edit))
          (should (get-buffer second))
          (should (string-match-p "Second cell"
                                  (with-current-buffer second (buffer-string))))
          (should (string-match-p "An hour of unsaved writing"
                                  (with-current-buffer first (buffer-string))))
          ;; Coming back to a cell being edited returns the edit, not
          ;; the text of the file.
          (goto-char (point-min))
          (forward-line 1)
          (save-window-excursion (overblock-pycell-md-edit))
          (should (string-match-p "An hour of unsaved writing"
                                  (with-current-buffer first (buffer-string)))))
      (dolist (name (list first second))
        (when (and name (get-buffer name)) (kill-buffer name)))
      (kill-buffer notebook))))

(ert-deftest overblock-pycell-test-output-head-stops-at-a-budget ()
  "A cell printing much on few lines reads only what can show.
Not where an escape sequence would be cut in two: comint-mime sends an
image as one, and a cut inside it drops the figure."
  (with-temp-buffer
    (setq-local comint-prompt-regexp "^In \\[[0-9]+\\]: ")
    (setq-local overblock-run-backend (overblock-pycell--backend))
    (insert "In [1]: ")
    (let* ((from (copy-marker (point)))
           ;; What the body can show.
           (budget (* overblock-run-max-lines (1+ overblock-run-max-line-length))))
      (setq-local overblock-run--state (list :from from :beg (point-min-marker)
                                    :end (point-max-marker) :tail ""
                                    :start (float-time)))
      ;; One line, longer than the budget: the head stops at it.
      (insert (make-string (* 3 budget) ?x))
      (should (= (length (overblock-run-output-head from)) budget))
      ;; The same output with an escape sequence inside the budget: all
      ;; of it is read, so the image of comint-mime arrives whole.
      (setq overblock-run--state (plist-put overblock-run--state :head nil))
      (goto-char (+ from 10))
      (insert "\e]5151;file=x\e\\")
      (should (> (length (overblock-run-output-head from)) budget)))))

(ert-deftest overblock-pycell-test-mirror-reads-only-what-it-shows ()
  "The live mirror reads the head of the output, not all of it.
Reading everything on every tick is a pass over the whole output five
times a second, which grows with the cell."
  (let ((overblock-run-max-lines 4))
    (with-temp-buffer
      (setq-local comint-prompt-regexp "^In \\[[0-9]+\\]: ")
      (setq-local overblock-run-backend (overblock-pycell--backend))
      (let ((from (point-max-marker)))
        (setq-local overblock-run--state (list :from from :tail "" :start 0.0))
        (insert (mapconcat (lambda (i) (format "line %d" i))
                           (number-sequence 1 200) "\n")
                "\n")
        ;; The head holds what shows and a little slack, not the rest.
        (let ((head (overblock-run-output-head from)))
          (should (string-prefix-p "line 1\nline 2" head))
          (should-not (string-match-p "line 100" head))
          (should (< (length head) 100)))
        ;; It is kept, so later ticks read nothing.
        (should (equal (plist-get overblock-run--state :head) (overblock-run-output-head from)))
        ;; The count is of the whole output, counted as it arrives: the
        ;; position it reached is kept.
        (should (= (overblock-run-total from) 200))
        (should (= (car (plist-get overblock-run--state :count)) (point-max)))
        (insert "line 201\nline 202")
        (should (= (overblock-run-total from) 202))))))

(ert-deftest overblock-pycell-test-leading-blank-lines-do-not-shorten-the-head ()
  "Output that begins with blank lines still shows the lines it may.
The leading blank lines do not count toward the lines of the head,
because `:clean' removes them."
  (let ((overblock-run-max-lines 4))
    (with-temp-buffer
      (setq-local comint-prompt-regexp "^In \\[[0-9]+\\]: ")
      (setq-local overblock-run-backend (overblock-pycell--backend))
      (let ((from (point-max-marker)))
        (setq-local overblock-run--state (list :from from :tail "" :start 0.0))
        (insert (make-string 8 ?\n)
                (mapconcat (lambda (i) (format "line %d" i))
                           (number-sequence 1 20) "\n")
                "\n")
        (should (string-search "line 4" (overblock-run-output-head from)))))))

(ert-deftest overblock-pycell-test-mirror-keeps-nothing-while-it-has-nothing ()
  "An empty head is not kept, so the text can still arrive.
An incomplete escape sequence hides everything after it, and
comint-mime renders it only when it is complete."
  (let ((overblock-run-max-lines 2))
    (with-temp-buffer
      (setq-local comint-prompt-regexp "^In \\[[0-9]+\\]: ")
      (setq-local overblock-run-backend (overblock-pycell--backend))
      (let ((from (point-max-marker)))
        (setq-local overblock-run--state (list :from from :tail "" :start 0.0))
        (insert "\e]5151;{\"image/png\"\n")
        (insert (mapconcat (lambda (i) (format "line %d" i))
                           (number-sequence 1 20) "\n")
                "\n")
        (should (equal (overblock-run-output-head from) ""))
        (should-not (plist-get overblock-run--state :head))))))

(ert-deftest overblock-pycell-test-md-render-all-matches-one-by-one ()
  "Converting the buffer at once renders what one call per cell does."
  (skip-unless (overblock-md-program))
  (let ((buffer (generate-new-buffer "*overblock-pycell test notebook*"))
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
          (setq-local overblock-run-backend (overblock-pycell--backend))
          (overblock-pycell-test--render-all)
          (let ((batched (funcall displays)))
            (should (= (length batched) 3))
            (overblock-clear (point-min) (point-max) 'markdown)
            ;; The same buffer without the batch: with no joined text,
            ;; every cell converts on its own.
            (cl-letf (((symbol-function 'overblock-md--batch-text)
                       (lambda (_texts) nil)))
              (overblock-pycell-test--render-all))
            (should (equal batched (funcall displays)))))
      (kill-buffer buffer))))

(ert-deftest overblock-pycell-test-md-boundary-shapes ()
  "Every boundary `code-cells' takes as markdown is taken as markdown.
VS Code and Spyder write =#%% [markdown]= where jupytext writes
=# %% [markdown]=, and a tag list or a title can follow either."
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
      (should (overblock-pycell--md-cell-start (point))))
    ;; A code cell is not one, whatever its title.
    (dolist (line '("# %%" "#%%" "# %% A title" "# %% tags=[\"parameters\"]"))
      (erase-buffer)
      (insert line "\nx = 1\n")
      (goto-char (point-min))
      (forward-line 1)
      (should-not (overblock-pycell--md-cell-start (point))))))

(ert-deftest overblock-pycell-test-move-cell-carries-its-result ()
  "A cell that moves takes its result with it, and point comes along.
`transpose-regions' leaves an overlay where the text was, so the block
of one cell would end up under the other."
  (with-temp-buffer
    (insert "# %%\nfirst = 1\n\n# %%\nsecond = 2\n\n# %%\nthird = 3\n")
    (python-mode)
    (code-cells-mode)
    (setq-local overblock-run-backend (overblock-pycell--backend))
    ;; A result on the first two cells.
    (goto-char (point-min))
    (pcase-let ((`(,beg ,end) (code-cells--bounds nil nil t)))
      (overblock-run-show beg end "one" 0.1))
    (goto-char (point-min))
    (forward-line 3)
    (pcase-let ((`(,beg ,end) (code-cells--bounds nil nil t)))
      (overblock-run-show beg end "two" 0.2))
    ;; Move the first cell down, from inside it.
    (goto-char (point-min))
    (forward-line 1)
    (let ((column (- (point) (pos-bol))))
      (overblock-pycell-move-cell-down 1)
      ;; The text swapped.
      (should (string-match-p "\\`# %%\nsecond = 2\n\n# %%\nfirst = 1\n"
                              (buffer-substring-no-properties (point-min)
                                                              (point-max))))
      ;; Point is in the moved cell, at the same offset.
      (should (string-prefix-p "first = 1"
                               (buffer-substring-no-properties
                                (pos-bol) (pos-eol))))
      (should (= (- (point) (pos-bol)) column)))
    ;; Each result is on its own cell again.
    (let ((texts (mapcar #'overblock-run--result-text
                         (overblock-in (point-min) (point-max) 'result))))
      (should (equal (sort (copy-sequence texts) #'string<) '("one" "two")))
      (goto-char (point-min))
      (pcase-let ((`(,beg ,end) (code-cells--bounds nil nil t)))
        (should (equal (overblock-run--result-text (car (overblock-in beg end 'result)))
                       "two"))))))

(ert-deftest overblock-pycell-test-move-cell-keeps-a-rendered-markdown-cell ()
  "A rendered markdown cell moves whole, marker and rendering.
Its pieces hang on its source lines, and the lines move under them."
  (skip-unless (overblock-md-program))
  (with-temp-buffer
    (insert "# %%\nx = 1\n\n# %% [markdown]\n# ## Prose\n#\n# Words here.\n")
    (python-mode)
    (code-cells-mode)
    (setq-local overblock-run-backend (overblock-pycell--backend))
    (goto-char (point-min))
    (overblock-pycell-test--render-all)
    ;; The markdown cell is the second one; move it up.
    (goto-char (point-min))
    (forward-line 4)
    (overblock-pycell-move-cell-up 1)
    (let ((text (buffer-substring-no-properties (point-min) (point-max))))
      ;; The boundary line moved with the cell.
      (should (string-prefix-p "# %% [markdown]\n# ## Prose\n" text))
      (should (string-match-p "# %%\nx = 1\n" text)))
    ;; The rendering is on the cell, which is now the first one.
    (let ((rendered (overblock-in (point-min) (point-max) 'markdown)))
      (should rendered)
      (should (< (overlay-start (car rendered))
                 (save-excursion (goto-char (point-min))
                                 (forward-line 4)
                                 (point)))))))

(ert-deftest overblock-pycell-test-move-cell-carries-a-cell-that-holds-a-def ()
  "A cell moves whole, from anywhere inside it, defs and all.
`code-cells-mode' adds the headings of the major mode to
`outline-regexp', so a `def' in a cell is an outline heading too, and
the move must start from the boundary line."
  (with-temp-buffer
    (insert "# %%\ndef one():\n    return 1\n\n# %%\nprint(\"two\")\n")
    (python-mode)
    (code-cells-mode)
    (setq-local overblock-run-backend (overblock-pycell--backend))
    ;; Point in the body of the def.
    (goto-char (point-min))
    (forward-line 2)
    (overblock-pycell-move-cell-down 1)
    (should (equal (buffer-substring-no-properties (point-min) (point-max))
                   "# %%\nprint(\"two\")\n# %%\ndef one():\n    return 1\n\n"))
    ;; Point is still in the moved cell.
    (pcase-let ((`(,beg ,end) (code-cells--bounds)))
      (should (<= beg (point) end))
      (should (string-match-p "def one"
                              (buffer-substring-no-properties beg end))))))

(ert-deftest overblock-pycell-test-a-move-keeps-the-results-of-other-cells ()
  "A move takes the two cells it moves, and no others.
The text a move inserts lands at the anchor of the cell below it, whose
`insert-in-front-hooks' must not remove its result."
  (with-temp-buffer
    (insert "# %%\na = 1\n\n# %%\nb = 2\n\n# %%\nc = 3\n")
    (python-mode)
    (code-cells-mode)
    (setq-local overblock-run-backend (overblock-pycell--backend))
    ;; A result on each of the three cells.
    (goto-char (point-min))
    (dolist (text '("one" "two" "three"))
      (pcase-let ((`(,beg ,end) (code-cells--bounds nil nil t)))
        (overblock-run-show beg end text 0.1))
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
      ;; Move the first cell down: the third keeps its result.
      (goto-char (point-min))
      (forward-line 1)
      (overblock-pycell-move-cell-down 1)
      (should (equal (funcall texts) '("two" "one" "three")))
      ;; And back.
      (overblock-pycell-move-cell-up 1)
      (should (equal (funcall texts) '("one" "two" "three"))))))

(ert-deftest overblock-pycell-test-move-cell-stops-at-the-ends ()
  "The first cell cannot move up and the last cannot move down.
`outline-move-subtree-down' signals, because there is no sibling that
way, and nothing is removed before that."
  (with-temp-buffer
    (insert "# %%\nfirst = 1\n\n# %%\nsecond = 2\n")
    (python-mode)
    (code-cells-mode)
    (setq-local overblock-run-backend (overblock-pycell--backend))
    (goto-char (point-min))
    (pcase-let ((`(,beg ,end) (code-cells--bounds nil nil t)))
      (overblock-run-show beg end "one" 0.1))
    (let ((before (buffer-substring-no-properties (point-min) (point-max))))
      (goto-char (point-min))
      (forward-line 1)
      (should-error (overblock-pycell-move-cell-up 1) :type 'user-error)
      ;; The buffer and the result are untouched.
      (should (equal (buffer-substring-no-properties (point-min) (point-max))
                     before))
      (goto-char (point-min))
      (pcase-let ((`(,beg ,end) (code-cells--bounds nil nil t)))
        (should (equal (overblock-run--result-text (car (overblock-in beg end 'result)))
                       "one"))))))

(ert-deftest overblock-pycell-test-table-pops-as-a-live-table ()
  "The pop of a table gives a table that sorts, not a picture of one.
A copy carries the table object, and vtable draws a copy of it for the
window: the table of a result belongs to the shell, and Emacs 31
refuses to insert one vtable into a second buffer."
  (skip-unless (fboundp 'make-vtable))
  (overblock-pycell-test--with-cells
    (let* ((comint-prompt-regexp "^In \\[[0-9]+\\]: ")
           (text (overblock-pycell--clean (overblock-test-common-vtable-text))))
      (pcase-let ((`(,beg ,end) (code-cells--bounds nil nil t)))
        (overblock-run-show beg end text 0.4))
      (let ((ov (car (overblock-in (point-min) (point-max) 'result))))
        (goto-char (overlay-start ov))
        (save-window-excursion (overblock-run-pop-output))
        (with-current-buffer (format "*overblock-pycell: %s:%d*" (buffer-name) (line-number-at-pos (overlay-start ov)))
          ;; The table is among the other output, so it is searched for.
          (goto-char (or (text-property-not-all (point-min) (point-max)
                                                'vtable nil)
                         (point-min)))
          (should (vtable-current-table))
          (should (equal (mapcar #'vtable-column-name
                                 (vtable-columns (vtable-current-table)))
                         '("alpha" "beta_longer" "gamma")))
          ;; The drawn table is not the one of the result.
          (should-not (eq (vtable-current-table)
                          (get-text-property
                           (text-property-not-all 0 (length text)
                                                  'overblock-repl-table nil text)
                           'overblock-repl-table text)))
          ;; A copy of the text also carries the table object, so test
          ;; that the table works: a drawn table knows the column at
          ;; point and can sort by it.
          (forward-line 1)
          (should (vtable-current-column))
          (should (equal (vtable-current-object) '("1" "22" "333")))
          (let ((inhibit-read-only t))
            (vtable-sort-by-current-column))
          (goto-char (point-min))
          (forward-line 1)
          (should (equal (vtable-current-object) '("1" "22" "333"))))))))

(ert-deftest overblock-pycell-test-the-queue-belongs-to-its-shell ()
  "Two notebooks do not share the cells a run-all still has to run.
Each shell has its own queue."
  (let ((one (generate-new-buffer "one.py"))
        (two (generate-new-buffer "two.py"))
        (shell-one (generate-new-buffer "*Python one*"))
        (shell-two (generate-new-buffer "*Python two*")))
    (unwind-protect
        (progn
          (dolist (shell (list shell-one shell-two))
            (with-current-buffer shell (setq major-mode 'inferior-python-mode)))
          ;; A notebook is a buffer with a backend; the queue is
          ;; reached through it.
          (dolist (notebook (list one two))
            (with-current-buffer notebook
              (setq-local overblock-run-backend (overblock-pycell--backend))))
          ;; Each notebook has a shell of its own, as with
          ;; `python-shell-dedicated'.
          (cl-letf (((symbol-function 'python-shell-get-process)
                     (lambda (&rest _)
                       (if (eq (current-buffer) one) 'proc-one 'proc-two)))
                    ((symbol-function 'process-buffer)
                     (lambda (proc)
                       (if (eq proc 'proc-one) shell-one shell-two))))
            (with-current-buffer one (overblock-run--queue-set '(a b c)))
            (with-current-buffer two (overblock-run--queue-set '(x)))
            (should (equal (with-current-buffer one (overblock-run--queued)) '(a b c)))
            (should (equal (with-current-buffer two (overblock-run--queued)) '(x)))
            ;; Stopping one leaves the other running.
            (with-current-buffer two (overblock-run-stop))
            (should (equal (with-current-buffer one (overblock-run--queued)) '(a b c)))
            (should-not (with-current-buffer two (overblock-run--queued)))))
      (mapc #'kill-buffer (list one two shell-one shell-two)))))

(ert-deftest overblock-pycell-test-a-pass-aligns-the-cell-with-the-window-top ()
  "A queued pass puts the cell it starts at the top of the window.
The pass walks point down the notebook, but point alone can leave the
first line of the cell at the bottom edge, with the code out of sight."
  (overblock-pycell-test--with-notebook "# %%\nx = 1\n\n# %%\ny = 2\n"
    (let ((shell (generate-new-buffer " *overblock-pycell-test-shell*"))
          sent)
      (unwind-protect
          (cl-letf (((symbol-function 'python-shell-get-process)
                     (lambda (&rest _) 'proc))
                    ((symbol-function 'process-buffer)
                     (lambda (_proc) shell))
                    ((symbol-function 'overblock-run--send)
                     (lambda (_proc beg _end) (setq sent beg))))
            (with-current-buffer shell
              (setq major-mode 'inferior-python-mode))
            (let ((second (cadr (overblock-pycell--cell-starts)))
                  (window (get-buffer-window)))
              (set-window-start window (point-max))
              ;; As a pass starts: the home starts the following.
              (overblock-run--home-set (point-marker))
              (overblock-run--queue-set (list second))
              (overblock-run-next)
              (should (>= sent second))
              (should (= (window-start window) second))
              (should (= (window-point window) second))))
        (kill-buffer shell)))))

(ert-deftest overblock-pycell-test-a-pass-stops-following-when-the-reader-scrolls ()
  "A window the reader moved is left alone, and point stays at the end."
  (overblock-pycell-test--with-notebook "# %%
x = 1

# %%
y = 2

# %%
z = 3
"
    (let ((shell (generate-new-buffer " *overblock-pycell-test-shell*")))
      (unwind-protect
          (cl-letf (((symbol-function 'python-shell-get-process)
                     (lambda (&rest _) 'proc))
                    ((symbol-function 'process-buffer)
                     (lambda (_proc) shell))
                    ((symbol-function 'overblock-run--send) #'ignore))
            (with-current-buffer shell
              (setq major-mode 'inferior-python-mode))
            (pcase-let* ((starts (overblock-pycell--cell-starts))
                         (`(,first ,second ,third)
                          (mapcar #'marker-position starts))
                         (window (get-buffer-window)))
              (goto-char (point-max))
              (overblock-run--home-set (point-marker))
              (overblock-run--queue-set (list (car starts)))
              (overblock-run-next)
              (should (= (window-start window) first))
              (should (= (point) first))
              (should overblock-run--following)
              ;; The reader scrolls, as redisplay reports it.
              (set-window-start window third)
              (run-hook-with-args 'window-scroll-functions window third)
              (should-not overblock-run--following)
              ;; The next cell moves neither the window nor point.
              (overblock-run--queue-set (list (copy-marker second)))
              (overblock-run-next)
              (should (= (window-start window) third))
              (should (= (point) first))
              (overblock-run-go-home)
              (should (= (point) first))
              ;; With the option off, nothing moves at all.
              (let ((overblock-run-follow nil))
                (goto-char (point-min))
                (overblock-run--home-set (point-marker))
                (overblock-run--queue-set (list (copy-marker second)))
                (overblock-run-next)
                (should (= (point) (point-min)))
                (should-not overblock-run--following))))
        (kill-buffer shell)))))

(ert-deftest overblock-pycell-test-a-pass-asked-while-busy-goes-behind-the-queue ()
  "Run-above or run-below while a cell runs queues behind it, not over it."
  (overblock-pycell-test--with-notebook "# %% One\nx = 1\n\n# %% Two\ny = 2\n"
    (let ((shell (generate-new-buffer " *overblock-pycell-test-shell*")))
      (unwind-protect
          (cl-letf (((symbol-function 'python-shell-get-process)
                     (lambda (&rest _) 'proc))
                    ((symbol-function 'process-buffer)
                     (lambda (_proc) shell))
                    ((symbol-function 'overblock-run-next)
                     (lambda () (ert-fail "the pass must not start over the running one"))))
            (with-current-buffer shell
              (setq major-mode 'inferior-python-mode)
              (setq-local overblock-run--state (list :from 1)))
            (overblock-run--queue-set '(a))
            (overblock-run-cells '(b c) "running")
            (should (equal (overblock-run--queued) '(a b c)))
            (should (buffer-local-value 'overblock-run--home shell)))
        (kill-buffer shell)))))

(ert-deftest overblock-pycell-test-run-below-takes-this-cell-and-the-rest ()
  "`overblock-run-below' queues the cell at point and every one below it."
  (overblock-pycell-test--with-notebook "# %% One
x = 1

# %% Two
y = 2

# %% Three
z = 3
"
    (let (passed)
      (cl-letf (((symbol-function 'overblock-run-cells)
                 (lambda (cells _message) (setq passed cells))))
        (goto-char (cadr (overblock-pycell--cell-starts)))
        (forward-line 1)
        (overblock-run-below)
        (should (equal (mapcar #'marker-position passed)
                       (cdr (mapcar #'marker-position
                                    (overblock-pycell--cell-starts)))))))))

(ert-deftest overblock-pycell-test-a-new-button-list-redraws-the-bars ()
  "Customizing the buttons draws the bars of an open notebook again.
A bar stays as it is where nothing it compares changed, and the button
list is a change it cannot see, so the `:set' marks the bars stale."
  (overblock-pycell-test--with-notebook "# %%\nx = 1\n\n# %%\ny = 2\n"
    (let ((before (overblock-pycell-test--bar-texts))
          (was overblock-pycell-cell-buttons))
      (should before)
      (unwind-protect
          (progn
            (setopt overblock-pycell-cell-buttons
                    '((run ("" "▷" "run") "Run this cell"
                           overblock-pycell-run-cell t)))
            (should-not (equal before (overblock-pycell-test--bar-texts))))
        (setopt overblock-pycell-cell-buttons was))
      ;; And back again: the bars follow the option.
      (should (equal before (overblock-pycell-test--bar-texts))))))

(ert-deftest overblock-pycell-test-the-run-button-runs-the-cell-it-sits-on ()
  "The command hands the bounds of the cell at point to code-cells.
A press on the bar of another cell moves point there first: the click
carries the position."
  (overblock-pycell-test--with-cells
    (let (asked)
      (cl-letf (((symbol-function 'code-cells-eval)
                 (lambda (beg end &rest _) (setq asked (cons beg end)))))
        (goto-char (point-min))
        (overblock-pycell-run-cell)
        (should (equal asked (pcase-let ((`(,beg ,end)
                                          (code-cells--bounds nil nil t)))
                               (cons beg end))))
        ;; The second cell is another pair.
        (let ((first asked))
          (goto-char (point-max))
          (overblock-pycell-run-cell)
          (should-not (equal asked first)))))))

(ert-deftest overblock-pycell-test-the-mode-goes-on-in-python-alone ()
  "The hook a reader installs turns the mode on in a Python buffer only.
`code-cells-mode' is also on in other buffers, such as an Org file
with source blocks."
  (with-temp-buffer
    (python-mode)
    (overblock-pycell-mode-maybe)
    (unwind-protect (should overblock-pycell-mode)
      (overblock-pycell-mode -1)))
  (with-temp-buffer
    (text-mode)
    (overblock-pycell-mode-maybe)
    (should-not (bound-and-true-p overblock-pycell-mode))))

(ert-deftest overblock-pycell-test-a-bar-is-cut-again-for-a-new-width ()
  "A bar follows the window it is drawn in when that window changes width.
A bar stays as it is where nothing it compares changed.  The width is
not compared: a width change marks the bars stale."
  (overblock-pycell-test--with-notebook "# %%\nx = 1\n\n# %%\ny = 2\n"
    (let ((wide (overblock-pycell-test--bar-texts)))
      (should wide)
      (cl-letf (((symbol-function 'window-max-chars-per-line)
                 (lambda (&rest _) 24)))
        (overblock--width-changed)
        (should-not (equal wide (overblock-pycell-test--bar-texts)))))))

(ert-deftest overblock-pycell-test-a-guarded-key-answers-at-the-result ()
  "The filter lets a key through at the end of a cell that has a result.
A reader binds TAB in the result map, and TAB indents everywhere else
in the cell: the filter keeps the two apart."
  (overblock-pycell-test--with-cells
    (pcase-let ((`(,beg ,end) (code-cells--bounds nil nil t)))
      (overblock-run-show beg end "42" 0.3)
      (let ((block (car (overblock-in (point-min) (point-max) 'result))))
        ;; At the end of the cell, next to the result.
        (goto-char (overlay-end block))
        (should (overblock-pycell-tab-filter 'a-command))
        ;; Nowhere else in it.
        (goto-char beg)
        (should-not (overblock-pycell-tab-filter 'a-command))
        (goto-char (1- (overlay-end block)))
        (should-not (overblock-pycell-tab-filter 'a-command))))))

(ert-deftest overblock-pycell-test-a-running-cell-carries-a-stop-button ()
  "The header of a running cell holds a stop button, a finished one none.
The button shows only while the cell runs, and its click is
`overblock-run-interrupt'."
  (cl-flet ((stops (header)
              (let ((len (length header))
                    (pos 0)
                    found)
                (while (and (not found) (< pos len))
                  (when-let* ((map (get-text-property pos 'keymap header)))
                    (when (eq (keymap-lookup map "<down-mouse-1>")
                              #'overblock-run-interrupt)
                      (setq found t)))
                  (setq pos (1+ pos)))
                found)))
    (should (stops (overblock-run-header-of-pycell nil 1 1 0.5 'running nil)))
    (should-not (stops (overblock-run-header-of-pycell nil 1 1 0.5 nil nil)))
    (should-not (stops (overblock-run-header-of-pycell nil 1 1 0.5 'died nil)))))

(ert-deftest overblock-pycell-test-out-label-goes-where-it-begins-a-line ()
  "An Out[N] label goes where it begins a line, and nowhere else.
That is where the shell writes one.  A label in the middle of a line
cannot be told from the same characters inside a value, so it stays."
  (let ((comint-prompt-regexp "^\\(?:>>> \\|In \\[[0-9]+\\]: \\)"))
    ;; The label of the shell, at a line start.
    (should (equal (overblock-pycell--clean "a\nOut[3]: 42\n") "a\n42"))
    (should (equal (overblock-pycell--clean "Out[1]: 42") "42"))
    ;; The characters inside a value stay.
    (should (equal (overblock-pycell--clean "Out[1]: 'it reads Out[1]: here'")
                   "'it reads Out[1]: here'"))
    (should (equal (overblock-pycell--clean "a fake label: Out[42]: done")
                   "a fake label: Out[42]: done"))))

(ert-deftest overblock-pycell-test-an-edit-at-the-end-of-a-cell-drops-the-block ()
  "Typing on the blank line that ends a cell takes the result with it.
The anchor of a block stops one character short of the last newline
of the cell, so typing there is an insertion at the end of the overlay,
which `modification-hooks' does not see.  The same holds at the first
character of the cell."
  (dolist (where '(end start))
    (overblock-pycell-test--with-cells
      (pcase-let ((`(,beg ,end) (code-cells--bounds nil nil t)))
        (overblock-run-show beg end "old output" 0.1)
        (should (overblock-in (point-min) (point-max) 'result))
        (goto-char (if (eq where 'end) (1- end) beg))
        (insert "print(1)")
        (should-not (overblock-in (point-min) (point-max) 'result))))))

(ert-deftest overblock-pycell-test-a-final-newline-does-not-unrender-the-last-cell ()
  "A markdown cell at the end of a file survives the newline on save.
Without a final newline the anchor ends at `point-max', so the newline
that `require-final-newline' adds is an insertion at the end of the
anchor.  It changes nothing the cell renders, so the block stays.

The render itself writes nothing, so a read-only notebook renders and a
visited buffer stays unmodified."
  (skip-unless (overblock-md-program))
  (with-temp-buffer
    (insert "# %% [markdown]\n# text")          ; no final newline
    (python-mode)
    (code-cells-mode)
    (setq-local overblock-run-backend (overblock-pycell--backend))
    (goto-char (point-min))
    (let ((size (buffer-size)))
      (overblock-pycell-test--render-all)
      (should (overblock-in (point-min) (point-max) 'markdown))
      ;; The render left the text alone.
      (should (= (buffer-size) size))
      (set-buffer-modified-p nil)
      ;; The newline of a save leaves the rendering.
      (goto-char (point-max))
      (insert "\n")
      (should (overblock-in (point-min) (point-max) 'markdown))
      ;; A second character does not.
      (goto-char (point-max))
      (insert "x")
      (should-not (overblock-in (point-min) (point-max) 'markdown)))))

(ert-deftest overblock-pycell-test-a-read-only-notebook-renders ()
  "Rendering a markdown cell writes nothing, so a read-only buffer renders.
This matters for `view-file', a read-only checkout, or any file the
reader cannot write."
  (skip-unless (overblock-md-program))
  (with-temp-buffer
    (insert "# %% [markdown]\n# text")          ; no final newline
    (python-mode)
    (code-cells-mode)
    (setq-local overblock-run-backend (overblock-pycell--backend))
    (setq buffer-read-only t)
    (goto-char (point-min))
    (overblock-pycell-test--render-all)
    (should (overblock-in (point-min) (point-max) 'markdown))))

(ert-deftest overblock-pycell-test-a-key-in-a-rendered-cell-reaches-the-cell ()
  "A key bound in `overblock-pycell-md-map' answers on a rendered markdown cell.
Point never enters a display string, so the keymap that answers a key
is the one on the overlays.  The map has no keys of its own; the
binding of the reader must arrive."
  (skip-unless (overblock-md-program))
  (with-temp-buffer
    (insert "# %% [markdown]\n# A [link](https://ctan.org/) in prose.\n\n"
            "# %%\nx = 1\n")
    (python-mode)
    (code-cells-mode)
    (setq-local overblock-run-backend (overblock-pycell--backend))
    (overblock-pycell-test--render-all)
    (should (overblock-in (point-min) (point-max) 'markdown))
    ;; Point on the rendered cell.
    (goto-char (point-min))
    (forward-line 1)
    ;; No key of the package.
    (should-not (keymap-lookup overblock-pycell-md-map "RET"))
    (unwind-protect
        (progn (keymap-set overblock-pycell-md-map "RET" #'overblock-pycell-md-edit)
               (should (eq (key-binding (kbd "RET")) #'overblock-pycell-md-edit)))
      (keymap-unset overblock-pycell-md-map "RET" t))
    (should (eq (key-binding [mouse-1]) #'overblock-pycell-md-raw))
    (should (get-char-property (point) 'help-echo))))

(ert-deftest overblock-pycell-test-a-link-can-be-followed-from-the-keyboard ()
  "The links of a rendered cell are reachable without the mouse.
A click is answered by the string it lands on, and follows the link.
Point never enters a display string, so the cell is asked for its
links."
  (skip-unless (overblock-md-program))
  (with-temp-buffer
    (insert "# %% [markdown]\n"
            "# A [link](https://ctan.org/) and [another](https://gnu.org/).\n\n"
            "# %%\nx = 1\n")
    (python-mode)
    (code-cells-mode)
    (setq-local overblock-run-backend (overblock-pycell--backend))
    (overblock-pycell-test--render-all)
    (goto-char (point-min))
    (forward-line 1)
    (let* ((block (overblock-pycell--md-at nil))
           (links (overblock-pycell--md-links block)))
      (should (equal (mapcar #'cdr links)
                     '("https://ctan.org/" "https://gnu.org/")))
      (should (equal (mapcar #'car links) '("link" "another")))
      ;; One is followed at once; from several, the reader chooses.
      (let (asked visited)
        (cl-letf (((symbol-function 'browse-url)
                   (lambda (url &rest _) (setq visited url)))
                  ((symbol-function 'completing-read)
                   (lambda (&rest _) (setq asked t) "another")))
          (overblock-pycell-md-follow-link)
          (should asked)
          (should (equal visited "https://gnu.org/")))))
    ;; A cell with no link says so.
    (erase-buffer)
    (insert "# %% [markdown]\n# No link here.\n\n# %%\nx = 1\n")
    (overblock-pycell-test--render-all)
    (goto-char (point-min))
    (forward-line 1)
    (should-error (overblock-pycell-md-follow-link) :type 'user-error)))

(ert-deftest overblock-pycell-test-a-link-on-an-image-is-found ()
  "A link around an image is found with the rest.
The piece that holds an image hides its line with an empty display
string and shows the row on the before-string.

An Emacs that cannot read a PNG draws no image and keeps no link on
one: `overblock-md--image-file' returns nil for every path there."
  (skip-unless (overblock-md-program))
  (skip-unless (image-type-available-p 'png))
  (with-temp-buffer
    (insert "# %% [markdown]\n"
            "# [![badge](f.png)](https://colab.google/) and "
            "[plain](https://gnu.org/).\n\n# %%\nx = 1\n")
    (python-mode)
    (code-cells-mode)
    (setq-local overblock-run-backend (overblock-pycell--backend))
    (cl-letf (((symbol-function 'display-images-p) (lambda (&rest _) t))
              ((symbol-function 'create-image)
               (lambda (f &rest _) (list 'image :type 'png :file f))))
      (overblock-pycell-test--render-all))
    (goto-char (point-min))
    (forward-line 1)
    (should (equal (mapcar #'cdr (overblock-pycell--md-links (overblock-pycell--md-at nil)))
                   '("https://colab.google/" "https://gnu.org/")))))

(ert-deftest overblock-pycell-test-a-pop-out-follows-a-running-cell ()
  "A popped-out result keeps filling while the cell runs.
The block shows `overblock-run-max-lines' of the output; the buffer
gets all of it, so a long run can be followed in a window of its own.
Only what is new is copied each time, and the end of the cell writes
all of it again without the prompts."
  (let ((shell (generate-new-buffer " *overblock-pycell-test-shell*"))
        (out (generate-new-buffer " *overblock-pycell-test-out*")))
    (unwind-protect
        (with-current-buffer shell
          (insert "one\ntwo\n")
          (setq-local overblock-run--state
                      (list :from (copy-marker 1)
                            :follow (cons out (copy-marker 1))))
          ;; What was printed already.
          (overblock-run--follow-tick)
          (should (equal (with-current-buffer out (buffer-string))
                         "one\ntwo\n"))
          ;; Then only what is new.
          (goto-char (point-max))
          (insert "three\n")
          (overblock-run--follow-tick)
          (should (equal (with-current-buffer out (buffer-string))
                         "one\ntwo\nthree\n"))
          ;; Point at the end followed the output.
          (should (with-current-buffer out (= (point) (point-max))))
          ;; A killed buffer is not written to.
          (let ((gone (generate-new-buffer " *overblock-pycell-test-gone*")))
            (kill-buffer gone)
            (setq overblock-run--state (plist-put overblock-run--state :follow
                                         (cons gone (copy-marker 1))))
            (should-not (overblock-run--follow-tick)))
          ;; The end writes all of it, cleaned.
          (overblock-run--follow-done out "one\ntwo\nthree")
          (should (equal (with-current-buffer out (buffer-string))
                         "one\ntwo\nthree")))
      (kill-buffer shell)
      (kill-buffer out))))

(ert-deftest overblock-pycell-test-a-restart-sweeps-what-lost-its-anchor ()
  "A restart takes the results and whatever no live block owns.
Clearing only the results cannot sweep an orphan, such as the cloak of
a lost block that keeps lines invisible, so the restart sweeps too."
  (cl-letf (((symbol-function 'run-python) #'ignore)
            ((symbol-function 'python-shell-get-process) #'ignore)
            ((symbol-function 'overblock-run--queue-set) #'ignore)
            ((symbol-function 'overblock-pycell--dedicated) #'ignore))
    (overblock-pycell-test--with-cells
      (let ((orphan (make-overlay (point-min) (1+ (point-min)))))
        (overlay-put orphan 'overblock-part t)
        (overlay-put orphan 'invisible t)
        (overblock-pycell-restart)
        (should-not (overlay-buffer orphan))))))

(ert-deftest overblock-pycell-test-a-narrower-window-gets-a-new-bar ()
  "A bar is drawn again when the window it was built for has changed width.
`overblock-bar' cuts the label to the room the icons leave, and the cut
is in the string, so a narrower window (a split, a side window, a
resized frame) needs a new one."
  (overblock-pycell-test--with-cells
    (pcase-let ((`(,beg ,end) (code-cells--bounds nil nil t)))
      (overblock-run-show beg end "one line of output" 1.6))
    (let* ((block (car (overblock-in (point-min) (point-max) 'result)))
           (wide (overlay-get block 'after-string)))
      (should wide)
      ;; The same buffer in a window of twenty columns.
      (cl-letf (((symbol-function 'window-max-chars-per-line)
                 (lambda (&rest _) 20))
                ((symbol-function 'get-buffer-window-list)
                 (lambda (&rest _) (list (selected-window)))))
        (overblock--width-changed)
        (let ((narrow (overlay-get block 'after-string)))
          (should-not (equal wide narrow))
          (should (string-search "…" narrow))))
      ;; Nothing is redrawn while the width stays.
      (cl-letf (((symbol-function 'window-max-chars-per-line)
                 (lambda (&rest _) 20))
                ((symbol-function 'get-buffer-window-list)
                 (lambda (&rest _) (list (selected-window))))
                ((symbol-function 'overblock-run-update)
                 (lambda (&rest _) (error "Drawn again for nothing"))))
        (overblock--width-changed)))))

(ert-deftest overblock-pycell-test-a-pop-out-keeps-what-is-around-a-table ()
  "A pop-out holds the whole result, table and all the rest.
The lines a cell printed before its DataFrame, and after it, are in the
buffer too."
  (skip-unless (fboundp 'make-vtable))
  (with-temp-buffer
    (let ((text (concat "before the table\n"
                        (let ((comint-prompt-regexp "^In \\[[0-9]+\\]: "))
                          (overblock-pycell--clean (overblock-test-common-vtable-text)))
                        "\nafter the table")))
      (overblock-run--insert-result text)
      (let ((shown (buffer-string)))
        ;; In the order the cell printed them: `vtable-insert' leaves
        ;; point between the header and the rows.
        (should (string-search "before the table" shown))
        (should (string-search "after the table" shown))
        (should (< (string-search "before the table" shown)
                   (string-search "alpha" shown)))
        (should (< (string-search "alpha" shown)
                   (string-search "after the table" shown)))
        ;; Every row of the table is above the text after it.
        (dolist (cell '("22" "4444" "9999"))
          (should (< (string-search cell shown)
                     (string-search "after the table" shown)))))
      (goto-char (or (text-property-not-all (point-min) (point-max)
                                            'vtable nil)
                     (point-min)))
      (should (vtable-current-table)))))

(ert-deftest overblock-pycell-test-a-pop-out-interrupts-its-own-shell ()
  "A popped-out result interrupts the shell it came from.
It is not a Python buffer, so `python-shell-get-process' would return
the shell of the settings, which can be the wrong one.  The buffer
remembers its shell instead."
  (let* ((shell (generate-new-buffer " *overblock-pycell-test-shell*"))
         (notebook (generate-new-buffer " *overblock-pycell-test-nb*"))
         ;; A marker whose buffer is gone, made before the stubs:
         ;; `kill-buffer' calls `get-buffer-process'.
         (dead (let ((gone (generate-new-buffer " *overblock-pycell-test-gone*")))
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
                   (lambda (&rest _) (error "Asked for a shell of its own"))))
          (let ((cell (with-current-buffer notebook
                        (insert "x = 1\n")
                        (copy-marker (point-min)))))
            (with-current-buffer shell
              (setq-local overblock-run--state (list :beg cell)))
            (with-temp-buffer
              (setq-local overblock-run--follower (cons shell cell))
              (overblock-run-interrupt)
              (should (equal asked (list 'process-of shell)))
              ;; Not the run of another notebook, nor an ended result.
              (setq asked nil)
              (with-current-buffer shell
                (setq overblock-run--state (list :beg (with-current-buffer notebook
                                               (copy-marker (point-max))))))
              (should-error (overblock-run-interrupt) :type 'user-error)
              (should-not asked)
              (with-current-buffer shell (setq overblock-run--state nil))
              (should-error (overblock-run-interrupt) :type 'user-error)
              (should-not asked))
            ;; A killed notebook leaves the shared marker pointing
            ;; nowhere, which gives a `user-error', not a signal of `='.
            (progn
              (with-current-buffer shell (setq overblock-run--state (list :beg dead)))
              (with-temp-buffer
                (setq-local overblock-run--follower (cons shell dead))
                (should-error (overblock-run-interrupt) :type 'user-error)
                (should-not asked)))
            ;; A shell without a process says so: `interrupt-process' of
            ;; nil takes the process of the current buffer.
            (with-current-buffer shell (setq overblock-run--state (list :beg cell)))
            (cl-letf (((symbol-function 'get-buffer-process) #'ignore))
              (with-temp-buffer
                (setq-local overblock-run--follower (cons shell cell))
                (should-error (overblock-run-interrupt) :type 'user-error)
                (should-not asked)))))
      (kill-buffer shell)
      (kill-buffer notebook))))

(ert-deftest overblock-pycell-test-a-restart-keeps-the-renderings ()
  "A restart takes the results down and leaves the markdown standing.
`overblock-pycell-restart-and-run-all' renders a cell only when the
pass reaches it, so a pass that stops early would leave the cells after
it plain."
  (cl-letf (((symbol-function 'run-python) #'ignore)
            ((symbol-function 'python-shell-get-process) #'ignore)
            ((symbol-function 'overblock-run--queue-set) #'ignore)
            ((symbol-function 'overblock-pycell--dedicated) #'ignore))
    (overblock-pycell-test--with-cells
      (pcase-let ((`(,beg ,end) (code-cells--bounds nil nil t)))
        (overblock-run-show beg end "output" 0.1))
      (goto-char (point-max))
      (let ((beg (point)))
        (insert "# %% [markdown]\n# text\n")
        (overblock-show (+ beg 16) (point-max) :kind 'markdown :over "text"))
      (should (overblock-in (point-min) (point-max) 'result))
      (should (overblock-in (point-min) (point-max) 'markdown))
      (overblock-pycell-restart)
      (should-not (overblock-in (point-min) (point-max) 'result))
      (should (overblock-in (point-min) (point-max) 'markdown)))))

(ert-deftest overblock-pycell-test-the-queue-walks-markdown-cells-in-one-frame ()
  "A run-all pass crosses markdown cells without building a frame each.
`overblock-run-next' is a loop.  With recursion, each frame would run
its tail on the way out and send a code cell while another runs."
  (with-temp-buffer
    (insert "# %% [markdown]\n# one\n\n# %% [markdown]\n# two\n\n"
            "# %%\nx = 1\n\n# %%\ny = 2\n")
    (python-mode)
    (code-cells-mode)
    (setq-local overblock-run-backend (overblock-pycell--backend))
    (let ((notebook (current-buffer))
          (shell (generate-new-buffer " *overblock-pycell-test-shell*"))
          (sent nil)
          (depth 0)
          (deepest 0))
      (unwind-protect
          (cl-letf* (((symbol-function 'overblock-run-shell)
                      (lambda (&rest _) shell))
                     ((symbol-function 'overblock-md-rendered)
                      (lambda (md &rest _) md))
                     ;; A code cell is where the walk has to stop.
                     ((symbol-function 'python-shell-get-process)
                      (lambda (&rest _) 'process))
                     (send (symbol-function 'overblock-run--send))
                     ((symbol-function 'overblock-run--send)
                      (lambda (_proc beg _end)
                        (ignore send)
                        (setq depth (1+ depth)
                              deepest (max deepest depth))
                        (push beg sent)
                        (setq depth (1- depth)))))
            ;; The markers belong to the notebook, not the shell.
            (let ((cells (with-current-buffer notebook
                           (save-excursion
                             (goto-char (point-min))
                             (let ((marks (list (point-marker))))
                               (dotimes (_ 3)
                                 (code-cells-forward-cell)
                                 (push (point-marker) marks))
                               (nreverse marks))))))
              (with-current-buffer shell
                (setq-local overblock-run--queue cells)))
            (overblock-run-next)
            ;; The two markdown cells render, the first code cell is
            ;; sent, and the walk stops: the second code cell waits for
            ;; the prompt of the first.
            (should (= (length (overblock-in (point-min) (point-max)
                                             'markdown))
                       2))
            (should (= (length sent) 1))
            (should (= deepest 1))
            (should (= (length (overblock-run--queued)) 1)))
        (kill-buffer shell)))))

(ert-deftest overblock-pycell-test-copy-output-keeps-what-the-result-holds ()
  "The copy carries the text properties, so an image survives a yank.
It is the result the click landed on, not the one point is in.
The buffer must be in a window: a click carries its window, and the
commands select it."
  (overblock-pycell-test--with-notebook "# %%\nx = 1\n\n# %%\ny = 2\n"
    (let ((kill-ring nil)
          (in-the-first-cell nil))
      (pcase-let ((`(,beg ,end) (code-cells--bounds nil nil t)))
        (setq in-the-first-cell beg)
        (overblock-run-show beg end (concat "a line\n" overblock-test-common-image) 0.1))
      ;; Point in the other cell: the click decides which result.
      (goto-char (point-max))
      (overblock-run-copy-output (list 'mouse-1 (list (selected-window)
                                               in-the-first-cell
                                               (cons 0 0) 0)))
      (should (string-prefix-p "a line" (current-kill 0)))
      (should (overblock-image-in (current-kill 0))))))

(ert-deftest overblock-pycell-test-discard-output-takes-one-result ()
  "The result of the cell that was clicked goes, and no other."
  (overblock-pycell-test--with-notebook "# %%\nx = 1\n\n# %%\ny = 2\n"
    (dolist (which '(1 -1))
      (goto-char (if (> which 0) (point-min) (point-max)))
      (pcase-let ((`(,beg ,end) (code-cells--bounds nil nil t)))
        (overblock-run-show beg end "out" 0.1)))
    (should (= 2 (length (overblock-in (point-min) (point-max) 'result))))
    (goto-char (point-min))
    (overblock-run-discard-output)
    (should (= 1 (length (overblock-in (point-min) (point-max) 'result))))
    ;; The one left belongs to the second cell.
    (should (> (overlay-start (car (overblock-in (point-min) (point-max)
                                                 'result)))
               (point-min)))
    ;; Asking again where there is none signals.
    (should-error (overblock-run-discard-output) :type 'user-error)))

(ert-deftest overblock-pycell-test-a-markdown-edit-can-be-abandoned ()
  "`overblock-edit-abort' leaves the source as it was, and the window with it."
  (let ((buffer (get-buffer-create " *overblock-pycell test md abort*")))
    (unwind-protect
        (with-current-buffer buffer
          (setq-local overblock-edit--source
                      (list (current-buffer) 1 2 #'ignore))
          (should (commandp 'overblock-edit-abort))
          (cl-letf (((symbol-function 'quit-window)
                     (lambda (&optional kill _window)
                       (should kill)
                       (throw 'quit t))))
            (should (catch 'quit (overblock-edit-abort) nil))))
      (kill-buffer buffer))))

(ert-deftest overblock-pycell-test-save-image-writes-the-bytes-it-was-given ()
  "The file holds the data of the image, and its type names it.
A result with no image says so rather than writing an empty file."
  (overblock-pycell-test--with-notebook "# %%\nx = 1\n\n# %%\ny = 2\n"
    (let* ((png (propertize " " 'display '(image :type png :data "\211PNG!")))
           ;; A new name: `write-region' gets MUSTBENEW, which asks before
           ;; it overwrites, and a batch Emacs cannot answer.
           (file (expand-file-name (make-temp-name "overblock-pycell-image-")
                                   temporary-file-directory)))
      (unwind-protect
          (progn
            (pcase-let ((`(,beg ,end) (code-cells--bounds nil nil t)))
              (overblock-run-show beg end (concat "a figure\n" png) 0.1))
            (cl-letf (((symbol-function 'read-file-name)
                       (lambda (_prompt &rest _) file)))
              (overblock-run-save-image))
            (should (file-exists-p file))
            (should (equal (with-temp-buffer
                             (set-buffer-multibyte nil)
                             (insert-file-contents-literally file)
                             (buffer-string))
                           "\211PNG!"))
            ;; The default name follows the type of the image.
            (delete-file file)
            (let (offered)
              (cl-letf (((symbol-function 'read-file-name)
                         (lambda (_prompt _dir _default _mustmatch initial)
                           (setq offered initial)
                           file)))
                (overblock-run-save-image))
              (should (equal offered "figure.png")))
            ;; A result without an image gives no file.
            (goto-char (point-max))
            (pcase-let ((`(,beg ,end) (code-cells--bounds nil nil t)))
              (overblock-run-show beg end "no figure here" 0.1))
            (should-error (overblock-run-save-image) :type 'user-error))
        (when (file-exists-p file) (delete-file file))))))

;;;; The bar over a boundary line

(ert-deftest overblock-pycell-test-the-title-is-what-follows-the-marker ()
  "The text after the marker names the cell, and a tag list is not text."
  (with-temp-buffer
    (insert "# %%\n# %% A title\n# %% [markdown]\n# %% [markdown] Notes\n")
    (python-mode)
    (code-cells-mode)
    (setq-local overblock-run-backend (overblock-pycell--backend))
    (should (equal (mapcar (lambda (line)
                             (goto-char (point-min))
                             (forward-line (1- line))
                             (overblock-pycell--cell-title (pos-bol) (pos-eol)))
                           '(1 2 3 4))
                   '(nil "A title" nil "Notes")))))

(ert-deftest overblock-pycell-test-a-bar-over-every-code-cell ()
  "Every code cell boundary line carries a bar, and the line is hidden.
A markdown boundary line is left to the rendering, which brings its own."
  (overblock-pycell-test--with-notebook
      "# %%\nx = 1\n\n# %% Titled\ny = 2\n\n# %% [markdown]\n# text\n"
    (should (equal (overblock-pycell-test--bar-labels) '("python" "Titled")))
    ;; No code bar on the markdown line.
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
      ;; The bar is the before-string; the text of the line draws as the
      ;; last glyph of the bar, with `cursor' for the caret. Never the
      ;; after-string, which a cloak can hide.
      (should (overlay-get bar 'before-string))
      (should (eq (get-text-property 0 'cursor (overlay-get bar 'display)) t))
      (should (= (length (overlay-get bar 'display)) 1))
      (should-not (overlay-get bar 'after-string))
      (should (equal (buffer-substring-no-properties (overlay-start bar)
                                                     (overlay-end bar))
                     "# %%")))))

(ert-deftest overblock-pycell-test-a-cell-typed-in-gets-a-bar ()
  "A boundary line written into the buffer is barred as it appears.
And a line that becomes a markdown boundary loses the code bar it had."
  (overblock-pycell-test--with-notebook "# %%\nx = 1\n"
    (goto-char (point-max))
    (insert "\n# %% Later\nz = 3\n")
    (should (equal (overblock-pycell-test--bar-labels) '("python" "Later")))
    ;; The title is read again when the line is edited.
    (goto-char (point-min))
    (end-of-line)
    (insert " Named")
    (should (equal (overblock-pycell-test--bar-labels) '("Named" "Later")))
    ;; And a line rewritten as a markdown boundary loses its code bar.
    (goto-char (point-min))
    (delete-region (pos-bol) (pos-eol))
    (insert "# %% [markdown]")
    (should (equal (overblock-pycell-test--bar-labels) '("Later")))))

(ert-deftest overblock-pycell-test-a-narrowing-hides-no-cell-from-the-bars ()
  "Every cell is barred, not only the ones the narrowing shows.
A notebook can be narrowed when the mode goes on, for example by
`narrow-to-defun'."
  (with-temp-buffer
    (insert "# %% One\nx = 1\n\n# %% Two\ny = 2\n")
    (python-mode)
    (set-window-buffer nil (current-buffer))
    (code-cells-mode)
    (narrow-to-region (point-min) 12)
    (unwind-protect
        (progn
          (overblock-pycell-mode)
          (should (= 2 (length (overblock-bars)))))
      (overblock-pycell-mode -1))))

(ert-deftest overblock-pycell-test-the-mode-takes-its-bars-with-it ()
  "Turning the mode off leaves the buffer as it was."
  (with-temp-buffer
    (insert "# %%\nx = 1\n")
    (python-mode)
    (code-cells-mode)
    (setq-local overblock-run-backend (overblock-pycell--backend))
    (overblock-pycell-mode)
    (should (overblock-bars))
    (overblock-pycell-mode -1)
    (should-not (overblock-bars))))

(ert-deftest overblock-pycell-test-run-above-refuses-the-first-cell ()
  "There is nothing above the first cell, and nothing is started for it.
The cells above are the ones the walk finds before this one begins."
  (overblock-pycell-test--with-notebook "# %% One\nx = 1\n\n# %% Two\ny = 2\n"
    (goto-char (point-min))
    (should-error (overblock-run-above) :type 'user-error)
    (goto-char (point-max))
    (should (equal (mapcar #'marker-position
                           (seq-take-while
                            (lambda (m) (< m (car (code-cells--bounds))))
                            (overblock-pycell--cell-starts)))
                   (list 1)))))

(ert-deftest overblock-pycell-test-a-button-moves-the-cell-it-belongs-to ()
  "A click on the arrow of one cell moves that cell, not the one at point.
The commands read the click, so the cell of the pressed button moves."
  (overblock-pycell-test--with-notebook "# %% One\nx = 1\n\n# %% Two\ny = 2\n"
    (let* ((second (save-excursion
                     (goto-char (point-min))
                     (re-search-forward "^# %% Two")
                     (pos-bol)))
           (click (list 'mouse-1 (list (selected-window) second
                                       (cons 0 0) 0))))
      ;; Point in the first cell, the click on the second.
      (goto-char (point-min))
      (overblock-pycell-move-cell-up 1 click)
      (goto-char (point-min))
      (should (looking-at-p "# %% Two")))))

(ert-deftest overblock-pycell-test-a-line-that-stops-being-a-boundary-loses-its-bar ()
  "A bar belongs to a boundary line, and to a line that still is one.
After a space typed before the comment, or half of the marker
deleted, the buttons of the bar would act on the wrong cell."
  (overblock-pycell-test--with-notebook "# %% One\nx = 1\n\n# %% Two\ny = 2\n"
    (should (equal (overblock-pycell-test--bar-labels) '("One" "Two")))
    (goto-char (point-min))
    (insert " ")                        ; " # %% One" is no boundary
    (should (equal (overblock-pycell-test--bar-labels) '("Two")))
    (goto-char (point-min))
    (delete-char 1)
    (should (equal (overblock-pycell-test--bar-labels) '("One" "Two")))
    ;; Half a marker is no marker.
    (goto-char (point-min))
    (re-search-forward "%%")
    (delete-char -1)
    (should (equal (overblock-pycell-test--bar-labels) '("Two")))))

(ert-deftest overblock-pycell-test-a-markdown-cell-showing-its-source-has-a-bar ()
  "A markdown cell that is not rendered is barred too, and can be rendered.
Writing `[markdown]' on a line removes its code bar, and a source bar
takes its place."
  (overblock-pycell-test--with-notebook "# %% [markdown]\n# text\n\n# %% Two\ny = 2\n"
    (let ((bar (overblock-bar-in (point-min) (pos-eol))))
      ;; Rendered or not (a converter can be missing), the line has a
      ;; bar, and it is no code bar.
      (should bar)
      (should (memq (overblock-bar-kind bar) '(source markdown)))
      (should (commandp 'overblock-pycell-md-render-cell)))))

(ert-deftest overblock-pycell-test-a-boundary-line-keeps-one-bar-through-its-kinds ()
  "Writing and unwriting `[markdown]' leaves one bar, of the right kind.
A source bar goes when the line stops saying =[markdown]=, and the
code bar takes its place."
  (overblock-pycell-test--with-notebook "# %% One\nx = 1\n\n# %% Two\ny = 2\n"
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

(ert-deftest overblock-pycell-test-one-glyph-means-one-thing ()
  "No two buttons draw the same glyph, in any row of candidates.
A frame draws whichever row it can: the nerd glyphs, the symbols of an
ordinary font, or the plain characters of a terminal."
  (let ((bars (list overblock-pycell-result-buttons overblock-pycell-md-buttons
                    overblock-pycell-cell-buttons overblock-pycell-source-buttons)))
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
    ;; `overblock-glyph' chooses per button, so a frame can mix rows. A
    ;; glyph means one command whichever row it comes from.
    (let (seen)
      (dolist (buttons bars)
        (dolist (button buttons)
          (dolist (glyph (nth 1 button))
            (let ((before (assoc glyph seen)))
              (when before
                (should (eq (cdr before) (nth 3 button))))
              (push (cons glyph (nth 3 button)) seen))))))))

(ert-deftest overblock-pycell-test-customizing-the-buttons-draws-the-bars-again ()
  "A button list set with `setopt' shows on a notebook already open.
The `:set' of the option draws the bars again."
  (let ((was overblock-pycell-cell-buttons))
    (overblock-pycell-test--with-notebook "# %% One\nx = 1\n"
      (unwind-protect
          (progn
            (should-not (string-search "ZZ" (car (overblock-pycell-test--bar-texts))))
            (setopt overblock-pycell-cell-buttons
                    '((only ("ZZ") "The only button" overblock-pycell-run-cell t)))
            (should (string-search "ZZ" (car (overblock-pycell-test--bar-texts)))))
        (setopt overblock-pycell-cell-buttons was)))))

(ert-deftest overblock-pycell-test-a-cell-taken-back-to-its-source-keeps-a-bar ()
  "Taking a cell back to its source leaves it a bar to be rendered from.
`overblock-pycell-md-raw' is the command, and the render button is on
that bar.  Taking a rendering down deletes its bar and changes no text,
so nothing else would draw one."
  (skip-unless (overblock-md-program))
  (overblock-pycell-test--with-notebook "# %% [markdown]\n# text\n\n# %% Two\ny = 2\n"
    (let ((kind (lambda ()
                  (save-excursion
                    (goto-char (point-min))
                    (overblock-bar-kind (overblock-bar-on-line))))))
      ;; `should', not `skip-unless': the kind of the bar is what this
      ;; tests. The converter is checked above.
      (should (eq (funcall kind) 'markdown))
      (goto-char (point-min))
      (forward-line 1)
      (overblock-pycell-md-raw)
      (should (eq (funcall kind) 'source))
      ;; One bar, not two.
      (should (= 1 (length (seq-filter
                            #'overblock-bar-kind
                            (overlays-in (point-min)
                                         (save-excursion
                                           (goto-char (point-min))
                                           (pos-eol)))))))
      ;; The button renders it again.
      (goto-char (point-min))
      (forward-line 1)
      (overblock-pycell-md-render-cell)
      (should (eq (funcall kind) 'markdown)))))

(ert-deftest overblock-pycell-test-a-click-on-a-bar-leaves-the-bar-showing ()
  "Point lands below the bar, not on it, so the button can be pressed again.
The bar keeps showing while point is on its line, so the button stays
for the next press."
  (overblock-pycell-test--with-notebook "# %% One\nx = 1\n\n# %% Two\ny = 2\n"
    (let* ((second (save-excursion
                     (goto-char (point-min))
                     (re-search-forward "^# %% Two")
                     (pos-bol)))
           (click (list 'mouse-1 (list (selected-window) second
                                       (cons 0 0) 0))))
      (goto-char (point-min))
      (overblock-goto-event click)
      (should (= (point) second))
      ;; The commands find that cell.
      (should (equal (car (code-cells--bounds)) second))
      ;; The bar is where it was.
      (should (overlay-get (save-excursion (goto-char second)
                                           (overblock-bar-on-line))
                           'before-string)))))

(ert-deftest overblock-pycell-test-a-refused-pass-leaves-nothing-queued ()
  "A run-above that cannot start leaves no cells behind to run later.
A refusal empties the queue, so the rest of the pass does not run
later."
  (overblock-pycell-test--with-notebook "# %% One\nx = 1\n\n# %% Two\ny = 2\n"
    (let ((shell (generate-new-buffer " *overblock-pycell-test-shell*")))
      (unwind-protect
          (cl-letf* (((symbol-function 'python-shell-get-process)
                      (lambda (&rest _) 'a-process))
                     ((symbol-function 'overblock-run-shell)
                      (lambda () shell))
                     ;; The shell refuses the cell, as a busy one does.
                     ((symbol-function 'overblock-pycell-eval-region)
                      (lambda (&rest _) (user-error "Still busy"))))
            (goto-char (point-max))
            (should-error (overblock-run-above) :type 'user-error)
            (should-not (overblock-run--queued)))
        (kill-buffer shell)))))

(ert-deftest overblock-pycell-test-a-change-leaves-the-search-alone ()
  "A caller's match survives the bars being drawn.
`after-change-functions' runs between a search and its use of the
match, and the walk that draws the bars searches too."
  (overblock-pycell-test--with-notebook "# %% code BEFORE\nx = 1\n"
    (goto-char (point-min))
    (should (search-forward "BEFORE" nil t))
    (replace-match "AFTER")
    (should (equal (buffer-string) "# %% code AFTER\nx = 1\n"))))

(ert-deftest overblock-pycell-test-a-failure-without-a-traceback-stops-a-pass ()
  "Output that names an exception ends a pass, traceback or not.
A `SyntaxError' prints only the name of the exception."
  ;; What ipython prints for a syntax error, in full.
  (should (overblock-pycell--error-p "  File <ipython-input-3>:1\n    x = = 1\n        ^\nSyntaxError: invalid syntax\n"))
  (should (overblock-pycell--error-p "Traceback (most recent call last)\n  ...\nValueError: boom\n"))
  (should (overblock-pycell--error-p "SystemExit: 2"))
  (should (overblock-pycell--error-p "KeyboardInterrupt"))
  (should (overblock-pycell--error-p "numpy.linalg.LinAlgError: singular matrix"))
  ;; And what is not a failure.
  (should-not (overblock-pycell--error-p "42\n"))
  (should-not (overblock-pycell--error-p ""))
  (should-not (overblock-pycell--error-p "the Error: was printed, not raised\n"))
  (should-not (overblock-pycell--error-p "Done\n"))
  ;; A cell that prints the name of an exception it caught: the colon
  ;; tells a report from a print, except for the two names IPython
  ;; prints alone.
  (should-not (overblock-pycell--error-p "ValueError\n"))
  (should-not (overblock-pycell--error-p "caught: ZeroDivisionError\n")))

(ert-deftest overblock-pycell-test-a-rendered-bar-follows-its-line ()
  "A title typed at the end of a boundary line reaches the bar above it.
The overlay of the bar does not grow at its end, so the bar moves it
to the line before it reads the label."
  (skip-unless (overblock-md-program))
  (overblock-pycell-test--with-notebook "# %% [markdown] first\n# text\n"
    (overblock-pycell-test--render-all)
    (let ((bar (save-excursion (goto-char (point-min))
                               (overblock-bar-on-line))))
      ;; The product under test, so `should': see the same question in
      ;; `overblock-pycell-test-md-an-edit-takes-the-bar-with-it'.
      (should (eq (overblock-bar-kind bar) 'markdown))
      (goto-char (pos-eol))
      (insert " and more")
      ;; The bar covers the whole line again, and says so.
      (should (= (overlay-end bar) (pos-eol)))
      (should (string-match-p "first and more"
                              (overlay-get bar 'overblock-bar-text))))))

(ert-deftest overblock-pycell-test-a-pass-remembers-where-it-came-from ()
  "The place a pass was asked for is kept with the shell, and given back.
A refused pass clears the place, so it does not move point when
another cell ends."
  (overblock-pycell-test--with-notebook "# %% One\nx = 1\n\n# %% Two\ny = 2\n"
    (let ((shell (get-buffer-create " *overblock-pycell-test-shell*")))
      (cl-letf (((symbol-function 'overblock-run-shell) (lambda () shell)))
        (with-current-buffer shell (setq-local overblock-run--home nil))
        (goto-char (point-max))
        (overblock-run--home-set (point-marker))
        (should (= (marker-position
                    (buffer-local-value 'overblock-run--home shell))
                   (point-max)))
        ;; A refusal takes it away again, so nothing drags point later.
        (overblock-run--home-set nil)
        (should-not (buffer-local-value 'overblock-run--home shell))
        ;; And going home with none set is not an error.
        (overblock-run-go-home))
      (kill-buffer shell))))

(ert-deftest overblock-pycell-test-a-read-only-notebook-shows-a-result ()
  "A result shows in a notebook that refuses to be written to.
The last cell of a file without a final newline gets a newline for the
result.  A read-only notebook (`view-file', a read-only checkout)
refuses it, and the write is in the process filter, where an error
leaves the shell busy.  The block then hangs on its anchor."
  (with-temp-buffer
    (insert "# %%\nx = 1")                     ; no final newline
    (python-mode)
    (code-cells-mode)
    (setq-local overblock-run-backend (overblock-pycell--backend))
    (setq buffer-read-only t)
    (goto-char (point-min))
    (let ((size (buffer-size)))
      (pcase-let ((`(,beg ,end) (code-cells--bounds nil nil t)))
        (overblock-run-show beg end "out" 0.1))
      ;; The text is untouched, and the result is there all the same:
      ;; with no newline the block hangs on its anchor.
      (should (= (buffer-size) size))
      (should (overblock-in (point-min) (point-max) 'result)))))

(ert-deftest overblock-pycell-test-the-scroll-runner-arms-no-timer-in-batch ()
  "Loading the scrolling runner in a batch session arms nothing.
`make test' loads every file of test/, the runner too, and the runner
arms a timer that calls `kill-emacs' only outside batch.  A batch
session runs its timers whenever it waits for a process."
  (require 'run-scroll)
  (should-not (seq-find (lambda (timer)
                          (eq (timer--function timer) 'run-scroll--all))
                        timer-list)))

(ert-deftest overblock-pycell-test-the-mode-owns-the-outline-advice ()
  "The advice on `outline-flag-region' comes with a notebook and goes with it.
Loading the file adds no advice."
  (let ((advised (lambda ()
                   (and (advice-member-p #'overblock-pycell--outline-flag-blocks
                                         'outline-flag-region)
                        t))))
    ;; No notebook is open, so no advice: every test that turns the mode
    ;; on turns it off again.
    (should-not (funcall advised))
    (let ((one (generate-new-buffer "one.py"))
          (two (generate-new-buffer "two.py")))
      (unwind-protect
          (progn
            (dolist (buffer (list one two))
              (with-current-buffer buffer
                (insert "# %%\nx = 1\n")
                (python-mode)
                (code-cells-mode)
                (setq-local overblock-run-backend (overblock-pycell--backend))
                (overblock-pycell-mode 1)))
            (should (funcall advised))
            ;; The second notebook still wants it.
            (with-current-buffer one (overblock-pycell-mode -1))
            (should (funcall advised))
            (with-current-buffer two (overblock-pycell-mode -1))
            (should-not (funcall advised)))
        (mapc #'kill-buffer (list one two))
        (advice-remove 'outline-flag-region #'overblock-pycell--outline-flag-blocks)))))

(ert-deftest overblock-pycell-test-a-result-does-not-come-back-with-the-mode-off ()
  "The end of a run shows nothing in a notebook whose mode is off.
Turning the mode off removes the blocks and the bars.  A result put
back afterwards would have no bars and no hooks of the mode."
  (overblock-pycell-test--with-cells
    (pcase-let ((`(,beg ,end) (code-cells--bounds nil nil t)))
      (let ((from (copy-marker beg))
            (to (copy-marker end t)))
        (overblock-pycell-mode 1)
        (overblock-run--show-in-notebook from to "out" 0.1 nil)
        (should (overblock-in (point-min) (point-max) 'result))
        (overblock-pycell-mode -1)
        (should-not (overblock-in (point-min) (point-max) 'result))
        ;; Both the ticker and the end of the cell come this way.
        (overblock-run--show-in-notebook from to "more" 0.2 'running 2)
        (overblock-run--show-in-notebook from to "out" 0.3 nil)
        (should-not (overblock-in (point-min) (point-max) 'result))))))

(ert-deftest overblock-pycell-test-a-region-of-no-length-leaves-the-shell-busy-no-longer ()
  "A cell whose region has no length takes a run down instead of wedging it.
`overblock-show' returns nil instead of a zero-length overlay, which
would evaporate.  In a live run the region of a cell can be empty,
because insertions move the markers of the queue, and an error in the
process filter would leave the shell busy."
  (overblock-pycell-test--with-notebook "# %%\nx = 1\n"
    ;; The empty region the filter received: two markers on one place.
    (let ((beg (copy-marker (point-min)))
          (end (copy-marker (point-min))))
      (should (= beg end))
      ;; No signal, and nothing shown: there is no place to show it.
      (should-not (overblock-run-show beg end "" 0.0)))
    ;; The filter path survives it, and the next cell of the run still
    ;; shows its result.
    (overblock-run--show-in-notebook (copy-marker (point-min))
                              (copy-marker (point-min))
                              "" 0.0 nil)
    (should (overblock-run-show (copy-marker (+ 5 (point-min)))
                          (copy-marker (point-max))
                          "2" 0.0))))

(ert-deftest overblock-pycell-test-a-result-of-one-line-is-not-a-prompt ()
  "The colour comint paints a prompt with does not reach a result.
comint calls a chunk of output that ends without a newline a prompt,
and a cell that prints one line arrives as one such chunk.  Only that
face goes: ansi-color and comint-mime put the colours of the output in
the same property."
  (let ((comint-prompt-regexp "^In \\[[0-9]+\\]: "))
    (let ((text (overblock-pycell--clean
                 (propertize "one" 'font-lock-face
                             'comint-highlight-prompt))))
      (should (equal (substring-no-properties text) "one"))
      ;; No property, not a nil: a nil is a face run of its own, and
      ;; face runs cost redisplay time.
      (should-not (memq 'font-lock-face (text-properties-at 0 text))))
    ;; A run that carries the prompt face beside a colour of its own
    ;; keeps the colour, and a run without the prompt face is untouched.
    (let ((text (overblock-pycell--clean
                 (concat (propertize "red" 'font-lock-face
                                     '(bold comint-highlight-prompt))
                         "\n"
                         (propertize "plain" 'font-lock-face 'shadow)))))
      (should (equal (substring-no-properties text) "red\nplain"))
      (should (eq (get-text-property 0 'font-lock-face text) 'bold))
      (should (eq (get-text-property 4 'font-lock-face text) 'shadow)))))

(provide 'overblock-pycell-test)
;;; overblock-pycell-test.el ends here
