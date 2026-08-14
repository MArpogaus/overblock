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
  (let ((pycell-max-lines 10))
    (should (equal (pycell--body-lines (list "text" pycell-test--image "more"))
                   (list "text" pycell-test--image)))))

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
           (bov (overlay-get ov 'pycell-body))
           (body (or (overlay-get bov 'display)
                     (overlay-get bov 'after-string))))
      (should body)
      (outline-flag-region (pos-eol) (overlay-end ov) t)
      (should-not (or (overlay-get bov 'display)
                      (overlay-get bov 'after-string)))
      (outline-flag-region (pos-eol) (overlay-end ov) nil)
      (should (or (overlay-get bov 'display)
                  (overlay-get bov 'after-string))))))

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
  "A string and a list of candidates both resolve to a program."
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

(provide 'pycell-test)
;;; pycell-test.el ends here
