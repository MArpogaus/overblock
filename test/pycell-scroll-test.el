;;; pycell-scroll-test.el --- Scrolling regression test -*- lexical-binding: t; -*-

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

;; Run with: make scroll
;;
;; A block is one buffer line and can be taller than the window, which
;; is the case redisplay gets wrong.  This test scrolls a window up
;; over such blocks and fails when the window ever moves down.  It
;; guards the promise the package makes: with the scroll options at
;; their defaults, the wheel moves through blocks in one direction.
;;
;; The test needs a graphical frame, because only there does a line
;; have a pixel height and can a window show part of one.  A batch
;; session skips it; without a display, run it under `xvfb-run', which
;; is what the CI does.

;;; Code:

(require 'ert)
(require 'pycell)
(require 'pixel-scroll)

(defun pycell-scroll-test--source (cells paragraphs)
  "Return buffer text of CELLS markdown cells of PARAGRAPHS each.
Enough prose that each rendered block is taller than the window."
  (mapconcat
   (lambda (n)
     (concat (format "# %%%% [markdown]\n# ## Cell %d\n#\n" n)
             (mapconcat
              (lambda (i)
                (format "# Paragraph %d, with prose that runs on for a\n\
# while so the block grows past the window height.\n#\n" i))
              (number-sequence 1 paragraphs) "")
             (format "\n# %%%%\ny%d = %d\nprint(y%d)\n\n" n n n)))
   (number-sequence 1 cells) ""))

(defun pycell-scroll-test--reversals ()
  "Scroll the window up to the top, 40 pixels at a time.
Return the steps that went wrong, as a list of strings.  Scrolling up
may only lower the window start, or keep it and lower the vscroll,
and it may not signal on the way: a line of no height, or a hidden
run that begins a line, stops `pixel-scroll-precision-scroll-up' with
a beginning-of-buffer error in the middle of a cell, which is a
refusal to scroll and not the end of the buffer."
  (goto-char (point-max))
  (set-window-start nil (point))
  (set-window-vscroll nil 0 t)
  (redisplay t)
  (let ((previous (cons (window-start) (window-vscroll nil t)))
        (steps 0)
        reversals)
    (while (< (cl-incf steps) 250)
      (condition-case err
          (pixel-scroll-precision-scroll-up 40)
        ;; At the top of the buffer the refusal is the right answer.
        (beginning-of-buffer
         (unless (= (window-start) (point-min))
           (push (format "step %d: %S at line %d" steps (car err)
                         (line-number-at-pos (window-start)))
                 reversals))))
      (redisplay t)
      (let ((now (cons (window-start) (window-vscroll nil t))))
        (when (or (> (car now) (car previous))
                  (and (= (car now) (car previous))
                       (> (cdr now) (cdr previous))))
          (push (format "step %d: %d+%d to %d+%d" steps
                        (line-number-at-pos (car previous)) (cdr previous)
                        (line-number-at-pos (car now)) (cdr now))
                reversals))
        ;; Stop at the top; what the scroll command does once there is
        ;; not this test's business.
        (when (= (car now) (point-min))
          (setq steps 999))
        (setq previous now)))
    (nreverse reversals)))

(defun pycell-scroll-test--stalls ()
  "Scroll the window down from the top, 40 pixels at a time.
Return the steps that went wrong.  Scrolling down may only raise the
window start, or keep it and raise the vscroll: a block the wheel
bounces off keeps the window where it is, or throws it back, and the
buffer below stays out of reach."
  (goto-char (point-min))
  (set-window-start nil (point))
  (set-window-vscroll nil 0 t)
  (redisplay t)
  (let ((previous (cons (window-start) (window-vscroll nil t)))
        (steps 0)
        stalls)
    (while (< (cl-incf steps) 250)
      (condition-case err
          (pixel-scroll-precision-scroll-down 40)
        (end-of-buffer
         (unless (pos-visible-in-window-p (point-max))
           (push (format "step %d: %S at line %d" steps (car err)
                         (line-number-at-pos (window-start)))
                 stalls))))
      (redisplay t)
      (let ((now (cons (window-start) (window-vscroll nil t))))
        (when (or (< (car now) (car previous))
                  (and (= (car now) (car previous))
                       (< (cdr now) (cdr previous))))
          (push (format "step %d: %d+%d to %d+%d" steps
                        (line-number-at-pos (car previous)) (cdr previous)
                        (line-number-at-pos (car now)) (cdr now))
                stalls))
        (when (pos-visible-in-window-p (point-max))
          (setq steps 999))
        (setq previous now)))
    ;; Reaching the end is the point: a wheel that inches forward for
    ;; 250 events without arriving is stuck as surely as one that
    ;; bounces, and an empty list would call that a pass.
    (unless (> steps 900)
      (push (format "the end stayed out of reach, at line %d"
                    (line-number-at-pos (window-start)))
            stalls))
    (nreverse stalls)))

(ert-deftest pycell-scroll-test-defaults ()
  "With the scroll options at their defaults the window never reverses.
Two block shapes on purpose: text blocks taller than the window, and
a short one after them, which is where redisplay changes lines."
  (skip-unless (display-graphic-p))
  ;; Without a converter the cells stay plain source and the test
  ;; would pass without a single block in the buffer.
  (skip-unless (overblock-md-program))
  (let ((buffer (generate-new-buffer "*pycell scroll*")))
    (unwind-protect
        (progn
          (switch-to-buffer buffer)
          (delete-other-windows)
          (insert (pycell-scroll-test--source 2 14)
                  "# %% [markdown]\n# A short one.\n\n# %%\nz = 3\n")
          (python-mode)
          (code-cells-mode)
          (pycell-mode 1)
          (redisplay t)
          ;; The blocks are the point of the test.
          (should (= (length (seq-filter
                              (lambda (o) (overblock-get o :parts))
                              (overblock-in (point-min) (point-max) 'markdown)))
                     3))
          (should (equal (pycell-scroll-test--stalls) nil))
          (should (equal (pycell-scroll-test--reversals) nil)))
      (kill-buffer buffer))))

(ert-deftest pycell-scroll-test-figures ()
  "A wheel passes a result that holds a figure, in both directions.
The rows of such a result ride a string, because a display property
swallows an image.  Measured while this test was written: with those rows
on the after-string of the newline and the newline itself replaced by an
empty display string, `pixel-scroll-precision-scroll-up\=' refused to
pass the block — 280 refusals with a beginning-of-buffer error over six
figures — where the same buffer with the newline left alone scrolls to
the top without one.  The rows therefore ride the anchor, and this test
is what says so."
  (skip-unless (display-graphic-p))
  (skip-unless (image-type-available-p 'svg))
  (let ((buffer (generate-new-buffer "*pycell figures*"))
        ;; A square of a colour, as tall as a figure from matplotlib.
        (figure (create-image
                 (concat "<svg xmlns=\"http://www.w3.org/2000/svg\" "
                         "width=\"300\" height=\"600\">"
                         "<rect width=\"300\" height=\"600\" fill=\"#7aa2f7\"/>"
                         "</svg>")
                 'svg t)))
    (unwind-protect
        (progn
          (switch-to-buffer buffer)
          (delete-other-windows)
          (dotimes (n 6) (insert (format "# %%%%\nplot(%d)\n\n" n)))
          (python-mode)
          (code-cells-mode)
          (goto-char (point-min))
          (while (< (point) (point-max))
            (pcase-let ((`(,beg ,end) (code-cells--bounds nil nil t)))
              (pycell--show beg end
                            (concat "a figure\n"
                                    (propertize " " 'display figure))
                            0.2)
              (goto-char end)))
          (redisplay t)
          (should (= (length (overblock-in (point-min) (point-max) 'result))
                     6))
          ;; every one of them holds a figure, or the test proves nothing
          (should (seq-every-p (lambda (block)
                                 (overblock-image-in
                                  (or (overlay-get block 'after-string) "")))
                               (overblock-in (point-min) (point-max)
                                             'result)))
          (should (equal (pycell-scroll-test--stalls) nil))
          (should (equal (pycell-scroll-test--reversals) nil)))
      (kill-buffer buffer))))

(provide 'pycell-scroll-test)
;;; pycell-scroll-test.el ends here
