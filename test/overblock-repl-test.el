;;; overblock-repl-test.el --- Tests for overblock-repl -*- lexical-binding: t; -*-

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
;; The output of a shell, cut loose from that shell: the properties
;; that go, the tables laid out again, the images capped.

;;; Code:

(require 'ert)
(require 'overblock-repl)

(defconst overblock-repl-test--image
  (propertize " " 'display '(image :type png :data "x"))
  "A stand-in for what comint-mime inserts for an image.")

(defun overblock-repl-test--vtable-text ()
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

(ert-deftest overblock-repl-test-fit-caps-an-image ()
  "An image drawn inline is capped to a share of the window.
A block taller than the window bounces the wheel backwards off itself
and cannot be scrolled past at all."
  (let ((buffer (get-buffer-create "*pycell test fit*")))
    (unwind-protect
        (with-current-buffer buffer
          (set-window-buffer (selected-window) buffer)
          (let ((line (concat "x" overblock-repl-test--image)))
            (let* ((overblock-image-height 0.5)
                   (fitted (overblock-repl-fit line)))
              (should (= (plist-get (cdr (overblock-image-in fitted)) :max-height)
                         (round (* 0.5 (window-body-height
                                        (selected-window) t)))))
              ;; the line kept for the popup is not touched
              (should-not (plist-get (cdr (overblock-image-in line)) :max-height)))
            ;; zero draws it at its own size
            (let* ((overblock-image-height 0)
                   (fitted (overblock-repl-fit line)))
              (should-not (plist-get (cdr (overblock-image-in fitted))
                                     :max-height)))))
      (kill-buffer buffer))))

(ert-deftest overblock-repl-test-fit-caps-from-an-unshown-buffer ()
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
            (let* ((overblock-image-height 0.5)
                   (line (concat "x" overblock-repl-test--image))
                   (fitted (overblock-repl-fit line)))
              (should-not (get-buffer-window notebook t))
              (should (= (plist-get (cdr (overblock-image-in fitted)) :max-height)
                         (round (* 0.5 (window-body-height
                                        (selected-window) t))))))))
      (kill-buffer elsewhere)
      (kill-buffer notebook))))

(ert-deftest overblock-repl-test-detach-flattens-a-copied-table ()
  "A copied vtable gets literal columns and no dead bindings.
comint-mime renders a DataFrame as a vtable in the shell buffer, which
aligns with pixel targets measured for that window and carries the
keymap of a live table.  The block shows a copy: the targets land
elsewhere, and no binding can find a table to sort."
  (let* ((cell (propertize "alpha" 'keymap (make-sparse-keymap)
                           'mouse-face 'highlight
                           'help-echo "Click to sort"))
         (gap (propertize " " 'display '(space :align-to (104))))
         (clean (overblock-repl-detach (concat cell gap "beta"))))
    ;; the stretch is gone, and real spaces stand in its place
    (should-not (text-property-not-all 0 (length clean) 'display nil clean))
    (should (string-match-p "\\`alpha +beta\\'" (substring-no-properties clean)))
    ;; and nothing promises a click any more
    (dolist (prop '(keymap local-map mouse-face help-echo))
      (should-not (text-property-not-all 0 (length clean) prop nil clean)))))

(ert-deftest overblock-repl-test-table-is-laid-out-in-characters ()
  "A copied table gets columns that no face can move.
A vtable aligns with stretches of pixels measured in the window that
drew it, and it measures a header cell in the face of a header: a copy
shown in another face had the header squashed and the rows apart."
  (skip-unless (fboundp 'make-vtable))
  (let* ((clean (overblock-repl-detach (overblock-repl-test--vtable-text)))
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

(ert-deftest overblock-repl-test-table-reads-a-getter ()
  "The cells of a table come from its getter where it has one.
comint-mime hands vtable a list for each row and no getter, so the
default reading is the one that runs, but a table is free to bring its
own."
  (skip-unless (fboundp 'make-vtable))
  (let* ((text (with-temp-buffer
                 (make-vtable
                  :use-header-line nil
                  :columns '("first" "second")
                  :objects '((1 . "one") (2 . "two"))
                  :getter (lambda (object index _table)
                            (if (zerop index) (car object) (cdr object))))
                 (buffer-string)))
         (clean (substring-no-properties (overblock-repl-detach text))))
    (should (equal (split-string clean "\n")
                   '("first  second" "1      one" "2      two")))))

(ert-deftest overblock-repl-test-fit-leaves-a-slice-alone ()
  "A sliced image keeps its geometry: the cap is for a whole image.
Emacs 31 slices a tall image into a row for each line, and the fractions
in the slice were worked out against the height the image had — a cap
under them would draw bands with gaps."
  (let* ((image '(image :type png :data "x"))
         (slice '(slice 0.0 0.5 1.0 0.5))
         (line (propertize " " 'display (list slice image))))
    (cl-letf (((symbol-function 'overblock-image-limit) (lambda () 100)))
      (let ((fitted (overblock-repl-fit line)))
        (should (equal (get-text-property 0 'display fitted)
                       (list slice image)))))))

(provide 'overblock-repl-test)
;;; overblock-repl-test.el ends here
