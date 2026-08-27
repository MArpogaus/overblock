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

(ert-deftest overblock-repl-test-fit-unslices-a-tall-image ()
  "A run of slices becomes the whole image, capped, on its first row.
Emacs 31 slices an image taller than `shr-sliced-image-height\=' into a
row for each line of the window it was rendered in.  Slicing does not
make an image smaller, so leaving the slices alone left the cap with
nothing to cap — and the image cannot be capped under the slice either,
because the fractions were worked out against the height it had."
  (let* ((image '(image :type png :data "x"))
         (rows (list '(slice 0.0 0.0 1.0 0.5) '(slice 0.0 0.5 1.0 0.5)))
         (line (concat (propertize " " 'display (list (nth 0 rows) image))
                       "\n"
                       (propertize " " 'display (list (nth 1 rows) image)))))
    (cl-letf (((symbol-function 'overblock-image-limit) (lambda () 100)))
      (let* ((fitted (overblock-repl-fit line))
             (first (get-text-property 0 'display fitted))
             (later (get-text-property (1- (length fitted)) 'display fitted)))
        ;; The first row carries the image, capped and no longer sliced.
        (should (eq (car-safe first) 'image))
        (should (= (plist-get (cdr first) :max-height) 100))
        ;; The rows that followed it carry nothing.
        (should (equal later ""))))))

(ert-deftest overblock-repl-test-first-lines-of-nothing-is-nothing ()
  "A limit of zero takes no lines, rather than the whole result.
`pycell-max-lines\=' is a natnum, so zero is a legal value, and the scan
counts from one: it never met a limit of zero and read and copied
everything instead."
  (should-not (overblock-repl-first-lines "a\nb\nc\n" 0))
  (should-not (overblock-repl-first-lines "a\nb\nc\n" -1))
  (should (equal (overblock-repl-first-lines "a\nb\nc\n" 2) '("a" "b"))))

(ert-deftest overblock-repl-test-a-copy-keeps-what-the-columns-carry ()
  "The copy of a table keeps every column property, `min-width\=' included.
comint-mime gives each column a `:min-width\=' of its name\='s length and
no `:width\=' at all, and a copy built from four keys came out narrower
than the table it was made from."
  (skip-unless (fboundp 'make-vtable))
  (let* ((table (make-vtable :columns (list (list :name "alpha" :min-width 5)
                                            (list :name "b" :align 'right))
                             :objects '((1 2) (3 4))
                             :insert nil))
         (copy (overblock-repl-table-copy table)))
    (should (equal (mapcar #'vtable-column-name (vtable-columns copy))
                   '("alpha" "b")))
    (should (equal (vtable-column-min-width
                    (car (vtable-columns copy)))
                   5))
    (should (eq (vtable-column-align (cadr (vtable-columns copy))) 'right))
    ;; Objects of its own, not the table's list.
    (should (equal (vtable-objects copy) (vtable-objects table)))
    (should-not (eq (car (vtable-columns copy)) (car (vtable-columns table))))))

(ert-deftest overblock-repl-test-two-tables-both-survive ()
  "A cell that shows two frames keeps both, and what follows them.
The regions were read as the first and the last run of the property, so
the second table and everything between them was replaced by the layout
of the first — and the text after a table lost the newline its run had
swallowed."
  (skip-unless (fboundp 'make-vtable))
  (let* ((one (make-vtable :columns '("a") :objects '((1)) :insert nil))
         (two (make-vtable :columns '("b") :objects '((2)) :insert nil))
         (text (concat (propertize "one\n" 'vtable one)
                       (propertize "two\n" 'vtable two)
                       "tail\n"))
         (detached (overblock-repl-detach text)))
    (should (string-search "a" detached))
    (should (string-search "b" detached))
    (should (string-search "tail" detached))
    ;; The tail stands on a line of its own.
    (should-not (string-match-p "[^\n]tail" detached))))

(provide 'overblock-repl-test)
;;; overblock-repl-test.el ends here
