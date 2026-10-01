;;; overblock-repl-test.el --- Tests for overblock-repl -*- lexical-binding: t; -*-

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
;; The output of a shell, cut loose from that shell: the properties
;; that go, and the tables laid out again.

;;; Code:

(require 'ert)
(require 'overblock-repl)
(require 'overblock-test-common)

(ert-deftest overblock-repl-test-detach-flattens-a-copied-table ()
  "A copied vtable gets literal columns and no dead bindings.
comint-mime renders a DataFrame as a vtable, which aligns with pixel
targets of the shell window and carries the keymap of a live table.
In a block the targets are wrong and no binding finds a table."
  (let* ((cell (propertize "alpha" 'keymap (make-sparse-keymap)
                           'mouse-face 'highlight
                           'help-echo "Click to sort"))
         (gap (propertize " " 'display '(space :align-to (104))))
         (clean (overblock-repl-detach (concat cell gap "beta"))))
    ;; Real spaces replace the stretch.
    (should-not (text-property-not-all 0 (length clean) 'display nil clean))
    (should (string-match-p "\\`alpha +beta\\'" (substring-no-properties clean)))
    ;; No property offers a click.
    (dolist (prop '(keymap local-map mouse-face help-echo))
      (should-not (text-property-not-all 0 (length clean) prop nil clean)))))

(ert-deftest overblock-repl-test-table-is-laid-out-in-characters ()
  "A copied table gets columns that no face can move.
A vtable aligns with pixel stretches of the window that drew it, and
measures a header cell in the face of a header, so another face moves
its columns."
  (skip-unless (fboundp 'make-vtable))
  (let* ((clean (overblock-repl-detach (overblock-test-common-vtable-text)))
         (lines (split-string (substring-no-properties clean) "\n")))
    ;; One column starts at the same place on every row.
    (should (= (length lines) 4))
    (let ((column (string-search "beta_longer" (car lines))))
      (should column)
      (dolist (line (cdr lines))
        (should (eq (string-match-p "[0-9]" line column) column))))
    ;; The column names are bold.
    (should (memq 'bold (ensure-list (get-text-property 0 'face clean))))
    ;; No stretch is left.
    (should-not (text-property-not-all 0 (length clean) 'display nil clean))))

(ert-deftest overblock-repl-test-table-reads-a-getter ()
  "The cells of a table come from its getter where it has one.
comint-mime gives vtable a list for each row and no getter, but a
table can bring its own."
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

(ert-deftest overblock-repl-test-first-lines-of-zero-is-every-line ()
  "A limit of zero takes every line, as the options that pass one mean."
  (should (equal (overblock-repl-first-lines "a\nb\nc\n" 0) '("a" "b" "c" "")))
  (should (equal (overblock-repl-first-lines "a\nb\nc\n" 2) '("a" "b"))))

(ert-deftest overblock-repl-test-a-copy-keeps-what-the-columns-carry ()
  "The copy of a table keeps every column property, `min-width' included.
comint-mime gives each column a `:min-width' and no `:width', so a
copy of a few keys would be narrower than the table."
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
Each table is a region of its own, and the text after a table keeps
the newline its run swallowed."
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

(ert-deftest overblock-repl-test-detach-drops-the-shell-s-bookkeeping ()
  "A detached copy keeps no property that belongs to the shell buffer.
comint marks its output as a field with sticky boundaries and change
hooks, and under `comint-prompt-read-only' makes the prompts
read-only.  A copy with those puts read-only text on the kill ring.
The face stays."
  (let* ((text (propertize "42" 'read-only t 'field 'output
                           'front-sticky t 'rear-nonsticky t
                           'inhibit-line-move-field-capture t
                           'insert-in-front-hooks '(ignore)
                           'insert-behind-hooks '(ignore)
                           'modification-hooks '(ignore)
                           'keymap (make-sparse-keymap)
                           'mouse-face 'highlight
                           'help-echo "shell"
                           'face 'bold))
         (copy (overblock-repl-detach text)))
    (should (equal (substring-no-properties copy) "42"))
    (dolist (property '(read-only field front-sticky rear-nonsticky
                                  inhibit-line-move-field-capture insert-in-front-hooks
                                  insert-behind-hooks modification-hooks
                                  keymap mouse-face help-echo))
      (should-not (get-text-property 0 property copy)))
    ;; The face stays.
    (should (eq (get-text-property 0 'face copy) 'bold))))

(provide 'overblock-repl-test)
;;; overblock-repl-test.el ends here
