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

(ert-deftest overblock-repl-test-a-result-of-one-line-is-not-a-prompt ()
  "The colour comint paints a prompt with does not reach a result.
comint calls a chunk of output that ends without a newline a prompt,
and a cell that prints one line arrives as one such chunk.  Only that
face goes: ansi-color and comint-mime put the colours of the output in
the same property."
  (progn
    (let ((text (overblock-repl-drop-prompt-face
                 (propertize "one" 'font-lock-face
                             'comint-highlight-prompt))))
      (should (equal (substring-no-properties text) "one"))
      ;; No property, not a nil: a nil is a face run of its own, and
      ;; face runs cost redisplay time.
      (should-not (memq 'font-lock-face (text-properties-at 0 text))))
    ;; A run that carries the prompt face beside a colour of its own
    ;; keeps the colour, and a run without the prompt face is untouched.
    (let ((text (overblock-repl-drop-prompt-face
                 (concat (propertize "red" 'font-lock-face
                                     '(bold comint-highlight-prompt))
                         "\n"
                         (propertize "plain" 'font-lock-face 'shadow)))))
      (should (equal (substring-no-properties text) "red\nplain"))
      (should (eq (get-text-property 0 'font-lock-face text) 'bold))
      (should (eq (get-text-property 4 'font-lock-face text) 'shadow)))))

(ert-deftest overblock-repl-test-a-file-line-becomes-an-image ()
  "A line naming a PNG comes in as the image, bytes and all.
The R wrapper of overblock-rmd writes one such line for every page.
Each becomes what comint-mime gives the Python notebook: one space
with the image and the bytes of the file, for the save button and the
pop-out.  The
newline before the line goes too, so no blank row comes before the
figure."
  (skip-unless (image-type-available-p 'png))
  (let ((file (make-temp-file "overblock-repl-test" nil ".png")))
    (unwind-protect
        (progn
          (with-temp-file file (set-buffer-multibyte nil) (insert "\x89PNG-bytes"))
          (let* ((clean (overblock-repl-file-images
                         (format "[1] 1\n\nfig:%s" file) "fig:"))
                 (image (overblock-image-in clean)))
            (should image)
            (should (equal (plist-get (cdr image) :data) "\x89PNG-bytes"))
            (should (equal (substring-no-properties clean) "[1] 1\n "))
            ;; Two pages, two images, no blank rows between them.
            (should (= 2 (cl-count ?\s (substring-no-properties
                                        (overblock-repl-file-images
                                         (format "\nfig:%s\n\nfig:%s" file file)
                                         "fig:")))))))
      (delete-file file))
    ;; A missing file is named, not drawn.
    (should (string-match-p "\\[figure /no/such\\.png\\]"
                            (overblock-repl-file-images "fig:/no/such.png" "fig:")))))

(ert-deftest overblock-repl-test-a-prompt-on-the-last-line-goes ()
  "A prompt at the end goes, on a line of its own or after the output.
Only a prompt is no output at all."
  (should (equal (overblock-repl-strip-trailing-prompt "[1] 32\n> " "> ") "[1] 32"))
  (should (equal (overblock-repl-strip-trailing-prompt "abc>>> " "^>>> ") "abc"))
  (should (equal (overblock-repl-strip-trailing-prompt "> " "> ") ""))
  (should (equal (overblock-repl-strip-trailing-prompt "a\n> b" "> ") "a\n> b")))

(provide 'overblock-repl-test)
;;; overblock-repl-test.el ends here
