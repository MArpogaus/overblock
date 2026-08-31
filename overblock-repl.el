;;; overblock-repl.el --- The output of a shell, made showable  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Marcel Arpogaus

;; Author: Marcel Arpogaus <znepry.necbtnhf@tznvy.pbz>
;; Assisted-by: Claude:claude-opus-5
;; Assisted-by: Claude:claude-fable-5
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1"))
;; Keywords: convenience, tools
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

;; What a shell prints is not what a block can show.  A copy of it
;; carries the keymap of the shell, alignment measured in another
;; window, a live vtable that belongs to that buffer, and images at
;; whatever size they came in.
;;
;;     (overblock-repl-detach (buffer-substring beg end))
;;
;; cuts a copy loose from all of that: the properties of the shell go,
;; the columns of a table are laid out in characters, and the table
;; keeps its object under `overblock-repl-table\=' so a caller can show
;; it live elsewhere, and `overblock-repl-first-lines\=' takes the head of
;; a long output without reading the rest of it.  Capping the images of a
;; line belongs to the layer: `overblock-image-cap\='.
;;
;; The prompts are the caller\='s business: what one looks like belongs
;; to the shell it came from, and this file needs no comint at all.

;;; Code:

(require 'overblock)
;; comint-mime renders a table with it, and a copy of that table is laid
;; out again here.  Optional, as it is in comint-mime: an Emacs without
;; vtable shows the text of the table as it came.
(require 'vtable nil t)
(require 'seq)
(require 'subr-x)

(defun overblock-repl--table-regions (text)
  "Return every (TABLE BEG END) of TEXT, front to back.
comint-mime renders an HTML table with vtable, a DataFrame among them,
and the copy carries the table object in a text property.

One table is several runs of that property rather than one stretch: the
padding that `overblock-flattened\=' writes in place of the alignment
stretches carries no properties of its own.  So the runs of one table
are joined, and a run that names a different table starts a region of
its own — a cell that shows two frames used to lose the second and
everything between them, because the first and the last run were read as
one table.

Run to run, not character to character: measured, a step of one cost 30
milliseconds over a hundred thousand characters and 223 over eight
hundred thousand, where a jump costs nothing."
  (when (fboundp 'vtable-p)
    (let ((len (length text))
          (pos 0)
          regions)
      (while (and pos
                  (setq pos (text-property-not-all pos len 'vtable nil text)))
        (let ((here (get-text-property pos 'vtable text))
              (next (or (next-single-property-change pos 'vtable text) len)))
          (when (vtable-p here)
            (if (eq here (car (car regions)))
                (setf (nth 2 (car regions)) next)
              (push (list here pos next) regions)))
          (setq pos (and (< next len) next))))
      (nreverse regions))))

(defun overblock-repl-table-in (text)
  "Return the table TEXT was laid out from, or nil.
`overblock-repl-detach\=' leaves the table object on the text it laid
out, under `overblock-repl-table\=', so a caller can show it live
elsewhere with `overblock-repl-table-copy\='."
  (when-let* ((pos (text-property-not-all 0 (length text)
                                          'overblock-repl-table nil text)))
    (get-text-property pos 'overblock-repl-table text)))

(defun overblock-repl-table-copy (table)
  "Return a table of the rows and columns of TABLE, for another buffer.
The table of a result belongs to the shell that drew it.  Emacs 31
refuses to insert one vtable into a second buffer — \"A vtable cannot be
inserted into more than one buffer\" — and even where it is allowed, two
buffers holding one object is not a state worth having."
  ;; The columns whole: `make-vtable' takes a `vtable-column' where it
  ;; takes a plist, and a plist of four keys dropped what comint-mime
  ;; actually sets — it gives every column a `:min-width' of its name's
  ;; length and no `:width' at all, so the copy came out narrower than
  ;; the table it was made from.
  (make-vtable :columns (mapcar #'copy-vtable-column (vtable-columns table))
               :objects (vtable-objects table)
               :getter (vtable-getter table)
               :formatter (vtable-formatter table)
               :separator-width (vtable-separator-width table)
               ;; The rows show in the order the first table showed them,
               ;; which is the order of its objects put through its sort.
               :sort-by (vtable-sort-by table)
               ;; comint-mime draws the names of the columns into the
               ;; buffer, where `make-vtable' would put them on the
               ;; window's header line and shift every row up by one.
               :use-header-line (vtable-use-header-line table)
               :insert nil))

(defun overblock-repl--table-text (table)
  "Return TABLE as text whose columns line up in characters.
A vtable aligns with stretches of pixels measured in the window that
drew it, and it measures a header cell in the face of a header.  A copy
is shown elsewhere, in a face of its own, so the columns are laid out
again here: one space of padding to the widest cell of each column, and
nothing that a face can move."
  (let* ((columns (vtable-columns table))
         ;; Read once for the table, not once for every cell: it is a
         ;; slot accessor, and measured over a sixty by ten frame — the
         ;; shape pandas hands comint-mime — asking per cell cost 0.95 of
         ;; the 2.67 milliseconds the whole layout took.
         (getter (vtable-getter table))
         (rows (cons (mapcar #'vtable-column-name columns)
                     (mapcar
                      (lambda (object)
                        (let ((index -1))
                          (mapcar
                           (lambda (_column)
                             (setq index (1+ index))
                             (format "%s"
                                     (if getter
                                         (funcall getter object index table)
                                       (elt object index))))
                           columns)))
                      (vtable-objects table))))
         ;; Every row has a cell for every column, the header row
         ;; included, so a column is as wide as its widest cell.
         (widths (seq-map-indexed
                  (lambda (_column index)
                    (apply #'max (mapcar (lambda (row)
                                           (string-width (nth index row)))
                                         rows)))
                  columns))
         (lines (mapcar
                 (lambda (row)
                   (string-trim-right
                    (string-join
                     (seq-mapn (lambda (cell width)
                                 (concat cell
                                         (make-string (- width
                                                         (string-width cell))
                                                      ?\s)))
                               row widths)
                     "  ")))
                 rows)))
    ;; the names of the columns, in bold as a markdown table has them
    (setcar lines (propertize (car lines) 'face 'bold))
    (string-join lines "\n")))

(defun overblock-repl-detach (text)
  "Return the part of TEXT a block shows, cut loose from the shell.
The outer whitespace goes, except whitespace that carries a display
property: comint-mime renders an image as one space with such a
property, and `string-trim\=' would delete it.

What the shell buffer shows is not what a copy of it shows.  comint-mime
renders a DataFrame as a vtable, which aligns its columns with pixel
targets measured in that window and carries the keymap of a live table.
In a result block the targets land elsewhere, and no binding of that
keymap can find a table.  So the columns become literal spaces, the
keymap, the mouse face and the help echo go, and a table keeps its
object under `overblock-repl-table\=', which a caller can show live."
  (let* ((beg 0)
         (end (length text))
         (blank (lambda (i) (and (memq (aref text i) '(?\s ?\t ?\n ?\r))
                                 (not (get-text-property i 'display text))))))
    (while (and (< beg end) (funcall blank beg)) (setq beg (1+ beg)))
    (while (and (< beg end) (funcall blank (1- end))) (setq end (1- end)))
    (let ((copy (let ((cut (substring text beg end)))
                  ;; Only a rendering leaves alignment stretches behind,
                  ;; and a stretch is a display property: plain output
                  ;; skips the copy through a buffer.  Measured, that
                  ;; round trip costs 23 milliseconds over eight hundred
                  ;; thousand characters of propertized text.
                  (if (text-property-not-all 0 (length cut) 'display nil cut)
                      (overblock-flattened cut)
                    cut))))
      (remove-list-of-text-properties
       0 (length copy) '(keymap local-map mouse-face help-echo) copy)
      ;; Back to front, so the places of the regions before each one
      ;; still hold.  The newline a run swallowed is put back: without it
      ;; the output that follows the table is glued to its last row.
      (dolist (region (reverse (overblock-repl--table-regions copy)))
        (pcase-let* ((`(,table ,tbeg ,tend) region)
                     (laid-out (propertize
                                (overblock-repl--table-text table)
                                'overblock-repl-table table)))
          (setq copy (concat (substring copy 0 tbeg)
                             laid-out
                             (if (eq (aref copy (1- tend)) ?\n) "\n" "")
                             (substring copy tend)))))
      copy)))

(defun overblock-repl-first-lines (text limit)
  "Return the first LIMIT lines of TEXT.
Only that much is looked at and only that much is copied: a result of
ten thousand lines costs what a result of twelve costs, which is what a
tick five times a second needs."
  (if (<= limit 0)
      ;; `pycell-max-lines' is a natnum, so zero is a legal value for it,
      ;; and the scan below counts from one: it never met a limit of zero
      ;; and read and copied the whole result instead — measured, 18.6
      ;; milliseconds and a full copy over twenty thousand lines, five
      ;; times a second while the cell runs.
      nil
    (let ((pos 0) (count 0) (cut nil))
      (while (and (null cut)
                  (setq pos (string-search "\n" text pos)))
        (setq count (1+ count)
              pos (1+ pos))
        (when (>= count limit) (setq cut (1- pos))))
      (split-string (if cut (substring text 0 cut) text) "\n"))))

(defun overblock-repl-count-lines (text)
  "Return how many lines TEXT holds.
The whole of it is searched, so a caller that already knows the number
had better not ask: measured, ten thousand lines cost 3.1 milliseconds
and a fold of such a result asked on every keypress.

The search is why this is a loop and not `cl-count\=': over the same ten
thousand lines, `string-search\=' measured 1.5 milliseconds against 8.0
for `cl-count\=' and 108 for `seq-count\=', all three answering alike."
  (let ((pos 0) (count 1))
    (while (setq pos (string-search "\n" text pos))
      (setq count (1+ count)
            pos (1+ pos)))
    count))

(provide 'overblock-repl)
;;; overblock-repl.el ends here
