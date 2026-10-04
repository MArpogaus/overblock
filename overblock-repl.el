;;; overblock-repl.el --- The output of a shell, made showable  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Marcel Arpogaus

;; Author: Marcel Arpogaus <znepry.necbtnhf@tznvy.pbz>
;; Assisted-by: Claude:claude-opus-5
;; Assisted-by: Claude:claude-fable-5
;; Keywords: convenience, tools
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

;; What a shell prints is not what a block can show.  A copy of it
;; carries the keymap of the shell and alignment measured in another
;; window.  It also carries a live vtable that belongs to that buffer,
;; and images at whatever size they came in.
;;
;;     (overblock-repl-detach (buffer-substring beg end))
;;
;; cuts a copy loose from all of that.  The properties of the shell go,
;; and the columns of a table are laid out in characters.  The table
;; keeps its object under `overblock-repl-table', so a caller can show
;; it live elsewhere.  `overblock-repl-first-lines' takes the head of a
;; long output without reading the rest of it.  Capping the images of a
;; line belongs to the layer: `overblock-image-cap'.
;;
;; What a prompt looks like is the caller's business: it belongs to the
;; shell it came from.  `overblock-repl-strip-trailing-prompt' takes it
;; off the end of an output, and `overblock-repl-drop-prompt-face'
;; takes the face comint paints it with.  `overblock-repl-file-images'
;; reads back the image files a program named, one to a line.

;;; Code:

(require 'overblock)
;; comint-mime renders a table with vtable, and this file lays out a
;; copy. Required outright: vtable ships with Emacs 29.1, the minimum,
;; and `overblock-repl-table-copy' calls `make-vtable' unguarded.
(require 'vtable)
(require 'seq)
(require 'subr-x)

;;;; Tables

(defun overblock-repl--table-regions (text)
  "Return every (TABLE BEG END) of TEXT, front to back.
comint-mime renders an HTML table, a DataFrame among them, with
vtable, and the copy carries the table object in a text property.

One table is several runs of that property: the padding that
`overblock-flattened' writes in place of the alignment stretches has no
properties.  So the runs of one table are joined, and a run that names
a different table starts a new region.

The loop steps from run to run, because a step of one character is
slow on a long output."
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
    (nreverse regions)))

(defun overblock-repl-table-copy (table)
  "Return a table of the rows and columns of TABLE, for another buffer.
The table of a result belongs to the shell that drew it.  Emacs 31
refuses to insert one vtable into a second buffer, and two buffers must
not share one object."
  ;; Whole columns, not plists: comint-mime sets `:min-width' and no
  ;; `:width', and a plist of a few keys would drop it.
  (make-vtable :columns (mapcar #'copy-vtable-column (vtable-columns table))
               :objects (vtable-objects table)
               :getter (vtable-getter table)
               :formatter (vtable-formatter table)
               :separator-width (vtable-separator-width table)
               ;; A copy: `vtable-sort-by-current-column' calls `delq'
               ;; on this list, which would change the table of the shell.
               :sort-by (copy-tree (vtable-sort-by table))
               ;; comint-mime draws the column names into the buffer,
               ;; not on the header line of the window.
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
         ;; Read once, not per cell: the slot accessor per cell is a
         ;; third of the layout time.
         (getter (vtable-getter table))
         (rows (cons (mapcar #'vtable-column-name columns)
                     (mapcar
                      (lambda (object)
                        (seq-map-indexed
                         (lambda (_column index)
                           (format "%s"
                                   (if getter
                                       (funcall getter object index table)
                                     (elt object index))))
                         columns))
                      (vtable-objects table))))
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
    ;; The column names are bold, as in a markdown table.
    (setcar lines (propertize (car lines) 'face 'bold))
    (string-join lines "\n")))

;;;; Prompts

(defun overblock-repl-strip-trailing-prompt (text prompt)
  "Return TEXT without the PROMPT the shell left at the end of it.
PROMPT is the prompt pattern of the shell, for example
`comint-prompt-regexp' in an inferior Python or
`inferior-ess-primary-prompt' in R.  Every copy of it at the end of
TEXT goes, on a line of its own or after the whitespace of the last
line.

So does one on the same line as output without a final newline, such
as a `sys.stdout.write' or a `cat' leaves, which a PROMPT anchored to
a line start cannot see.  TEXT that is only a prompt, as after an
assignment, is then empty."
  (let ((rx (concat "\n[ \t]*\\(?:" prompt "\\)[ \t\n]*\\'")))
    (while (string-match rx text)
      (setq text (substring text 0 (match-beginning 0))))
    (if (string-match (concat "\\(?:" (string-remove-prefix "^" prompt)
                              "\\)[ \t]*\\'")
                      text)
        (substring text 0 (match-beginning 0))
      text)))

(defun overblock-repl-drop-prompt-face (text)
  "Return TEXT without the face comint paints a prompt with.
comint calls a chunk of output that ends without a newline a prompt,
and paints it `comint-highlight-prompt'.  A cell that prints one line
arrives as one such chunk.

Only that face goes: ansi-color and comint-mime put other faces in the
same property.  A run left without a face loses the property instead
of a nil value, because each face run costs redisplay time.

TEXT changes in place: pass a copy, such as one from
`buffer-substring'."
  (let ((pos 0)
        (len (length text)))
    (while (< pos len)
      (let* ((next (or (next-single-property-change pos 'font-lock-face text)
                       len))
             (face (ensure-list (get-text-property pos 'font-lock-face text)))
             (kept (remq 'comint-highlight-prompt face)))
        (unless (= (length kept) (length face))
          (if kept
              (put-text-property pos next 'font-lock-face
                                 (if (cdr kept) kept (car kept))
                                 text)
            (remove-text-properties pos next '(font-lock-face nil) text)))
        (setq pos next))))
  text)

;;;; Output

(defun overblock-repl-file-images (text tag)
  "Return TEXT with each line of TAG and a file replaced by its PNG image.
A program that draws to files names each on a line of its own, after
TAG.  Each becomes what comint-mime inserts for an image: one space
that carries it, with the bytes of the file, so a block, a save button
and a pop-out treat both alike.  Where this Emacs draws no PNG, or the
file is gone, the line names the file instead.

The newline before the line goes too, so no blank row comes before an
image."
  (if (not (string-search tag text))
      text
    (replace-regexp-in-string
     (concat "\n?" (regexp-quote tag) ".+")
     (lambda (line)
       (let ((file (substring line (+ (string-search tag line) (length tag)))))
         (if (and (image-type-available-p 'png) (file-readable-p file))
             (propertize " " 'display
                         (create-image (with-temp-buffer
                                         (set-buffer-multibyte nil)
                                         (insert-file-contents-literally file)
                                         (buffer-string))
                                       'png t))
           (format "[figure %s]" file))))
     text t t)))

(defun overblock-repl-detach (text)
  "Return the part of TEXT a block shows, cut loose from the shell.
The outer whitespace goes, except whitespace that carries a display
property: comint-mime renders an image as one space with such a
property.

Leading whitespace goes up to the last newline in it, and no further.
The indentation of the first line is content: the columns of an R
`summary' or a pandas `describe' line up on it.

comint-mime renders a DataFrame as a vtable, which aligns its columns
with pixel targets measured in the shell window and carries the keymap
of a live table.  In a block those targets are wrong and the keymap
finds no table.  So the columns become literal spaces, the keymap, the
mouse face and the help echo go, and a table keeps its object under
`overblock-repl-table', which a caller can show live.

The bookkeeping of comint goes too: fields, sticky boundaries, change
hooks and read-only prompts.  A copy with them puts read-only text on
the kill ring, and its hooks run comint functions in the buffer it is
yanked into.  The faces, the display properties of the images and the
table object stay."
  (let* ((beg 0)
         (end (length text))
         (blank (lambda (i) (and (memq (aref text i) '(?\s ?\t ?\n ?\r))
                                 (not (get-text-property i 'display text))))))
    (let ((i 0))
      (while (and (< i end) (funcall blank i))
        (when (eq (aref text i) ?\n) (setq beg (1+ i)))
        (setq i (1+ i)))
      ;; Only whitespace: there is no first line to indent.
      (when (= i end) (setq beg i)))
    (while (and (< beg end) (funcall blank (1- end))) (setq end (1- end)))
    (let ((copy (let ((cut (substring text beg end)))
                  ;; Only a rendering leaves alignment stretches, which
                  ;; are display properties. Plain output skips the slow
                  ;; round trip through a buffer.
                  (if (text-property-not-all 0 (length cut) 'display nil cut)
                      (overblock-flattened cut)
                    cut))))
      (remove-list-of-text-properties
       0 (length copy)
       '(keymap local-map mouse-face help-echo read-only field
                front-sticky rear-nonsticky inhibit-line-move-field-capture
                insert-in-front-hooks insert-behind-hooks modification-hooks)
       copy)
      ;; Back to front, so the positions of earlier regions hold. The
      ;; newline a run swallowed is put back, so the output after the
      ;; table does not join its last row.
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
  "Return the first LIMIT lines of TEXT, every line where LIMIT is zero.
Only that part of TEXT is read and copied, so a long result costs what
a short one costs, on a tick five times a second.  Zero means all
lines, as in the options that pass a limit here."
  (if (<= limit 0)
      (split-string text "\n")
    (let ((pos 0) (count 0) (cut nil))
      (while (and (null cut)
                  (setq pos (string-search "\n" text pos)))
        (setq count (1+ count)
              pos (1+ pos))
        (when (>= count limit) (setq cut (1- pos))))
      (split-string (if cut (substring text 0 cut) text) "\n"))))

(defun overblock-repl-count-lines (text)
  "Return how many lines TEXT holds.
This searches all of TEXT, so a caller that knows the number does not
ask.  A loop of `string-search' is several times faster than
`cl-count'."
  (let ((pos 0) (count 1))
    (while (setq pos (string-search "\n" text pos))
      (setq count (1+ count)
            pos (1+ pos)))
    count))

(provide 'overblock-repl)
;;; overblock-repl.el ends here
