;;; overblock-pydoc-test.el --- Tests for the doc string overlay  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Marcel Arpogaus

;; Author: Marcel Arpogaus <znepry.necbtnhf@tznvy.pbz>
;; Assisted-by: Claude:claude-opus-5
;; URL: https://github.com/MArpogaus/overblock-pycell

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
;; The tests that need a converter skip themselves where none is
;; installed; `make test STRICT=1' refuses to skip.

;;; Code:

(require 'ert)
(require 'python)
(require 'overblock-pydoc)

(defconst overblock-pydoc-test--source
  "\"\"\"The module.\"\"\"


def f(x):
    \"\"\"Do a thing.

    Parameters
    ----------
    x : int
        the thing to do
    \"\"\"
    return x


class C:
    # a comment between the class and its doc string
    \"\"\"The class.\"\"\"

    def m(self):
        \"\"\"The method.\"\"\"
        s = \"\"\"data, not documentation\"\"\"
        return s
"
  "A Python buffer with a doc string of every kind.
It carries the doc string of a module, of a function, of a class behind
a comment and of a method — and one string that is data rather than
documentation.")

(defmacro overblock-pydoc-test--with (&rest body)
  "Evaluate BODY in a Python buffer holding the source above."
  (declare (indent 0))
  `(with-temp-buffer
     (python-mode)
     (insert overblock-pydoc-test--source)
     (goto-char (point-min))
     ,@body))

(defun overblock-pydoc-test--wait (count)
  "Wait until COUNT doc strings carry a rendering, and return how many do.
The rendering is asked of a process and not waited for, which is the
point of it: a test has to wait where a reader does not."
  (let ((deadline (+ (float-time) 10)))
    (while (and (< (length (overblock-in (point-min) (point-max) 'pydoc))
                   count)
                (< (float-time) deadline))
      (accept-process-output nil 0.05)))
  (length (overblock-in (point-min) (point-max) 'pydoc)))

(defun overblock-pydoc-test--first-lines ()
  "Return the first line of the prose of every doc string found."
  (mapcar (lambda (bounds)
            (car (split-string (overblock-pydoc--source (car bounds)
                                                        (cdr bounds))
                               "\n")))
          (overblock-pydoc--strings (point-min) (point-max))))

(ert-deftest overblock-pydoc-test-the-converter-paints-no-code ()
  "Every pandoc named here is told to leave a code block alone.
For the reason `overblock-md-command' gives: shr reads no CSS class,
so painting one costs the reader the syntax definitions pandoc loads
and gives them nothing."
  (dolist (command (ensure-list overblock-pydoc-command))
    (when (string-prefix-p "pandoc" command)
      (should (string-search "--no-highlight" command)))))

(ert-deftest overblock-pydoc-test-a-doc-string-opens-its-line ()
  "Every doc string is found, and a string that is data is not one.
The module, the function, the class behind its comment and the method
are documentation; the string assigned inside the method is a value."
  (overblock-pydoc-test--with
    (should (equal (overblock-pydoc-test--first-lines)
                   '("The module." "Do a thing." "The class."
                     "The method.")))))

(ert-deftest overblock-pydoc-test-a-doc-string-ends-at-its-quotes ()
  "The bounds reach from the opening quotes to past the closing ones.
`scan-sexps' cannot answer this: `python-mode' gives the first of three
quotes the syntax of a plain string delimiter, so a scan from the
start reads the first two as an empty string."
  (overblock-pydoc-test--with
    (let ((first (car (overblock-pydoc--strings (point-min) (point-max)))))
      (should (equal (buffer-substring-no-properties (car first) (cdr first))
                     "\"\"\"The module.\"\"\"")))))

(defconst overblock-pydoc-test--mispaired
  "class A:
    \"\"\"A term's behavior, with a raw string r\"\"\"raw\"\"\" in the prose.
    \"\"\"

    def m(self):
        \"\"\"Give this term's contribution.

        More prose.
        \"\"\"
        return 1
"
  "A doc string whose prose carries a quote run, and one below it.
The run ends the first string where Python ends it, and what follows
pairs the other way round: font lock paints a region that begins in
the middle of a line.")

(ert-deftest overblock-pydoc-test-a-mispaired-quote-run-is-no-doc-string ()
  "A region that does not begin where its line does is not taken.
A quote run in the prose ends a doc string early and every string
after it pairs the wrong way round; font lock paints those artefacts
with the doc face too.  One of them begins in the middle of a line —
measured, at column 51 of a line indented to four — and a rendering
laid over it would draw prose over code.  The sound doc strings around
it are still found."
  (with-temp-buffer
    (insert overblock-pydoc-test--mispaired)
    (python-mode)
    (let ((bounds (overblock-pydoc--strings (point-min) (point-max))))
      (dolist (region bounds)
        (goto-char (car region))
        (should (= (current-column) (current-indentation))))
      ;; the doc string of the method below the run is one of them
      (should (seq-some (lambda (region)
                          (string-prefix-p "\"\"\"Give this term"
                                           (buffer-substring-no-properties
                                            (car region) (cdr region))))
                        bounds)))))

(ert-deftest overblock-pydoc-test-an-assignment-is-no-doc-string ()
  "A triple-quoted value is data, wherever it stands.
python.el decides this, in `python-info-docstring-p\', and it is the
one thing a reader must be able to count on: prose drawn over a value
hides code."
  (with-temp-buffer
    (insert "s = \"\"\"data, not documentation\"\"\"\n"
            "def f():\n"
            "    t = \"\"\"data here too\"\"\"\n"
            "    return t\n")
    (python-mode)
    (should-not (overblock-pydoc--strings (point-min) (point-max)))))

(ert-deftest overblock-pydoc-test-a-quote-run-in-a-value-hides-nothing ()
  "A quote run inside an ordinary string costs no doc string its rendering.
The scan this replaced paired the quotes itself: a run inside an
f-string sent it looking for a closing fence that was not there, it
gave up at the end of the buffer, and every doc string below that line
went unrendered."
  (with-temp-buffer
    (insert "x = f\"{a!r} \x27\x27\x27\"\n"
            "class A:\n"
            "    \"\"\"The doc string below the run.\"\"\"\n")
    (python-mode)
    (should (= (length (overblock-pydoc--strings (point-min) (point-max))) 1))))

(ert-deftest overblock-pydoc-test-an-escape-keeps-a-doc-string-whole ()
  "An escape sequence in the prose does not cut the doc string in two.
Font lock paints an escape with a face of its own, in `python-mode\'
and in `python-ts-mode\' alike, so the doc face comes in runs; the
syntax scan says where the string ends."
  (with-temp-buffer
    (insert "class A:\n"
            "    \"\"\"Doc with \\n and \\alpha in it.\n\n    More.\n    \"\"\"\n")
    (python-mode)
    (let ((bounds (overblock-pydoc--strings (point-min) (point-max))))
      (should (= (length bounds) 1))
      (should (string-suffix-p "More.\n    \"\"\""
                               (buffer-substring-no-properties
                                (car (car bounds)) (cdr (car bounds))))))))

(ert-deftest overblock-pydoc-test-a-raw-doc-string-hangs-at-its-quotes ()
  "A raw doc string is rendered, and every row of it lines up.
Its region begins at the quotes and the letter that prefixes them
stands to their left, so the block hangs one column in from the code:
the rendering is padded to the column the block begins at, measured,
and not to the indentation of the line."
  (with-temp-buffer
    (insert "class A:\n    r\"\"\"Raw doc, with a \\alpha in it.\n\n    More.\n    \"\"\"\n")
    (python-mode)
    (let ((overblock-pydoc-renderer 'fontify)
          (overblock-pydoc-fontify-mode #'rst-mode))
      (goto-char (point-max))
      (overblock-pydoc-render-buffer))
    (let* ((block (car (overblock-in (point-min) (point-max) 'pydoc)))
           (lines (split-string (substring-no-properties
                                 (overblock-get block :over))
                                "\n")))
      (should block)
      ;; the block begins at the quotes, past the prefix
      (goto-char (overlay-start block))
      (should (= (current-column) 5))
      ;; the first row hangs there and carries no padding; every row
      ;; below it is padded to the same column
      ;; the label glyph of the bar falls back to a word in batch, so
      ;; the row opens with the space that follows it; what it must not
      ;; carry is the padding of the rows below
      (should-not (string-prefix-p "     " (car lines)))
      (dolist (line (cdr lines))
        (should (string-prefix-p "     " line))))))

(ert-deftest overblock-pydoc-test-the-prose-loses-its-indentation ()
  "The quotes go, and the indentation the lines share with the code.
A doc string is written where the code stands and reads as prose one
column from the left."
  (overblock-pydoc-test--with
    (let ((bounds (nth 1 (overblock-pydoc--strings (point-min) (point-max)))))
      (should (equal (overblock-pydoc--source (car bounds) (cdr bounds))
                     "Do a thing.\n\nParameters\n----------\nx : int\n    the thing to do")))))

(ert-deftest overblock-pydoc-test-the-markup-is-rendered ()
  "A doc string carries its rendering, and reST is what it is read as."
  (skip-unless (overblock-md-program))
  (overblock-pydoc-test--with
    (overblock-pydoc-mode 1)
    (unwind-protect
        (progn
          (goto-char (point-max))
          (overblock-pydoc-render-buffer)
          (should (= (overblock-pydoc-test--wait 4) 4))
          (let ((blocks (overblock-in (point-min) (point-max) 'pydoc)))
            ;; the roles of a numpydoc section survive, the underline
            ;; that marks them does not
            (let ((shown (substring-no-properties
                          (overblock-get (nth 1 blocks) :over))))
              (should (string-match-p "Parameters" shown))
              (should-not (string-match-p "----" shown)))))
      (overblock-pydoc-mode -1))))

(ert-deftest overblock-pydoc-test-the-answer-lands-nowhere-near-point ()
  "A doc string the reader walked into is left alone when the HTML lands.
The pass asks for every doc string but the one point is in, and the
answer comes back a moment later — by which time the reader may have
clicked one, or walked point into it.  Rendering it then takes the text
out from under them."
  (skip-unless (overblock-md-program))
  (overblock-pydoc-test--with
    (overblock-pydoc-mode 1)
    (unwind-protect
        (progn
          (goto-char (point-max))
          (overblock-pydoc-render-buffer)
          ;; the reader walks into the second doc string while the
          ;; converter runs; nothing has been answered yet, because
          ;; nothing here has waited
          (let ((bounds (nth 1 (overblock-pydoc--strings (point-min)
                                                         (point-max)))))
            (goto-char (1+ (car bounds)))
            (should (= (overblock-pydoc-test--wait 3) 3))
            (should-not (overblock-in (car bounds) (cdr bounds) 'pydoc))))
      (overblock-pydoc-mode -1))))

(ert-deftest overblock-pydoc-test-the-fontify-renderer-needs-no-process ()
  "Font lock renders the doc strings where the reader asks for it.
Nothing is waited for because nothing is started: the renderings are
there when the pass returns.  The lines stay as the writer wrote them,
so the rendering is exactly as tall as its source."
  (overblock-pydoc-test--with
    (let ((overblock-pydoc-renderer 'fontify)
          (overblock-pydoc-fontify-mode #'rst-mode))
      ;; out of the way first: the doc string point is in is the one
      ;; left alone, and the macro leaves point at the top of the file
      (goto-char (point-max))
      (overblock-pydoc-mode 1)
      (unwind-protect
          (let ((blocks (overblock-in (point-min) (point-max) 'pydoc)))
            ;; every doc string, and no waiting
            (should (= (length blocks) 4))
            (let* ((block (nth 1 blocks))
                   (shown (substring-no-properties
                           (overblock-get block :over)))
                   (source (overblock-pydoc--source (overlay-start block)
                                                    (overlay-end block))))
              ;; the title of a numpydoc section survives, its row of
              ;; dashes does not
              (should (string-match-p "Parameters" shown))
              (should-not (string-match-p "----" shown))
              ;; and the rendering is no taller than what it covers,
              ;; the two bars that dress it aside
              (should (<= (length (split-string shown "\n"))
                          (+ 2 (length (split-string source "\n")))))))
        (overblock-pydoc-mode -1)))))

(ert-deftest overblock-pydoc-test-a-fontified-title-carries-its-face ()
  "The face is the rendering: a section title is painted, not marked up."
  (with-temp-buffer
    (let ((shown (overblock-md-fontified
                  "Parameters\n----------\nxs : int\n" #'rst-mode)))
      (should (string-match-p "Parameters" shown))
      (should-not (string-match-p "----" shown))
      (should (get-text-property (string-match-p "Parameters" shown)
                                 'face shown)))))

(ert-deftest overblock-pydoc-test-a-doc-string-wears-its-bars ()
  "Prose of many lines is dressed in a bar above and a rule below.
The bar carries the label and the two buttons; the rule carries
nothing, since saying the same twice said nothing the second time."
  (let* ((dressed (overblock-pydoc--dressed "one\ntwo" 0))
         (lines (split-string dressed "\n")))
    (should (= (length lines) 4))
    (should (string-match-p overblock-pydoc-label (car lines)))
    (should (equal (nth 1 lines) "one"))
    (should (equal (nth 2 lines) "two"))
    ;; the rule has no label and no button of its own: spaces, and
    ;; the zero-width space that keeps the row from being read as a
    ;; blank line and trimmed away
    (should-not (string-match-p overblock-pydoc-label (nth 3 lines)))
    (should (string-match-p "\\`[\u200b[:blank:]]*\\'" (nth 3 lines)))))

(ert-deftest overblock-pydoc-test-one-line-takes-one-row ()
  "Prose of a single line shares its row with the buttons.
A bar above and a rule below would make three rows of one line of
prose, and a doc string of one line is the commonest of all."
  (let ((dressed (overblock-pydoc--dressed "all of it" 0)))
    (should-not (string-search "\n" dressed))
    (should (string-match-p "all of it" dressed))))

(ert-deftest overblock-pydoc-test-a-row-leaves-room-for-the-indent ()
  "A row does not fill the columns its own indentation stands in.
Padded to the width of the window, the buttons of an indented doc
string fell onto a row of their own, exactly as many columns over as
the doc string was deep."
  (let ((narrow (overblock-pydoc--row "a" "b" 'default 20))
        (wide (overblock-pydoc--row "a" "b" 'default 0)))
    ;; both are built for the same window, and the indented one is
    ;; shorter by what it is indented by
    (should (or (null (overblock-window-width))
                (= (- (string-width wide) (string-width narrow)) 20)))))

(ert-deftest overblock-pydoc-test-point-inside-shows-the-source ()
  "The doc string point is in shows its source; leaving renders it again."
  (skip-unless (overblock-md-program))
  (overblock-pydoc-test--with
    (overblock-pydoc-mode 1)
    (unwind-protect
        (let ((count (lambda ()
                       (length (overblock-in (point-min) (point-max)
                                             'pydoc)))))
          (goto-char (point-max))
          (overblock-pydoc-render-buffer)
          (should (= (overblock-pydoc-test--wait 4) 4))
          ;; a click takes one rendering off
          (goto-char (point-min))
          (overblock-live-edit)
          (should (= (funcall count) 3))
          ;; and the next pass puts it back
          (goto-char (point-max))
          (overblock-pydoc-render-buffer)
          (should (= (overblock-pydoc-test--wait 4) 4)))
      (overblock-pydoc-mode -1))))

(ert-deftest overblock-pydoc-test-the-mode-leaves-nothing-behind ()
  "Turning the mode off gives the buffer back as it was."
  (skip-unless (overblock-md-program))
  (overblock-pydoc-test--with
    (let ((before (buffer-string)))
      (overblock-pydoc-mode 1)
      (goto-char (point-max))
      (overblock-pydoc-render-buffer)
      (should (= (overblock-pydoc-test--wait 4) 4))
      (overblock-pydoc-mode -1)
      (should-not (overblock-in (point-min) (point-max) 'pydoc))
      (should-not overblock-live--spec)
      (should (equal (buffer-string) before)))))

(provide 'overblock-pydoc-test)
;;; overblock-pydoc-test.el ends here
