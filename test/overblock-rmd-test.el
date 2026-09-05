;;; overblock-rmd-test.el --- Tests for overblock-rmd  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Marcel Arpogaus

;; Author: Marcel Arpogaus <znepry.necbtnhf@tznvy.pbz>
;; Assisted-by: Claude:claude-opus-5
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

;; What can be proved without an R at the other end: the chunk walk,
;; the names knitr writes in a chunk header, the prose the chunks are
;; not, the R string a chunk travels as, the prompt that comes off the
;; output again, the bars, and the mode's own two ends.
;;
;; What only a real R can prove is `overblock-rmd-live-test'.

;;; Code:

(require 'ert)
(require 'overblock-rmd)

(defconst overblock-rmd-test--document
  "\
Some prose about the data.

```{r setup}
x <- 1:5
x
```

More prose, in a paragraph
of two lines.

```{r, echo=FALSE}
mean(x)
```

```{python}
print(\"not R\")
```

```{r empty}
```
"
  "A small Rmd file: two R chunks, a Python one, and an empty one.")

(defmacro overblock-rmd-test--with-document (text &rest body)
  "Evaluate BODY in a buffer holding TEXT, shown in a window.
A bar is cut to the width of the windows that show a buffer, and a
command that follows a click selects one."
  (declare (indent 1))
  `(with-temp-buffer
     (insert ,text)
     (set-window-buffer nil (current-buffer))
     (goto-char (point-min))
     ,@body))

(defmacro overblock-rmd-test--with-mode (text &rest body)
  "Evaluate BODY in a buffer holding TEXT with `overblock-rmd-mode' on.
The mode is turned off again afterwards: `with-temp-buffer' kills its
buffer without running the body that would take the hooks down."
  (declare (indent 1))
  `(overblock-rmd-test--with-document ,text
     ;; No converter is asked for: the tests here are about the chunks
     ;; and the bars, and rendering the prose would want pandoc and a
     ;; process apiece.
     (let ((overblock-md-command nil))
       (unwind-protect
           (progn (overblock-rmd-mode 1) ,@body)
         (overblock-rmd-mode -1)))))

(defun overblock-rmd-test--chunk-lines ()
  "Return (LINE NAME) for every R chunk of the buffer, in order."
  (mapcar (lambda (chunk)
            (list (line-number-at-pos (nth 0 chunk))
                  (save-excursion
                    (goto-char (nth 0 chunk))
                    (overblock-rmd--chunk-name (pos-bol) (pos-eol)))))
          (overblock-rmd-chunks)))

(defun overblock-rmd-test--bar-labels ()
  "Return the whole text of every chunk bar of the buffer, in order."
  (mapcar (lambda (ov)
            (substring-no-properties (or (overlay-get ov 'overblock-bar-text)
                                         "")))
          (sort (seq-filter (lambda (ov)
                              (eq (overlay-get ov 'overblock-bar) 'chunk))
                            (overblock-bars))
                (lambda (a b) (< (overlay-start a) (overlay-start b))))))


;;;; The chunk walk

(ert-deftest overblock-rmd-test-the-walk-takes-the-r-chunks ()
  "Only the R chunks, and only the ones holding code."
  (overblock-rmd-test--with-document overblock-rmd-test--document
    (should (equal (overblock-rmd-test--chunk-lines)
                   '((3 "setup") (11 nil))))))

(ert-deftest overblock-rmd-test-the-code-of-a-chunk-is-between-its-fences ()
  "A chunk's region is the code lines whole, the fences left out.
The last newline of the code is in it, because that is what a result
block hangs on: `overblock-show' shows a body on the newline that ends
its region."
  (overblock-rmd-test--with-document overblock-rmd-test--document
    (pcase-let ((`(,_open ,beg ,end) (car (overblock-rmd-chunks))))
      (should (equal (buffer-substring-no-properties beg end) "x <- 1:5\nx\n"))
      ;; the closing fence begins where the region ends
      (should (equal (buffer-substring-no-properties end (+ end 3)) "```")))))

(ert-deftest overblock-rmd-test-a-chunk-of-another-engine-is-left-alone ()
  "A ```{python} chunk is not R, and neither is ```{rmarkdown}.
The engine name has to end where knitr ends it: at a blank, a comma or
the closing brace."
  (dolist (case '(("```{python}\n1\n```\n" . 0)
                  ("```{rmarkdown}\n1\n```\n" . 0)
                  ("```{sql, connection=db}\n1\n```\n" . 0)
                  ("```{r}\n1\n```\n" . 1)
                  ("```{R}\n1\n```\n" . 1)
                  ("```{r }\n1\n```\n" . 1)
                  ("```{r, echo=FALSE}\n1\n```\n" . 1)
                  ("```{r name}\n1\n```\n" . 1)))
    (overblock-rmd-test--with-document (car case)
      (should (= (length (overblock-rmd-chunks)) (cdr case))))))

(ert-deftest overblock-rmd-test-a-chunk-name-is-the-word-knitr-reads ()
  "The word after the engine, where a comma or a brace ends it.
An option written where a name would stand names nothing: knitr reads
`echo=FALSE' as an option, and a bar that called the chunk `echo=FALSE'
said the wrong thing in the one place a reader looks."
  (dolist (case '(("```{r setup}" . "setup")
                  ("```{r plot-one, fig.width=4}" . "plot-one")
                  ("```{r  spaced  , echo=TRUE}" . "spaced")
                  ("```{r}" . nil)
                  ("```{r, echo=FALSE}" . nil)
                  ("```{r echo=FALSE}" . nil)))
    (overblock-rmd-test--with-document (concat (car case) "\n1\n```\n")
      (should (equal (overblock-rmd--chunk-name (point-min) (pos-eol))
                     (cdr case))))))

(ert-deftest overblock-rmd-test-the-chunk-at-point-covers-both-fences ()
  "A point on either fence line finds the chunk, and so does one inside.
The bar sits on the opening fence, so a command that follows a click
has to find the chunk from there."
  (overblock-rmd-test--with-document "prose\n\n```{r a}\n1\n2\n```\n\nmore\n"
    (let ((chunk (car (overblock-rmd-chunks))))
      (dolist (line '(3 4 5 6))
        (goto-char (point-min))
        (forward-line (1- line))
        (should (equal (overblock-rmd--chunk-at) chunk)))
      ;; and the prose around it does not
      (dolist (line '(1 2 8))
        (goto-char (point-min))
        (forward-line (1- line))
        (should-not (overblock-rmd--chunk-at))))))

(ert-deftest overblock-rmd-test-the-prose-is-what-the-chunks-are-not ()
  "The regions to render are the paragraphs; no chunk line is among them."
  (overblock-rmd-test--with-document overblock-rmd-test--document
    (let ((prose (overblock-rmd--prose (point-min) (point-max)))
          (chunks (overblock-rmd-chunks)))
      (should (= (length prose) 2))
      (should (equal (buffer-substring-no-properties (car (car prose))
                                                     (cdr (car prose)))
                     "Some prose about the data."))
      (dolist (region prose)
        (dolist (chunk chunks)
          ;; no prose region overlaps a chunk, fences and all
          (should (or (<= (cdr region) (nth 0 chunk))
                      (>= (car region) (nth 2 chunk)))))))))


;;;; What travels to R, and what comes back

(ert-deftest overblock-rmd-test-a-chunk-travels-as-one-r-string ()
  "The quotes, the backslashes and the newlines of a chunk are escaped.
A chunk goes inside an R string literal, so a quote of its own would
end that literal and a newline would end the line comint sends."
  (should (equal (overblock-rmd--r-string "x") "\"x\""))
  (should (equal (overblock-rmd--r-string "a\nb") "\"a\\nb\""))
  (should (equal (overblock-rmd--r-string "say \"hi\"") "\"say \\\"hi\\\"\""))
  (should (equal (overblock-rmd--r-string "back\\slash")
                 "\"back\\\\slash\""))
  ;; the whole of it on one line, whatever the chunk held
  (should-not (string-search "\n" (overblock-rmd--r-string
                                   "cat('a\\nb')\nx <- \"q\"\n"))))

(ert-deftest overblock-rmd-test-the-prompt-comes-off-the-result ()
  "The prompt R writes when the chunk is done goes, and nothing else."
  (let ((inferior-ess-primary-prompt "> "))
    (should (equal (overblock-rmd--clean "[1] 32\n> ") "[1] 32"))
    (should (equal (overblock-rmd--clean "a\nb\n\n> ") "a\nb"))
    ;; a chunk that printed nothing: the prompt alone
    (should (equal (overblock-rmd--clean "> ") ""))
    (should (equal (overblock-rmd--clean "\n> ") ""))
    ;; nothing to take off
    (should (equal (overblock-rmd--clean "a\nb") "a\nb"))))

(ert-deftest overblock-rmd-test-a-table-keeps-the-indent-of-its-header ()
  "The leading spaces of the first line of output are content.
R prints its tables with the header indented and the numbers lined up
under it: taking those spaces off left the header three characters to
the left of every row."
  (let ((inferior-ess-primary-prompt "> "))
    (should (equal (overblock-rmd--clean "   Min. Max. \n  10.4 33.9 \n> ")
                   "   Min. Max. \n  10.4 33.9"))))

(ert-deftest overblock-rmd-test-an-error-stops-a-pass ()
  "What R writes when a chunk fails, in each of its shapes."
  (should (overblock-rmd--error-p "Error in log(\"a\") : non-numeric"))
  (should (overblock-rmd--error-p "Error: object not found"))
  (should (overblock-rmd--error-p "[1] 1\nError in f() : boom"))
  (should-not (overblock-rmd--error-p "[1] 1 2 3"))
  (should-not (overblock-rmd--error-p "Warning message:\nIn log(-1) : NaNs"))
  ;; not a word that merely starts with those letters
  (should-not (overblock-rmd--error-p "Errors were counted: 3")))


(ert-deftest overblock-rmd-test-one-glyph-means-one-thing ()
  "No two buttons draw the same glyph, in any row of candidates.
A frame draws whichever row it can: the nerd glyphs, the symbols an
ordinary font has, or the plain words a terminal falls to.  A glyph
stands for one command whichever row it comes from, because a frame
draws no row whole — `overblock-glyph' answers for one button at a
time, so a font with some of the symbols draws those and the rest fall
to the words beside them."
  (let ((bars (list overblock-rmd-result-buttons overblock-rmd-chunk-buttons))
        seen)
    (dolist (buttons bars)
      (let ((glyphs (mapcan (lambda (button) (copy-sequence (nth 1 button)))
                            buttons)))
        (should (equal glyphs (delete-dups (copy-sequence glyphs))))))
    (dolist (buttons bars)
      (dolist (button buttons)
        (dolist (glyph (nth 1 button))
          (let ((before (assoc glyph seen)))
            (when before (should (eq (cdr before) (nth 3 button))))
            (push (cons glyph (nth 3 button)) seen)))))))

(ert-deftest overblock-rmd-test-every-button-carries-three-candidates ()
  "A nerd glyph, a plain symbol and a word, and none of them empty.
The private use characters of a nerd font are easy to lose in an editor
that does not draw them: a row of empty strings drew a bar with a label
and nothing else, and every button on it was invisible."
  (dolist (buttons (list overblock-rmd-result-buttons
                         overblock-rmd-chunk-buttons))
    (dolist (button buttons)
      (let ((glyphs (nth 1 button)))
        (should (= (length glyphs) 3))
        (dolist (glyph glyphs)
          (should (stringp glyph))
          (should-not (string-empty-p glyph)))
        ;; the first candidate is a nerd font private use character
        (should (<= #xE000 (aref (car glyphs) 0) #xF8FF))))))

;;;; The bars

(ert-deftest overblock-rmd-test-every-chunk-header-gets-a-bar ()
  "One bar a chunk, on its opening fence, naming it and offering to run it."
  (overblock-rmd-test--with-mode overblock-rmd-test--document
    (let ((labels (overblock-rmd-test--bar-labels)))
      (should (= (length labels) 2))
      (should (string-match-p "setup" (car labels)))
      ;; a chunk with no name of its own is called what it is
      (should (string-match-p "chunk" (cadr labels))))))

(ert-deftest overblock-rmd-test-a-bar-goes-with-the-header-that-had-it ()
  "A header line that stops being one loses its bar.
The bars are drawn from the idle cycle, so the pass that draws them is
also the pass that has to take the stale ones down."
  (overblock-rmd-test--with-mode "```{r a}\n1\n```\n"
    (should (= (length (overblock-rmd-test--bar-labels)) 1))
    (goto-char (point-min))
    ;; no longer an R chunk
    (delete-region (point-min) (pos-eol))
    (insert "```{python}")
    (overblock-rmd--bars)
    (should-not (overblock-rmd-test--bar-labels))))

(ert-deftest overblock-rmd-test-the-bar-of-a-chunk-is-drawn-once ()
  "A second pass over the buffer reuses the bar rather than adding one."
  (overblock-rmd-test--with-mode overblock-rmd-test--document
    (let ((bars (overblock-rmd-test--bar-labels)))
      (overblock-rmd--bars)
      (overblock-rmd--bars)
      (should (equal (overblock-rmd-test--bar-labels) bars)))))


;;;; The result block

(ert-deftest overblock-rmd-test-a-result-hangs-under-the-code ()
  "The result of a chunk shows after its code and before the closing fence."
  (overblock-rmd-test--with-mode "```{r a}\n1\n```\n"
    (pcase-let ((`(,_open ,beg ,end) (car (overblock-rmd-chunks))))
      (should (overblock-run-show beg end "[1] 1" 0.4))
      (let ((block (car (overblock-in (point-min) (point-max) 'result))))
        (should block)
        ;; the header says what it holds, and the body shows it
        (should (string-match-p "1 line" (overblock-get block :header)))
        (should (equal (substring-no-properties (overblock-get block :body))
                       "[1] 1"))
        ;; and it hangs inside the chunk, above the closing fence
        (should (< (overlay-end block) end))))))

(ert-deftest overblock-rmd-test-a-result-folds ()
  "The fold button hides the body and leaves the header."
  (overblock-rmd-test--with-mode "```{r a}\n1\n```\n"
    (pcase-let ((`(,_open ,beg ,end) (car (overblock-rmd-chunks))))
      (overblock-run-show beg end "one\ntwo" 0.1)
      (goto-char beg)
      (overblock-run-toggle-output)
      (let ((block (car (overblock-in (point-min) (point-max) 'result))))
        (should-not (overblock-get block :body))
        (should (overblock-get block :header)))
      (overblock-run-toggle-output)
      (should (overblock-get (car (overblock-in (point-min) (point-max)
                                                'result))
                             :body)))))

(ert-deftest overblock-rmd-test-a-result-is-discarded-and-copied ()
  "The two buttons that take a result away and put it on the kill ring."
  (overblock-rmd-test--with-mode "```{r a}\n1\n```\n"
    (pcase-let ((`(,_open ,beg ,end) (car (overblock-rmd-chunks))))
      (overblock-run-show beg end "[1] 1" 0.1)
      (goto-char beg)
      (let ((kill-ring nil))
        (overblock-run-copy-output)
        (should (equal (substring-no-properties (current-kill 0)) "[1] 1")))
      (overblock-run-discard-output)
      (should-not (overblock-in (point-min) (point-max) 'result)))))

(ert-deftest overblock-rmd-test-there-is-no-result-here ()
  "A command that wants a result says so where there is none."
  (overblock-rmd-test--with-mode "prose\n\n```{r a}\n1\n```\n"
    (goto-char (point-min))
    (should-error (overblock-run-toggle-output) :type 'user-error)))

(ert-deftest overblock-rmd-test-an-edit-of-the-code-drops-the-result ()
  "A result stands for the code it was run from; editing that takes it down."
  (overblock-rmd-test--with-mode "```{r a}\n1\n```\n"
    (pcase-let ((`(,_open ,beg ,end) (car (overblock-rmd-chunks))))
      (overblock-run-show beg end "[1] 1" 0.1)
      (should (overblock-in (point-min) (point-max) 'result))
      (goto-char beg)
      (insert "2 + ")
      (should-not (overblock-in (point-min) (point-max) 'result)))))


;;;; The commands and the mode

(ert-deftest overblock-rmd-test-a-command-wants-a-chunk ()
  "The commands that act on a chunk say so where point is in prose."
  (overblock-rmd-test--with-mode "prose\n\n```{r a}\n1\n```\n"
    (goto-char (point-min))
    (should-error (overblock-rmd-run-chunk) :type 'user-error)
    (should-error (overblock-run-above) :type 'user-error)))

(ert-deftest overblock-rmd-test-the-first-chunk-has-none-above-it ()
  "`overblock-run-above' on the first chunk refuses rather than runs."
  (overblock-rmd-test--with-mode "```{r a}\n1\n```\n\n```{r b}\n2\n```\n"
    (goto-char (point-min))
    (should-error (overblock-run-above) :type 'user-error)))

(ert-deftest overblock-rmd-test-the-step-of-a-vanished-chunk-walks-on ()
  "A queued marker whose chunk the reader has deleted stops nothing.
`overblock-run-next' takes a non-nil answer as \"wait for a prompt\",
so a step that ran nothing has to answer nil or the pass would stand
there forever."
  (overblock-rmd-test--with-mode "prose only, no chunk\n"
    (goto-char (point-min))
    (should-not (overblock-rmd--step))))

(ert-deftest overblock-rmd-test-the-mode-tells-the-runner-what-r-is ()
  "The mode gives the buffer a backend, and takes it away again.
The runner reads it to know the buffer is one it may run and draw in,
and a command called with the mode off must say so rather than do
nothing in silence."
  (overblock-rmd-test--with-document "```{r a}\n1\n```\n"
    (should-not overblock-run-backend)
    (should-error (overblock-rmd-run-chunk) :type 'user-error)
    (let ((overblock-md-command nil))
      (overblock-rmd-mode 1)
      (should (equal (plist-get overblock-run-backend :name) "overblock-rmd"))
      ;; and what ESS asks of a buffer it starts a process for
      (should (equal ess-dialect "R"))
      (overblock-rmd-mode -1))
    (should-not overblock-run-backend)))

(ert-deftest overblock-rmd-test-the-mode-off-leaves-nothing-behind ()
  "Turning the mode off takes the bars and the blocks with it."
  (overblock-rmd-test--with-document "```{r a}\n1\n```\n"
    (let ((overblock-md-command nil))
      (overblock-rmd-mode 1)
      (pcase-let ((`(,_open ,beg ,end) (car (overblock-rmd-chunks))))
        (overblock-run-show beg end "[1] 1" 0.1))
      (should (overblock-bars))
      (should (overblock-in (point-min) (point-max) 'result))
      (overblock-rmd-mode -1)
      (should-not (overblock-bars))
      (should-not (overblock-in (point-min) (point-max) 'result)))))

(ert-deftest overblock-rmd-test-the-mode-follows-the-file-name ()
  "`overblock-rmd-mode-maybe' turns the mode on for an Rmd file only."
  (dolist (case '(("/tmp/notes.Rmd" . t) ("/tmp/notes.rmd" . t)
                  ("/tmp/notes.md" . nil) ("/tmp/notes.R" . nil)
                  (nil . nil)))
    (overblock-rmd-test--with-document "```{r a}\n1\n```\n"
      (setq buffer-file-name (car case))
      (let ((overblock-md-command nil))
        (unwind-protect
            (progn (overblock-rmd-mode-maybe)
                   (should (eq (and overblock-rmd-mode t) (cdr case))))
          (when overblock-rmd-mode (overblock-rmd-mode -1))
          (setq buffer-file-name nil))))))

(provide 'overblock-rmd-test)
;;; overblock-rmd-test.el ends here
