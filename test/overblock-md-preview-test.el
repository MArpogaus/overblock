;;; overblock-md-preview-test.el --- Tests for the markdown preview  -*- lexical-binding: t; -*-

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

;; Run with: make test
;;
;; The tests that need a converter skip themselves where none is
;; installed; `make test STRICT=1' refuses to skip.

;;; Code:

(require 'ert)
(require 'overblock-md-preview)
(require 'overblock-test-common)

(defmacro overblock-md-preview-test--with (text &rest body)
  "Evaluate BODY in a buffer holding TEXT with the mode on."
  (declare (indent 1))
  `(with-temp-buffer
     (insert ,text)
     (markdown-mode)
     (goto-char (point-min))
     (overblock-md-preview-mode 1)
     (unwind-protect (progn ,@body)
       (overblock-md-preview-mode -1))))

(defun overblock-md-preview-test--wait (count)
  "Wait until COUNT blocks carry a rendering, and return how many do.
The conversion runs in a process that nothing waits for, so a test
must wait."
  (overblock-test-common-wait
   (lambda () (>= (length (overblock-md-preview-test--blocks)) count)) 10)
  (length (overblock-md-preview-test--blocks)))

(defun overblock-md-preview-test--blocks ()
  "Return the preview blocks of this buffer."
  (overblock-in (point-min) (point-max) 'md-preview))

(defun overblock-md-preview-test--sources ()
  "Return the source line of every rendered line, in order."
  (mapcar (lambda (block)
            (string-trim (buffer-substring-no-properties
                          (overlay-start block) (overlay-end block))))
          (overblock-md-preview-test--blocks)))

(defun overblock-md-preview-test--texts (beg end)
  "Return the text of every markdown block between BEG and END."
  (mapcar (lambda (region)
            (buffer-substring-no-properties (car region) (cdr region)))
          (overblock-md-preview-regions beg end)))

(ert-deftest overblock-md-preview-test-a-block-is-what-markdown-calls-one ()
  "A block is the run of lines between two blank ones, or a whole fence.
A row of a table needs the rows around it, and a fenced block keeps
its blank lines."
  (with-temp-buffer
    (insert "# Heading\n\npara one\ncontinues\n\n| a | b |\n|---|---|\n"
            "| 1 | 2 |\n\n```\ncode\n\nwith a blank\n```\n\n- one\n- two\n")
    (should (equal (overblock-md-preview-test--texts (point-min) (point-max))
                   '("# Heading"
                     "para one\ncontinues"
                     "| a | b |\n|---|---|\n| 1 | 2 |"
                     "```\ncode\n\nwith a blank\n```"
                     "- one\n- two")))))

(ert-deftest overblock-md-preview-test-a-fence-closes-on-its-own-kind ()
  "Only a fence of the same kind closes a block, and it may be longer.
Three backquotes inside a ~~~ block stay one block."
  (with-temp-buffer
    (insert "~~~\n```\ncode\n```\n~~~\n\nafter\n")
    (should (equal (overblock-md-preview-test--texts (point-min) (point-max))
                   '("~~~\n```\ncode\n```\n~~~" "after"))))
  (with-temp-buffer
    (insert "```\ncode\n`````\n\nafter\n")
    (should (equal (overblock-md-preview-test--texts (point-min) (point-max))
                   '("```\ncode\n`````" "after")))))

(ert-deftest overblock-md-preview-test-a-table-renders-as-a-table ()
  "A table reaches the converter whole, and comes back with its columns.
A row alone renders as a paragraph, and the rule as empty cells."
  (skip-unless (overblock-md-program))
  (with-temp-buffer
    (insert "| a | b |\n|---|---|\n| 1 | 2 |\n")
    (let* ((region (car (overblock-md-preview-regions (point-min)
                                                       (point-max))))
           (block (overblock-md-preview--show (car region) (cdr region)))
           (shown (substring-no-properties (overblock-get block :over))))
      ;; The rule is gone and the cells are in their columns.
      (should-not (string-match-p "---" shown))
      (should (string-match-p "a +b" shown))
      (should (string-match-p "1 +2" shown)))))

(ert-deftest overblock-md-preview-test-indented-code-renders-as-code ()
  "A block converted by itself keeps the indent that makes it code."
  (skip-unless (overblock-md-program))
  (with-temp-buffer
    (insert "Para.\n\n    def f():\n        return 1\n")
    (let* ((region (cadr (overblock-md-preview-regions (point-min)
                                                        (point-max))))
           (block (overblock-md-preview--show (car region) (cdr region)))
           (rows (split-string (substring-no-properties
                                (overblock-get block :over))
                               "\n")))
      (should (= 2 (length rows))))))

(ert-deftest overblock-md-preview-test-a-rendering-fits-the-window ()
  "The rendering is filled to the columns the window has.
The window is made narrower than the frame, so the test fails if the
width does not reach shr.  One column is kept back, because a row that
fills the last one wraps."
  (skip-unless (overblock-md-program))
  (overblock-md-preview-test--with
      (concat "A paragraph long enough to need filling, of ordinary "
              "words and no markup at all, so that what comes back is "
              "as wide as the filling made it.\n")
    (set-window-buffer nil (current-buffer))
    (cl-letf (((symbol-function 'window-max-chars-per-line) (lambda (&rest _) 30)))
      (let ((block (overblock-md-preview--show (point-min) (point-max))))
        (should block)
        (dolist (row (split-string (overblock-get block :over) "\n"))
          (should (<= (string-width row) 29)))
        ;; The filling did it, not a short answer.
        (should (seq-find (lambda (row) (> (string-width row) 20))
                          (split-string (overblock-get block :over) "\n")))))))

(ert-deftest overblock-md-preview-test-a-region-is-read-from-the-top ()
  "A region inside a fence is known to be inside it.
The walk starts at the top of the buffer whatever the region says,
because nothing else can tell whether the region opened in a fence."
  (with-temp-buffer
    (insert "```\none\ntwo\n```\n\nthree\n")
    (let ((inside (progn (goto-char (point-min)) (forward-line 2) (point))))
      ;; The fence starts before the region, so only the paragraph
      ;; after it is left.
      (should (equal (overblock-md-preview-test--texts inside (point-max))
                     '("three"))))))

(ert-deftest overblock-md-preview-test-every-line-is-rendered ()
  "Each line of markdown carries its own rendering."
  (skip-unless (overblock-md-program))
  (overblock-md-preview-test--with "# A heading\n\nsome *emphasis*\n"
    (goto-char (point-max))
    (overblock-md-preview-render-buffer)
    (should (equal (overblock-md-preview-test--wait 2) 2))
    (should (equal (overblock-md-preview-test--sources)
                   '("# A heading" "some *emphasis*")))
    ;; The markup is gone from what the reader sees.
    (let ((shown (overblock-get (car (overblock-md-preview-test--blocks))
                                :over)))
      (should (equal (string-trim (substring-no-properties shown))
                     "A heading")))))

(ert-deftest overblock-md-preview-test-an-edited-block-shows-its-source ()
  "A block taken down with `overblock-live-edit' shows its source.
With point elsewhere, the next pass renders it again."
  (skip-unless (overblock-md-program))
  (overblock-md-preview-test--with "# One\n\ntwo\n\nthree\n"
    (goto-char (point-max))
    (overblock-md-preview-render-buffer)
    (should (equal (overblock-md-preview-test--wait 3) 3))
    (should (equal (overblock-md-preview-test--sources)
                   '("# One" "two" "three")))
    (goto-char (point-min))
    (overblock-live-edit)
    (should (equal (overblock-md-preview-test--sources) '("two" "three")))
    (goto-char (point-max))
    (overblock-md-preview-render-buffer)
    (should (equal (overblock-md-preview-test--wait 3) 3))
    (should (equal (overblock-md-preview-test--sources)
                   '("# One" "two" "three")))))

(ert-deftest overblock-md-preview-test-an-edit-drops-the-rendering ()
  "An edit of a rendered line takes its rendering down.
The edit here is a replacement over the buffer."
  (skip-unless (overblock-md-program))
  (overblock-md-preview-test--with "# One\n\ntwo\n"
    (goto-char (point-max))
    (overblock-md-preview-render-buffer)
    (should (equal (overblock-md-preview-test--wait 2) 2))
    (should (equal (overblock-md-preview-test--sources) '("# One" "two")))
    (goto-char (point-min))
    (while (search-forward "One" nil t) (replace-match "Three"))
    (should (equal (overblock-md-preview-test--sources) '("two")))))

(ert-deftest overblock-md-preview-test-a-fence-in-a-paragraph-renders-once ()
  "A fence with no blank line around it interrupts the paragraph.
The prose before it, the fence and the prose after it are three blocks,
and no two of them cover the same line."
  (skip-unless (overblock-md-program))
  (overblock-md-preview-test--with
      "before the fence\n```\ncode\n```\nafter it\n\nlast\n"
    (goto-char (point-max))
    (overblock-md-preview-render-buffer)
    (should (equal (overblock-md-preview-test--wait 4) 4))
    ;; Not `:key': the keyword form of `sort' is Emacs 30, and this
    ;; package supports 29.1.
    (let ((blocks (sort (overblock-md-preview-test--blocks)
                        (lambda (a b)
                          (< (overlay-start a) (overlay-start b))))))
      (should (= (length blocks) 4))
      (while (cdr blocks)
        (should (< (overlay-end (car blocks)) (overlay-start (cadr blocks))))
        (pop blocks)))))

(ert-deftest overblock-md-preview-test-the-answer-lands-nowhere-near-point ()
  "A block the reader walked into is left alone when its HTML lands.
The answer arrives later, when point can be in another block, which
then stays source."
  (skip-unless (overblock-md-program))
  (overblock-md-preview-test--with "# One\n\ntwo\n\nthree\n"
    (goto-char (point-max))
    (overblock-md-preview-render-buffer)
    ;; Point moves into the first block while the converter runs.
    (goto-char (point-min))
    (should (equal (overblock-md-preview-test--wait 2) 2))
    (should (equal (overblock-md-preview-test--sources) '("two" "three")))))

(ert-deftest overblock-md-preview-test-the-mode-leaves-nothing-behind ()
  "Turning the mode off gives the buffer back as it was."
  (skip-unless (overblock-md-program))
  (with-temp-buffer
    (insert "# One\n\ntwo\n")
    (markdown-mode)
    (let ((before (buffer-string)))
      (overblock-md-preview-mode 1)
      (goto-char (point-max))
      (overblock-md-preview-render-buffer)
      (should (equal (overblock-md-preview-test--wait 2) 2))
      (overblock-md-preview-mode -1)
      (should-not (overblock-md-preview-test--blocks))
      ;; The live cycle stops too.
      (should-not overblock-live--timer)
      (should-not overblock-live--specs)
      (should (equal (buffer-string) before)))))

(ert-deftest overblock-md-preview-test-a-fence-ends-the-paragraph-it-touches ()
  "Prose right above a fence is a paragraph of its own, without the fence."
  (with-temp-buffer
    (insert "Some prose:\n```r\nx <- 1\n```\nMore prose.\n")
    (let ((fences (overblock-md-preview-fences (point-max))))
      (should (equal (overblock-md-preview-paragraphs (point-max) fences)
                     '((1 . 12) (29 . 40)))))))

(ert-deftest overblock-md-preview-test-an-indented-fence-stays-in-its-item ()
  "A fence inside a list item does not cut the item in two.
The item is one block, and the fence is not a second one over it."
  (with-temp-buffer
    (insert "- item *one*\n  ```python\n  x = 1\n  ```\n  tail of **item**\n- item two\n")
    (should (equal (overblock-md-preview-regions (point-min) (point-max))
                   '((1 . 69))))))

(ert-deftest overblock-md-preview-test-an-indented-fence-outside-a-list-ends-a-paragraph ()
  "A fence a few spaces in, with no list around it, still ends the paragraph."
  (with-temp-buffer
    (insert "Intro text\n  ```\n  code\n  ```\nAfter text\n\nNext para\n")
    (let ((fences (overblock-md-preview-fences (point-max))))
      (should (equal (overblock-md-preview-paragraphs (point-max) fences)
                     '((1 . 11) (31 . 41) (43 . 52)))))))

(ert-deftest overblock-md-preview-test-a-fence-after-an-item-s-second-paragraph-stays-in-it ()
  "A fence under the second paragraph of a loose item belongs to the item."
  (with-temp-buffer
    (insert "- Item one\n\n  Second para\n  ```python\n  x = 1\n  ```\n  tail\n- Item two\n")
    (let ((fences (overblock-md-preview-fences (point-max))))
      (should-not (seq-some (lambda (p) (string-prefix-p "  tail\n- Item"
                                                         (buffer-substring (car p) (min (point-max) (+ (car p) 14)))))
                            (overblock-md-preview-paragraphs (point-max) fences))))))

(ert-deftest overblock-md-preview-test-a-chunk-under-an-item-is-no-prose ()
  "With PROSE-ONLY each fence ends a paragraph: in an Rmd file it is a chunk."
  (with-temp-buffer
    (insert "1. Load the data:\n   ```{r}\n   x <- 1\n   ```\n   then look.\n")
    (let ((chunk (car (overblock-md-preview-fences (point-max)))))
      (should-not (seq-some (lambda (p) (and (<= (car p) (car chunk))
                                             (>= (cdr p) (cdr chunk))))
                            (overblock-md-preview-regions (point-min) (point-max) t))))))

(ert-deftest overblock-md-preview-test-inline-code-opens-no-fence ()
  "A line that begins with triple-backtick inline code is prose."
  (with-temp-buffer
    (insert "```x``` leads this line.\n\nLast para.\n")
    (should-not (overblock-md-preview-fences (point-max)))))

(ert-deftest overblock-md-preview-test-a-block-goes-out-closed ()
  "A block goes to the converter with its closing fence under its opening one."
  (should (equal (overblock-md-preview--closed "```\n\n## S") "```\n\n## S\n```"))
  (should (equal (overblock-md-preview--closed "~~~~ r\nx\n~~~~") "~~~~ r\nx\n~~~~"))
  (should (equal (overblock-md-preview--closed "```x``` text") "```x``` text"))
  (should (equal (overblock-md-preview--closed "Para.") "Para."))
  (should (equal (overblock-md-preview--closed "<!-- a") "<!-- a\n-->"))
  (should (equal (overblock-md-preview--closed "b -->") "<!--\nb -->"))
  (should (equal (overblock-md-preview--closed "a --> b") "a --> b"))
  ;; An indented code block opens no fence; an item line does.
  (should (equal (overblock-md-preview--closed "    ```\n    x") "    ```\n    x"))
  (should (equal (overblock-md-preview--closed "- ```sh\n  x") "- ```sh\n  x\n  ```"))
  (should (equal (overblock-md-preview--closed "- ```sh\n  x\n```\t")
                 "- ```sh\n  x\n  ```")))

(ert-deftest overblock-md-preview-test-a-failed-conversion-is-a-block ()
  "A region the converter fails on gets an empty block, and goes no more."
  (with-temp-buffer
    (insert "---\ntitle: x: y\n---\n")
    (setq-local overblock-live--specs (list (list 'md-preview #'ignore)))
    (cl-letf (((symbol-function 'overblock-md-rendered) #'ignore))
      (should (overblock-md-preview--show 1 (1- (point-max)))))
    (should-not (overblock-live-wanted-p 1 (1- (point-max)) 'md-preview))))

(ert-deftest overblock-md-preview-test-a-later-paragraph-of-an-item-is-dedented ()
  "The fence under a later paragraph of an item keeps its place in it."
  (with-temp-buffer
    (insert "- one\n\n  more of one\n  ```python\n  y = 2\n  ```\n")
    (should (equal (overblock-md-preview--source 8 (point-max))
                   "more of one\n```python\ny = 2\n```\n")))
  (with-temp-buffer
    (insert "Para.\n\n    def f():\n        return 1\n")
    (should (equal (overblock-md-preview--source 8 (point-max))
                   "    def f():\n        return 1\n")))
  (with-temp-buffer
    (insert "```\n        eight\n```\n")
    (should (equal (overblock-md-preview--source (point-min) (1- (point-max)))
                   "```\n        eight\n```"))))

(ert-deftest overblock-md-preview-test-an-item-after-a-chunk-is-its-own-block ()
  "In an Rmd file the rest of an item after its chunk stops at the next item."
  (with-temp-buffer
    (insert "- two\n  ```{r}\n  1\n  ```\n  Tail.\n- three\n")
    (should (equal (mapcar (lambda (r) (buffer-substring (car r) (cdr r)))
                           (overblock-md-preview-regions (point-min) (point-max) t))
                   '("- two" "  Tail." "- three"))))
  (with-temp-buffer
    (insert "- two\n\n  ```{r}\n  1\n  ```\n\n  Tail.\n- three\n")
    (should (equal (mapcar (lambda (r) (buffer-substring (car r) (cdr r)))
                           (overblock-md-preview-regions (point-min) (point-max) t))
                   '("- two" "  Tail." "- three"))))
  (with-temp-buffer
    (insert "- a\n\n  - b\n  - c\n")
    (should (= 2 (length (overblock-md-preview-regions
                          (point-min) (point-max) t)))))
  ;; An indented paragraph outside a list is no item's.
  (with-temp-buffer
    (insert "Intro:\n\n  indented para\n- item\n")
    (should (= 2 (length (overblock-md-preview-regions (point-min) (point-max))))))
  ;; Without a chunk too: the later paragraph of an item.
  (with-temp-buffer
    (insert "- a\n\n  para of a\n- b\n")
    (should (equal (mapcar (lambda (r) (buffer-substring (car r) (cdr r)))
                           (overblock-md-preview-regions (point-min) (point-max)))
                   '("- a" "  para of a" "- b")))))

(ert-deftest overblock-md-preview-test-deep-fence-under-an-item ()
  "Four spaces in, a fence opens a block only under a list item."
  (with-temp-buffer
    (insert "- Data:\n  - Load it:\n    ```{r}\n    x <- 1\n    ```\n")
    (should (equal (overblock-md-preview-fences (point-max))
                   (list (cons 22 (1- (point-max)))))))
  (with-temp-buffer
    (insert "Para.\n\n    ```\n    code\n")
    (should-not (overblock-md-preview-fences (point-max))))
  ;; A rendering hides the indentation of the opening fence.
  (with-temp-buffer
    (insert "- a\n  - b\n    ```\n    x\n    ```\n\nEnd.\n")
    (let ((ov (make-overlay 11 15)))
      (overlay-put ov 'invisible t)
      (should (equal (overblock-md-preview-fences (point-max))
                     '((11 . 32))))))
  ;; At the margin a closing fence is at most three columns in.
  (with-temp-buffer
    (insert "   ```\n   x\n      ```\n   y\n   ```\n")
    (should (= 1 (length (overblock-md-preview-fences (point-max))))))
  ;; Under an item a closing fence may be three columns deeper.
  (with-temp-buffer
    (insert "- a\n\n  ```\n  x\n     ```\n\nEnd.\n")
    (should (equal (overblock-md-preview-fences (point-max))
                   '((6 . 24)))))
  ;; A fence left of the item's text is no part of the item.
  (with-temp-buffer
    (insert "1. a\n\n  ```\n  x\n     ```\n  y\n  ```\n\nEnd.\n")
    (should (equal (overblock-md-preview-fences (point-max))
                   (list (cons 7 (- (point-max) 7))))))
  ;; A fence on the line of an item opens a block in it.
  (with-temp-buffer
    (insert "1. ```bash\n   pip\n   ```\n2. Run.\n\n```\nx\n```\n")
    (should (equal (overblock-md-preview-fences (point-max))
                   '((1 . 25) (35 . 44)))))
  ;; Four spaces in outside a list, an item line is code text.
  (with-temp-buffer
    (insert "Para.\n\n    - ```bash\n    x\n\n```\ny\n```\n")
    (should (equal (overblock-md-preview-fences (point-max))
                   (list (cons 29 (1- (point-max)))))))
  ;; A fence at the margin closes the block of an item line.
  (with-temp-buffer
    (insert "1. ```bash\n   pip\n```\n2. Run.\n")
    (should (equal (overblock-md-preview-fences (point-max))
                   '((1 . 22)))))
  ;; An HTML comment is a block of its own, and holds no fence.
  (with-temp-buffer
    (insert "<!-- a\n\n```\nb -->\n\n```\nx\n```\n")
    (should (equal (overblock-md-preview-fences (point-max) t)
                   '((1 . 18) (20 . 29))))
    ;; Without COMMENTS, as for the chunks of an Rmd file, a fence in
    ;; a comment counts.
    (should (= 2 (length (overblock-md-preview-fences (point-max))))))
  ;; Front matter is one region, whatever blank lines stand in it.
  (with-temp-buffer
    (insert "---\ntitle: x\n\ntags: [a]\n---\n\nText.\n")
    (should (equal (overblock-md-preview-fences (point-max)) '((1 . 28))))
    (should (equal (overblock-md-preview-regions (point-min) (point-max) t)
                   '((30 . 35)))))
  ;; A rule at the top, with a blank line after it, is no front matter.
  (with-temp-buffer
    (insert "---\n\nIntro.\n\n```\na\n---\n```\n")
    (should (equal (overblock-md-preview-fences (point-max)) '((14 . 27)))))
  ;; After a heading a comment begins a block of its own.
  (with-temp-buffer
    (insert "# Head\n<!-- a\n\n```\nx\n```\nb -->\n")
    (should (equal (overblock-md-preview-fences (point-max) t) '((8 . 31)))))
  ;; A comment of more lines is a block under a line of text too.
  (with-temp-buffer
    (insert "Some text.\n<!--\nOld.\n\nMore.\n-->\n")
    (should (equal (overblock-md-preview-fences (point-max) t) '((12 . 32)))))
  ;; Inside a paragraph a comment is part of it.
  (with-temp-buffer
    (insert "Some *long\n<!-- note -->\nend* here.\n")
    (should (= 1 (length (overblock-md-preview-regions
                          (point-min) (point-max))))))
  ;; A list shown inside a block: its deep fence is content.
  (with-temp-buffer
    (insert "```markdown\n- item\n\n    ```python\n    x = 1\n    ```\n```\n")
    (should (equal (overblock-md-preview-fences (point-max))
                   (list (cons 1 (1- (point-max))))))))

(ert-deftest overblock-md-preview-test-an-item-that-ends-in-its-fence-takes-it ()
  "An item whose last lines are its fence is one block with that fence."
  (with-temp-buffer
    (insert "1. Clone it:\n   ```sh\n   git clone x\n   ```\n\n2. Next.\n")
    (should (equal (car (overblock-md-preview-regions (point-min) (point-max)))
                   (cons 1 (save-excursion (goto-char (point-min))
                                           (forward-line 3) (pos-eol)))))))

(provide 'overblock-md-preview-test)
;;; overblock-md-preview-test.el ends here
