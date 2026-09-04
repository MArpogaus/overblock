;;; overblock-rmd-live-test.el --- Tests against a real R  -*- lexical-binding: t; -*-

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

;; Run with: make test-live
;;
;; What only a real R can prove.  The batch suite starts no process,
;; and three faults lived past every one of its checks for exactly that
;; reason:
;;
;;   - every result came back with a bare > on a line of its own,
;;     because ess-tracebug hands the prompt to the comint filter with
;;     `comint-prompt-regexp' bound to "^$" and the strip read that
;;     binding rather than the buffer's own prompt;
;;   - the chunk walk read the engine name as ending at a comma or a
;;     brace, so ```{r setup} was not an R chunk at all and a file of
;;     named chunks had none;
;;   - the header of an aligned table lost its indentation, so every
;;     `summary' came back with its columns three characters out.
;;
;; These tests send real chunks down a real R and read back what the
;; blocks show.
;;
;; `make test' does not load this file, and the target that does skips
;; with a word where no R is installed.

;;; Code:

(require 'ert)
(require 'overblock-rmd)

(defun overblock-rmd-live-test--wait (predicate &optional seconds)
  "Wait until PREDICATE answers non-nil and return that answer.
Give up after SECONDS, thirty by default, and answer whatever the
predicate says then."
  (let ((deadline (+ (float-time) (or seconds 30))))
    (while (and (not (funcall predicate)) (< (float-time) deadline))
      (accept-process-output nil 0.05))
    (funcall predicate)))

(defun overblock-rmd-live-test--idle-p ()
  "Return non-nil while R is there and runs no chunk."
  (when-let* ((proc (overblock-rmd--process)))
    (not (buffer-local-value 'overblock-run--state (process-buffer proc)))))

(defun overblock-rmd-live-test--results ()
  "Return the result blocks of the buffer, in order."
  (sort (overblock-in (point-min) (point-max) 'result)
        (lambda (a b) (< (overlay-start a) (overlay-start b)))))

(defun overblock-rmd-live-test--text (block)
  "Return what the result BLOCK shows, without its properties."
  (substring-no-properties
   (or (plist-get (overblock-get block :data) :text) "")))

(defun overblock-rmd-live-test--run-first ()
  "Run the first chunk of the buffer and wait for its result.
Answers the text of that result."
  (pcase-let ((`(,_open ,beg ,end) (car (overblock-rmd-chunks))))
    (overblock-run-region beg end))
  (should (overblock-rmd-live-test--wait
           (lambda () (and (overblock-rmd-live-test--idle-p)
                           (overblock-rmd-live-test--results)))
           60))
  (overblock-rmd-live-test--text (car (overblock-rmd-live-test--results))))

(defmacro overblock-rmd-live-test--with-document (text &rest body)
  "Evaluate BODY in an Rmd buffer holding TEXT, wired for a real R.
The R of an earlier test is reused where one is alive, which is what
the tests want: each startup costs seconds, and the package is meant to
keep one R across a session anyway.

The prose is not rendered: these tests are about what R answers, and a
converter process a paragraph would only make them slow."
  (declare (indent 1))
  `(let ((ess-ask-for-ess-directory nil)
         (ess-history-file nil)
         (ess-eval-visibly nil)
         (inferior-R-args "--no-save --no-restore --quiet")
         (overblock-md-command nil)
         (buffer (generate-new-buffer "overblock-rmd-live.Rmd")))
     (unwind-protect
         (with-current-buffer buffer
           (insert ,text)
           (setq buffer-file-name "/tmp/overblock-rmd-live.Rmd")
           (text-mode)
           (overblock-rmd-mode 1)
           (goto-char (point-min))
           ,@body)
       (with-current-buffer buffer
         (overblock-rmd-mode -1)
         (set-buffer-modified-p nil))
       (kill-buffer buffer))))

(ert-deftest overblock-rmd-live-test-a-chunk-comes-back-with-its-value ()
  "A chunk runs and its value shows inline, with no prompt left on it.
ess-tracebug hands the prompt to the comint filter with
`comint-prompt-regexp' bound to \"^$\", so a strip that read that
variable took nothing off: every result came back with a bare > on a
line of its own."
  (overblock-rmd-live-test--with-document "```{r one}\n40 + 2\n```\n"
    (should (equal (overblock-rmd-live-test--run-first) "[1] 42"))))

(ert-deftest overblock-rmd-live-test-a-chunk-prints-every-statement ()
  "Every top level expression of a chunk prints, and no prompt is between.
That is what the `source' wrapper buys.  Sent line by line R prompts
after each statement and those prompts land in the middle of the
output; a bare `eval' of the whole chunk would print the last value
only."
  (overblock-rmd-live-test--with-document
      "```{r many}\nx <- 1:3\nx\nsum(x)\ncat(\"done\\n\")\n```\n"
    (let ((text (overblock-rmd-live-test--run-first)))
      (should (equal text "[1] 1 2 3\n[1] 6\ndone"))
      ;; the assignment printed nothing, as at R's own prompt
      (should-not (string-match-p ">" text)))))

(ert-deftest overblock-rmd-live-test-a-table-keeps-its-columns ()
  "An aligned table comes back with its header over its numbers.
R indents the header of a `summary' and lines the values up under it,
and the trim that took the outer whitespace off a result took those
leading spaces with them: the header stood three characters to the left
of the row below it."
  (overblock-rmd-live-test--with-document
      "```{r table}\nsummary(c(1, 2, 3, 4))\n```\n"
    (let* ((text (overblock-rmd-live-test--run-first))
           (lines (split-string text "\n")))
      (should (= (length lines) 2))
      ;; the header is indented, and "Min." begins where "1.00" does
      (should (string-prefix-p " " (car lines)))
      (should (= (string-match-p "Min\\." (car lines))
                 (string-match-p "1\\.00" (cadr lines)))))))

(ert-deftest overblock-rmd-live-test-a-chunk-of-quotes-survives-the-trip ()
  "A chunk carrying quotes, backslashes and newlines reaches R whole.
The chunk travels inside an R string literal, so each of the three
would otherwise end that literal, the line, or both."
  (overblock-rmd-live-test--with-document
      "```{r quotes}\ncat(\"a\\tb\\n\")\n'say \\\"hi\\\"'\n```\n"
    (should (equal (overblock-rmd-live-test--run-first)
                   "a\tb\n[1] \"say \\\"hi\\\"\""))))

(ert-deftest overblock-rmd-live-test-an-error-reads-as-one ()
  "A chunk that raises comes back with R's own message, and marked."
  (overblock-rmd-live-test--with-document
      "```{r bad}\nlog(\"not a number\")\n```\n"
    (let ((text (overblock-rmd-live-test--run-first)))
      (should (string-match-p "non-numeric argument" text))
      (should (overblock-rmd--error-p text)))))

(ert-deftest overblock-rmd-live-test-a-pass-stops-at-an-error ()
  "A pass over every chunk stops at the first one that raises."
  (overblock-rmd-live-test--with-document
      "```{r a}\n\"first\"\n```\n\nprose\n\n```{r b}\nstop(\"boom\")\n```\n\n\
```{r c}\n\"never\"\n```\n"
    (overblock-rmd-restart-and-run-all)
    (should (overblock-rmd-live-test--wait
             (lambda () (and (overblock-rmd-live-test--idle-p)
                             (null (overblock-run-queued))
                             (= (length (overblock-rmd-live-test--results)) 2)))
             60))
    (should (equal (overblock-rmd-live-test--text
                    (car (overblock-rmd-live-test--results)))
                   "[1] \"first\""))
    (should (string-match-p "boom" (overblock-rmd-live-test--text
                                    (cadr (overblock-rmd-live-test--results)))))
    ;; the third chunk was never sent
    (should (= (length (overblock-rmd-live-test--results)) 2))))

(ert-deftest overblock-rmd-live-test-a-pass-carries-state-between-chunks ()
  "A later chunk sees what an earlier one defined.
The chunks go to one R at its top level, so a pass reads as the file
reads: `source' with `local = FALSE', which is its default, evaluates
in the global environment."
  (overblock-rmd-live-test--with-document
      "```{r set}\nlive_value <- 7\n```\n\n```{r use}\nlive_value * 6\n```\n"
    (overblock-rmd-restart-and-run-all)
    (should (overblock-rmd-live-test--wait
             (lambda () (and (overblock-rmd-live-test--idle-p)
                             (null (overblock-run-queued))
                             (= (length (overblock-rmd-live-test--results)) 2)))
             60))
    ;; the assignment printed nothing, and the chunk after it saw it
    (should (equal (overblock-rmd-live-test--text
                    (car (overblock-rmd-live-test--results)))
                   ""))
    (should (equal (overblock-rmd-live-test--text
                    (cadr (overblock-rmd-live-test--results)))
                   "[1] 42"))))

(ert-deftest overblock-rmd-live-test-stop-works-while-the-last-chunk-runs ()
  "`overblock-rmd-stop' during the last chunk leaves nothing queued.
The last chunk of a pass is sent with the queue already empty, and the
running chunk runs to its end."
  (overblock-rmd-live-test--with-document
      "```{r a}\n\"first\"\n```\n\n```{r b}\nSys.sleep(1)\n\"last\"\n```\n"
    (overblock-rmd-restart-and-run-all)
    ;; the last chunk is the one running: nothing queued, one chunk live
    (should (overblock-rmd-live-test--wait
             (lambda ()
               (when-let* ((proc (overblock-rmd--process)))
                 (and (null (overblock-run-queued))
                      (buffer-local-value 'overblock-run--state
                                          (process-buffer proc)))))
             60))
    (overblock-rmd-stop)
    (should-not (overblock-run-queued))
    (should (overblock-rmd-live-test--wait
             #'overblock-rmd-live-test--idle-p 60))
    ;; the running chunk was not cut short: both results arrived
    (should (= (length (overblock-rmd-live-test--results)) 2))
    (should (equal (overblock-rmd-live-test--text
                    (cadr (overblock-rmd-live-test--results)))
                   "[1] \"last\""))))

(ert-deftest overblock-rmd-live-test-a-restart-forgets-what-r-knew ()
  "A restart gives a fresh R, and takes the results down with it."
  (overblock-rmd-live-test--with-document
      "```{r a}\nrestart_witness <- 1\nexists(\"restart_witness\")\n```\n"
    (should (equal (overblock-rmd-live-test--run-first) "[1] TRUE"))
    (overblock-rmd-restart)
    (should-not (overblock-rmd-live-test--results))
    (should (overblock-rmd--process))
    ;; the new R has never heard of it
    (overblock-rmd-live-test--with-document
        "```{r b}\nexists(\"restart_witness\")\n```\n"
      (should (equal (overblock-rmd-live-test--run-first) "[1] FALSE")))))

(ert-deftest overblock-rmd-live-test-a-second-chunk-is-refused ()
  "A chunk sent while another one runs is refused, and says why."
  (overblock-rmd-live-test--with-document
      "```{r slow}\nSys.sleep(2)\n1\n```\n\n```{r other}\n2\n```\n"
    (pcase-let ((`(,_open ,beg ,end) (car (overblock-rmd-chunks))))
      (overblock-run-region beg end))
    (pcase-let ((`(,_open ,beg ,end) (cadr (overblock-rmd-chunks))))
      (should-error (overblock-run-region beg end) :type 'user-error))
    ;; and the first one still finishes
    (should (overblock-rmd-live-test--wait
             #'overblock-rmd-live-test--idle-p 60))
    (should (equal (overblock-rmd-live-test--text
                    (car (overblock-rmd-live-test--results)))
                   "[1] 1"))))

(provide 'overblock-rmd-live-test)
;;; overblock-rmd-live-test.el ends here
