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
;; What only a real R can show.  The batch suite starts no process.
;; These tests send real chunks to a real R and read back what the
;; blocks show: the prompt that ess-tracebug hands over, named chunks,
;; and the indentation of a table.
;;
;; `make test' does not load this file, and the target that does skips
;; with a word where no R is installed.

;;; Code:

(require 'ert)
(require 'overblock-test-common)
(require 'markdown-mode)
(require 'overblock-rmd)

(defun overblock-rmd-live-test--idle-p ()
  "Return non-nil while R is there and runs no chunk."
  (when-let* ((proc (overblock-rmd--process)))
    (not (buffer-local-value 'overblock-run--state (process-buffer proc)))))

(defun overblock-rmd-live-test--run-first ()
  "Run the first chunk of the buffer and wait for its result.
Return the text of that result."
  (pcase-let ((`(,_open ,beg ,end) (car (overblock-rmd-chunks))))
    (overblock-run-region beg end))
  (should (overblock-test-common-wait
           (lambda () (and (overblock-rmd-live-test--idle-p)
                           (overblock-test-common-results)))
           60))
  (overblock-test-common-text (car (overblock-test-common-results))))

(defmacro overblock-rmd-live-test--with-document (text &rest body)
  "Evaluate BODY in an Rmd buffer holding TEXT, wired for a real R.
The R of an earlier test is reused where one is alive: each startup
costs seconds, and the package keeps one R across a session anyway.

The prose is not rendered: these tests are about what R answers."
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
           ;; `overblock-only-in' refuses a buffer not in `markdown-mode'.
           (markdown-mode)
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
`comint-prompt-regexp' bound to \"^$\", so the strip must read the
prompt of ESS."
  (overblock-rmd-live-test--with-document "```{r one}\n40 + 2\n```\n"
    (should (equal (overblock-rmd-live-test--run-first) "[1] 42"))))

(ert-deftest overblock-rmd-live-test-a-chunk-that-draws-comes-back-with-its-figure ()
  "A chunk that plots answers with the figure, after what it printed.
R draws to the PNG device of the wrapper, and the file comes back as
an image in the result."
  (skip-unless (image-type-available-p 'png))
  (overblock-rmd-live-test--with-document
      "```{r fig}\ncat(\"before\\n\")\nplot(1:3)\n```\n"
    (let ((text (overblock-rmd-live-test--run-first)))
      (should (string-prefix-p "before" text))
      (should-not (string-search "overblock-figure" text))
      (let ((results (overblock-test-common-results)))
        (should (overblock-image-in
                 (plist-get (overblock-get (car results) :data) :text)))))))

(ert-deftest overblock-rmd-live-test-a-chunk-prints-every-statement ()
  "Every top level expression of a chunk prints, and no prompt is between.
This is what the `source' wrapper gives.  Sent line by line, R prompts
after each statement, in the middle of the output; a bare `eval' prints
only the last value."
  (overblock-rmd-live-test--with-document
      "```{r many}\nx <- 1:3\nx\nsum(x)\ncat(\"done\\n\")\n```\n"
    (let ((text (overblock-rmd-live-test--run-first)))
      (should (equal text "[1] 1 2 3\n[1] 6\ndone"))
      ;; The assignment printed nothing, as at the prompt of R.
      (should-not (string-match-p ">" text)))))

(ert-deftest overblock-rmd-live-test-a-table-keeps-its-columns ()
  "An aligned table comes back with its header over its numbers.
R indents the header of a `summary' and lines the values up under it,
so the trim of a result keeps the leading spaces."
  (overblock-rmd-live-test--with-document
      "```{r table}\nsummary(c(1, 2, 3, 4))\n```\n"
    (let* ((text (overblock-rmd-live-test--run-first))
           (lines (split-string text "\n")))
      (should (= (length lines) 2))
      ;; The header is indented, and "Min." starts where "1.00" does.
      (should (string-prefix-p " " (car lines)))
      (should (= (string-match-p "Min\\." (car lines))
                 (string-match-p "1\\.00" (cadr lines)))))))

(ert-deftest overblock-rmd-live-test-a-chunk-of-quotes-survives-the-trip ()
  "A chunk carrying quotes, backslashes and newlines reaches R whole.
The chunk travels inside an R string literal, which each of the three
could end, with the line or both."
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
    (overblock-run-restart-and-run-all)
    (should (overblock-test-common-wait
             (lambda () (and (overblock-rmd-live-test--idle-p)
                             (null (overblock-run--queued))
                             (= (length (overblock-test-common-results)) 2)))
             60))
    (should (equal (overblock-test-common-text
                    (car (overblock-test-common-results)))
                   "[1] \"first\""))
    (should (string-match-p "boom" (overblock-test-common-text
                                    (cadr (overblock-test-common-results)))))
    ;; The third chunk is not sent.
    (should (= (length (overblock-test-common-results)) 2))))

(ert-deftest overblock-rmd-live-test-a-pass-carries-state-between-chunks ()
  "A later chunk sees what an earlier one defined.
The chunks go to one R at its top level: `source' with its default
`local = FALSE' evaluates in the global environment."
  (overblock-rmd-live-test--with-document
      "```{r set}\nlive_value <- 7\n```\n\n```{r use}\nlive_value * 6\n```\n"
    (overblock-run-restart-and-run-all)
    (should (overblock-test-common-wait
             (lambda () (and (overblock-rmd-live-test--idle-p)
                             (null (overblock-run--queued))
                             (= (length (overblock-test-common-results)) 2)))
             60))
    ;; The assignment printed nothing, and the next chunk sees it.
    (should (equal (overblock-test-common-text
                    (car (overblock-test-common-results)))
                   ""))
    (should (equal (overblock-test-common-text
                    (cadr (overblock-test-common-results)))
                   "[1] 42"))))

(ert-deftest overblock-rmd-live-test-stop-works-while-the-last-chunk-runs ()
  "`overblock-run-stop' during the last chunk leaves nothing queued.
The last chunk of a pass is sent with the queue already empty, and the
running chunk runs to its end."
  (overblock-rmd-live-test--with-document
      "```{r a}\n\"first\"\n```\n\n```{r b}\nSys.sleep(1)\n\"last\"\n```\n"
    (overblock-run-restart-and-run-all)
    ;; The last chunk runs: nothing queued, one chunk live.
    (should (overblock-test-common-wait
             (lambda ()
               (when-let* ((proc (overblock-rmd--process)))
                 (and (null (overblock-run--queued))
                      (buffer-local-value 'overblock-run--state
                                          (process-buffer proc)))))
             60))
    (overblock-run-stop)
    (should-not (overblock-run--queued))
    (should (overblock-test-common-wait
             #'overblock-rmd-live-test--idle-p 60))
    ;; The running chunk runs to its end: both results arrive.
    (should (= (length (overblock-test-common-results)) 2))
    (should (equal (overblock-test-common-text
                    (cadr (overblock-test-common-results)))
                   "[1] \"last\""))))

(ert-deftest overblock-rmd-live-test-a-restart-forgets-what-r-knew ()
  "A restart gives a fresh R, and takes the results down with it."
  (overblock-rmd-live-test--with-document
      "```{r a}\nrestart_witness <- 1\nexists(\"restart_witness\")\n```\n"
    (should (equal (overblock-rmd-live-test--run-first) "[1] TRUE"))
    (overblock-run-restart)
    (should-not (overblock-test-common-results))
    (should (overblock-rmd--process))
    ;; The new R does not know it.
    (overblock-rmd-live-test--with-document
        "```{r b}\nexists(\"restart_witness\")\n```\n"
      (should (equal (overblock-rmd-live-test--run-first) "[1] FALSE")))))

(ert-deftest overblock-rmd-live-test-a-second-chunk-waits-its-turn ()
  "A chunk sent while another one runs is queued, and runs after it.
Point comes back to where the second chunk was asked for."
  (overblock-rmd-live-test--with-document
      "```{r slow}\nSys.sleep(2)\n1\n```\n\n```{r other}\n2\n```\n"
    (pcase-let ((`(,_open ,beg ,end) (car (overblock-rmd-chunks))))
      (overblock-run-region beg end))
    (goto-char (point-max))
    (pcase-let ((`(,_open ,beg ,end) (cadr (overblock-rmd-chunks))))
      (overblock-run-region beg end)
      (should (equal (mapcar (lambda (region) (marker-position (car region)))
                             (overblock-run--queued))
                     (list beg))))
    (should (overblock-test-common-wait
             (lambda () (and (overblock-rmd-live-test--idle-p)
                             (= (length (overblock-test-common-results)) 2)))
             60))
    (should (equal (mapcar #'overblock-test-common-text
                           (overblock-test-common-results))
                   '("[1] 1" "[1] 2")))
    (should (= (point) (point-max)))))

(provide 'overblock-rmd-live-test)
;;; overblock-rmd-live-test.el ends here
