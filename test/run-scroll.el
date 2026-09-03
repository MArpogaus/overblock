;;; run-scroll.el --- Runner for the scrolling tests -*- lexical-binding: t; -*-

;;; Commentary:

;; The scrolling tests need a graphical frame, so they cannot run in a
;; batch session.  In a graphical one `ert-run-tests-batch-and-exit'
;; reports through `message', which goes to *Messages* and not to
;; standard output, and `send-string-to-terminal' fails outright.  This
;; runner therefore writes its report to a file, which the Makefile
;; prints, and leaves the outcome in the exit status.

;;; Code:

(require 'ert)
(require 'pycell-scroll-test)

(defconst run-scroll-report "scroll-report.txt"
  "File the report is written to.")

(defun run-scroll--say (format-string &rest args)
  "Append FORMAT-STRING with ARGS to `run-scroll-report'."
  (write-region (concat (apply #'format format-string args) "\n")
                nil run-scroll-report t 'quiet))

(defun run-scroll--all ()
  "Run every scrolling test, report to a file, and exit."
  (let ((failed 0) (passed 0) (skipped 0))
    ;; A fixed frame size, so the test sees the same geometry on every
    ;; machine: the reversal depends on how the blocks fill the window.
    (set-frame-size (selected-frame) 1000 700 t)
    (redisplay t)
    (run-scroll--say "graphical=%s frame=%dx%d" (display-graphic-p)
                     (frame-pixel-width) (frame-pixel-height))
    (dolist (test (ert-select-tests "pycell-scroll-" t))
      (let* ((name (ert-test-name test))
             (start (float-time))
             (result (ert-run-test test))
             (seconds (- (float-time) start)))
        (cond ((ert-test-passed-p result)
               (setq passed (1+ passed))
               (run-scroll--say "  PASS %-40s %.1fs" name seconds))
              ((ert-test-result-type-p result :skipped)
               (setq skipped (1+ skipped))
               (run-scroll--say "  SKIP %s" name))
              (t
               (setq failed (1+ failed))
               (run-scroll--say "  FAIL %-40s %.1fs" name seconds)
               (run-scroll--say
                "%S" (ert-test-result-with-condition-condition result))))))
    ;; A skipped test is not a passed one.  Both tests open with
    ;; `skip-unless (display-graphic-p)', so a run without xvfb-run
    ;; skipped them both, printed "scrolling tests passed" and exited
    ;; 0: the one promise this suite guards was never exercised and the
    ;; CI was green.
    (cond ((not (zerop failed))
           (run-scroll--say "%d scrolling test(s) failed" failed))
          ((zerop passed)
           (run-scroll--say "no scrolling test ran: %d skipped" skipped))
          (t (run-scroll--say "scrolling tests passed")))
    (kill-emacs (if (and (zerop failed) (> passed 0)) 0 1))))

;; The tests measure pixels, so they wait until redisplay has brought
;; the frame up rather than running while the file loads.
(run-with-timer 0.5 nil #'run-scroll--all)

(provide 'run-scroll)
;;; run-scroll.el ends here
