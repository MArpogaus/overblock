;;; overblock-pydoc-scroll-test.el --- Scrolling over rendered doc strings -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Marcel Arpogaus

;; Author: Marcel Arpogaus <znepry.necbtnhf@tznvy.pbz>
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

;; Run with: make scroll
;;
;; A rendered doc string is pieces over its own lines, cloaks over the
;; lines left over, and since `:indent' a display string that begins in
;; the middle of a line.  Scrolling up over a file of them must move the
;; window one way, whatever moves it: the wheel, `scroll-down-command'
;; or `previous-line' walking off the top of the window.

;;; Code:

(require 'ert)
(require 'overblock-pydoc)
(require 'overblock-pycell-scroll-test)
(require 'pixel-scroll)

(defun overblock-pydoc-scroll-test--source (functions)
  "Return a Python module of FUNCTIONS methods with long numpy doc strings."
  (concat "class Thing:\n"
          (mapconcat
           (lambda (n)
             (format "    def method_%d(self, a, b=1):\n\
        \"\"\"Add a and b, number %d, with **bold** and `code`.\n\n\
        A second paragraph of prose that runs on for a while so the\n\
        rendering has something to fill, and a third line of it.\n\n\
        Parameters\n        ----------\n\
        a : int\n            The first operand, described at some length\n\
            over two lines of description.\n\
        b : int, optional\n            The second operand, by default 1.\n\n\
        Returns\n        -------\n        int\n            The sum.\n\
        \"\"\"\n        return a + b\n\n" n n))
           (number-sequence 1 functions) "")))

(defun overblock-pydoc-scroll-test--render ()
  "Turn the mode on in the current buffer and wait for every rendering."
  (python-mode)
  (overblock-pydoc-mode 1)
  (let ((deadline (+ (float-time) 15)))
    (while (and (seq-some (lambda (process)
                            (string-prefix-p "overblock-md" (process-name process)))
                          (process-list))
                (< (float-time) deadline))
      (accept-process-output nil 0.05)))
  (redisplay t))

(defun overblock-pydoc-scroll-test--walk-up (step)
  "Walk the window up from the bottom with STEP, a thunk; return the faults.
The window start may only go down, or stay while the vscroll goes
down, and point, where STEP moves it, may only go down too.  STEP
signalling `beginning-of-buffer' anywhere but at the top is a fault."
  (goto-char (point-max))
  (recenter -1)
  (redisplay t)
  (let ((previous (list (window-start) (window-vscroll nil t) (point)))
        (steps 0)
        faults)
    (while (< (cl-incf steps) 400)
      (condition-case err
          (funcall step)
        (beginning-of-buffer
         (unless (= (window-start) (point-min))
           (push (format "step %d: %S at line %d" steps (car err)
                         (line-number-at-pos (window-start)))
                 faults)))
        (error (push (format "step %d: %S" steps err) faults)))
      (redisplay t)
      (let ((now (list (window-start) (window-vscroll nil t) (point))))
        (when (or (> (nth 0 now) (nth 0 previous))
                  (and (= (nth 0 now) (nth 0 previous))
                       (> (nth 1 now) (nth 1 previous))))
          (push (format "step %d: window %d+%d to %d+%d" steps
                        (line-number-at-pos (nth 0 previous)) (nth 1 previous)
                        (line-number-at-pos (nth 0 now)) (nth 1 now))
                faults))
        (when (> (nth 2 now) (nth 2 previous))
          (push (format "step %d: point line %d to %d" steps
                        (line-number-at-pos (nth 2 previous))
                        (line-number-at-pos (nth 2 now)))
                faults))
        (when (and (= (nth 0 now) (point-min)) (= (nth 2 now) (point-min)))
          (setq steps 999))
        (setq previous now)))
    (nreverse faults)))

(defmacro overblock-pydoc-scroll-test--with-file (&rest body)
  "Run BODY in a window showing a rendered module of doc strings."
  (declare (indent 0))
  `(let ((buffer (generate-new-buffer "*overblock-pydoc scroll*")))
     (unwind-protect
         (progn
           (switch-to-buffer buffer)
           (delete-other-windows)
           (insert (overblock-pydoc-scroll-test--source 8))
           (overblock-pydoc-scroll-test--render)
           (should (= (length (overblock-in (point-min) (point-max) 'pydoc)) 8))
           ,@body)
       (kill-buffer buffer))))

(ert-deftest overblock-pydoc-scroll-test-wheel-up ()
  "The wheel moves the window one way up over rendered doc strings."
  (skip-unless (display-graphic-p))
  (skip-unless (overblock-md-program))
  (skip-unless (>= emacs-major-version 30))
  (overblock-pydoc-scroll-test--with-file
    (should (equal (overblock-pycell-scroll-test--reversals) nil))
    (should (equal (overblock-pycell-scroll-test--stalls) nil))))

(ert-deftest overblock-pydoc-scroll-test-page-up ()
  "`scroll-down-command' moves the window one way up over doc strings."
  (skip-unless (display-graphic-p))
  (skip-unless (overblock-md-program))
  (overblock-pydoc-scroll-test--with-file
    (should (equal (overblock-pydoc-scroll-test--walk-up #'scroll-down-command)
                   nil))))

(ert-deftest overblock-pydoc-scroll-test-line-up ()
  "`previous-line' off the top of the window never moves the window down."
  (skip-unless (display-graphic-p))
  (skip-unless (overblock-md-program))
  (overblock-pydoc-scroll-test--with-file
    (should (equal (overblock-pydoc-scroll-test--walk-up
                    (lambda () (let ((line-move-visual t))
                                 (call-interactively #'previous-line))))
                   nil))))

(provide 'overblock-pydoc-scroll-test)
;;; overblock-pydoc-scroll-test.el ends here
