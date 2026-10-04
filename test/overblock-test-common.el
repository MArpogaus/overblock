;;; overblock-test-common.el --- What the suites share -*- lexical-binding: t; -*-

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

;; No tests: the helpers that several suites need, such as the
;; stand-in for an image, the text of a vtable and the waiter of the
;; live suites.  `TEST := $(wildcard test/*.el)' in the Makefile picks
;; this file up.

;;; Code:

(require 'vtable)
(require 'seq)
(require 'overblock)

(defconst overblock-test-common-image
  (propertize " " 'display '(image :type png :data "x"))
  "A stand-in for what comint-mime inserts for an image.")

(defun overblock-test-common-vtable-text ()
  "Return the text of a vtable, as comint-mime leaves one in the shell."
  (with-temp-buffer
    (make-vtable
     :use-header-line nil
     :columns (mapcar (lambda (name) (list :name name
                                           :min-width (length name)
                                           :align 'right))
                      '("alpha" "beta_longer" "gamma"))
     :objects '(("1" "22" "333") ("4444" "5" "66") ("7" "888" "9999")))
    (buffer-string)))

(defun overblock-test-common-wait (predicate &optional seconds)
  "Wait until PREDICATE answers non-nil and return that answer.
Give up after SECONDS, thirty by default, and return what the
predicate says then.  The live suites wait with this."
  (let ((deadline (+ (float-time) (or seconds 30))))
    (while (and (not (funcall predicate)) (< (float-time) deadline))
      (accept-process-output nil 0.05))
    (funcall predicate)))

(defun overblock-test-common-converted (&optional seconds)
  "Wait until no converter process is running, for SECONDS at most.
The renderings come from pandoc asynchronously, so a test must wait."
  (overblock-test-common-wait
   (lambda ()
     (not (seq-some (lambda (process)
                      (string-prefix-p "overblock-md" (process-name process)))
                    (process-list))))
   (or seconds 10)))

(defun overblock-test-common-results ()
  "Return the result blocks of the buffer, in order."
  (sort (overblock-in (point-min) (point-max) 'result)
        (lambda (a b) (< (overlay-start a) (overlay-start b)))))

(defun overblock-test-common-text (block)
  "Return what the result BLOCK shows, without its properties."
  (substring-no-properties
   (or (plist-get (overblock-get block :data) :text) "")))

(provide 'overblock-test-common)
;;; overblock-test-common.el ends here
