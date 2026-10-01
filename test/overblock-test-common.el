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

;; No tests: what several suites need, in one place.  The stand-in for
;; an image was written out in three of them and the text of a vtable
;; in two, and the live suites keep one waiter each.  `test/run-scroll.el'
;; is the precedent for a file here that holds no tests, and
;; `TEST := $(wildcard test/*.el)' picks this one up with no change to
;; the Makefile.

;;; Code:

(require 'vtable)

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
Give up after SECONDS, thirty by default, and answer whatever the
predicate says then.  What a live suite waits with: a real interpreter
answers when it answers."
  (let ((deadline (+ (float-time) (or seconds 30))))
    (while (and (not (funcall predicate)) (< (float-time) deadline))
      (accept-process-output nil 0.05))
    (funcall predicate)))

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
