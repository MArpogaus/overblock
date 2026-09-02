;;; complexity-test.el --- Tests for tools/complexity.el -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Marcel Arpogaus

;; Author: Marcel Arpogaus <znepry.necbtnhf@tznvy.pbz>
;; Assisted-by: Claude:claude-opus-5
;; URL: https://github.com/MArpogaus/pycell

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
;; What the measure of `make complexity' counts, rule by rule.  A
;; measure nobody checks is a number nobody can argue with.

;;; Code:

(require 'ert)
(require 'complexity)

(defun complexity-test--score (definition)
  "Return the score of the DEFINITION form."
  (plist-get (complexity-of definition) :score))

(ert-deftest complexity-test-a-straight-line-costs-nothing ()
  "Length is not complexity: calls in a row, and a long `let'."
  (should (= 0 (complexity-test--score
                '(defun f () "Doc." (setq a 1) (setq b 2) (setq c 3)))))
  (should (= 0 (complexity-test--score
                '(defun f () "Doc." (let ((a 1) (b 2) (c 3)) (list a b c))))))
  (should (= 0 (complexity-test--score
                '(defun f () "A doc string of\nseveral lines." (list 1))))))

(ert-deftest complexity-test-a-branch-costs-one-and-an-else-another ()
  "One question is one, and the second way out of it is one more."
  (should (= 1 (complexity-test--score '(defun f (x) "Doc." (if x 1)))))
  (should (= 2 (complexity-test--score '(defun f (x) "Doc." (if x 1 2)))))
  (should (= 1 (complexity-test--score '(defun f (x) "Doc." (when x 1)))))
  (should (= 1 (complexity-test--score '(defun f (x) "Doc." (unless x 1))))))

(ert-deftest complexity-test-nesting-costs-a-level-each-time ()
  "A break inside a break costs one more for every level it is in."
  (should (= 3 (complexity-test--score
                '(defun f (x) "Doc." (when x (when x 1))))))
  (should (= 3 (complexity-test--score
                '(defun f (x) "Doc." (dolist (y x) (when y (push y r)))))))
  (should (= 6 (complexity-test--score
                '(defun f (x) "Doc."
                        (dolist (a x) (dolist (b a) (dolist (c b) (g c))))))))
  (should (= 4 (plist-get (complexity-of
                           '(defun f (x) "Doc."
                                   (dolist (a x)
                                     (dolist (b a)
                                       (dolist (c b)
                                         (when c (g c)))))))
                          :depth))))

(ert-deftest complexity-test-a-run-of-one-operator-costs-one ()
  "However long the run is; a change of operator is a second run."
  (should (= 1 (complexity-test--score '(defun f () "Doc." (and a b c d)))))
  (should (= 1 (complexity-test--score
                '(defun f () "Doc." (and a (and b c))))))
  (should (= 2 (complexity-test--score
                '(defun f () "Doc." (and a (or b c)))))))

(ert-deftest complexity-test-every-clause-after-the-first-costs-one ()
  "A `cond' of three clauses is one break and two more ways through."
  (should (= 3 (complexity-test--score
                '(defun f (x) "Doc." (cond (a 1) (b 2) (c 3))))))
  (should (= 1 (complexity-test--score '(defun f (x) "Doc." (cond (a 1))))))
  (should (= 2 (complexity-test--score
                '(defun f (x) "Doc." (pcase x (1 'one) (_ 'other)))))))

(ert-deftest complexity-test-a-handler-and-a-jump-each-cost-one ()
  "Catching costs one a handler; a `throw' costs one wherever it is."
  (should (= 1 (complexity-test--score
                '(defun f () "Doc." (condition-case nil (g) (error 1))))))
  (should (= 2 (complexity-test--score
                '(defun f () "Doc."
                        (condition-case nil (g) (error 1) (quit 2))))))
  (should (= 1 (complexity-test--score
                '(defun f () "Doc." (catch 'done (throw 'done 1)))))))

(ert-deftest complexity-test-a-lambda-is-a-level-and-recursion-costs-one ()
  "A body handed to another function reads as nested, and self-calls count."
  (should (= 3 (complexity-test--score
                '(defun f (x) "Doc." (mapcar (lambda (y) (if y 1 2)) x)))))
  (should (= 3 (complexity-test--score
                '(defun f (x) "Doc." (if (f (1- x)) 1 2)))))
  (should (plist-get (complexity-of '(defun f (x) "Doc." (f x))) :recurses)))

(ert-deftest complexity-test-a-quoted-form-is-data ()
  "A quoted list of the words `if' and `cond' is not a branch."
  (should (= 0 (complexity-test--score
                '(defun f () "Doc." '(if cond when dolist)))))
  (should (= 0 (complexity-test--score
                '(defun f () "Doc." (list #'if-let* 'when))))))

(ert-deftest complexity-test-a-file-is-read-and-counted ()
  "Every definition of a file is found, with its lines told apart."
  (let* ((file (make-temp-file "complexity" nil ".el"))
         (found (progn
                  (with-temp-file file
                    (insert ";;; a comment above\n"
                            "(defun one (x)\n"
                            "  \"Doc.\n"
                            "A second line of it.\"\n"
                            "  ;; a comment inside\n"
                            "  (when x 1))\n"
                            "\n"
                            "(defvar two nil \"Not a function.\")\n"))
                  (complexity-file file))))
    (delete-file file)
    (should (= 1 (length found)))
    (should (eq 'one (plist-get (car found) :name)))
    (should (= 2 (plist-get (car found) :line)))
    (should (= 1 (plist-get (car found) :score)))
    (should (= 2 (plist-get (car found) :doc)))
    (should (= 1 (plist-get (car found) :comment)))
    (should (= 2 (plist-get (car found) :code)))))

(provide 'complexity-test)
;;; complexity-test.el ends here
