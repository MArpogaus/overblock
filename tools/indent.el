;;; indent.el --- Indent Emacs Lisp the way Emacs does  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Marcel Arpogaus

;; Author: Marcel Arpogaus <znepry.necbtnhf@tznvy.pbz>
;; Assisted-by: Claude:claude-opus-5

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

;; The formatter of this repository:
;;
;;     emacs -Q --batch -l tools/indent.el FILE...
;;
;; Every file is indented in place, by `indent-region' — the indentation
;; Emacs itself gives Lisp, and nothing else.  Lines are not reflowed and
;; no form is rewritten: what a reader lined up by hand stays as it is,
;; and only the leading whitespace of a line can change.
;;
;; A file that had to be changed is named, and the exit status is then 1,
;; so a hook can stop a commit that was not indented.

;;; Code:

(let ((changed nil))
  (dolist (file command-line-args-left)
    ;; The file is visited rather than read: visiting applies the
    ;; directory-local variables, so `indent-tabs-mode' and the rest are
    ;; the repository's own and not this Emacs's defaults.
    (with-current-buffer (find-file-noselect file)
      (let ((before (buffer-string))
            (inhibit-message t))
        (indent-region (point-min) (point-max))
        (unless (equal before (buffer-string))
          (setq changed t)
          (save-buffer)
          (message "indented %s" file)))))
  (kill-emacs (if changed 1 0)))

;;; indent.el ends here
