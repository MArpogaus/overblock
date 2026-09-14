;;; init.el --- the configuration the animations were recorded with -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Marcel Arpogaus

;; This file is not part of GNU Emacs.

;;; Commentary:

;; The whole setup behind the animations in the README, and nothing
;; else: a built-in theme, one font, line numbers, and the five
;; packages of this repository turned on.  Run it against a checkout:
;;
;;     emacs -Q -l demo/init.el demo/demo.py
;;
;; The frame is asked for in pixels, because every animation is the
;; same size.  Nothing here is a recommendation — it is what the
;; pictures were taken with, so that anyone can take the same ones.
;;
;; The dependencies the notebooks need are not installed by this file.
;; code-cells and comint-mime come from a package archive, ESS as well;
;; with them missing, `overblock-md-preview-mode' and
;; `overblock-pydoc-mode' still work and the two notebooks do not load.

;;; Code:

;;;; Where the packages are

;; This checkout first, so the code being demonstrated is the code that
;; runs, and then whatever a package archive has installed for the
;; dependencies.
(add-to-list 'load-path (file-name-directory
                         (directory-file-name
                          (file-name-directory (or load-file-name
                                                   buffer-file-name)))))
(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("gnu" . "https://elpa.gnu.org/packages/")))
(package-initialize)

;;;; The look

(setq inhibit-startup-screen t
      initial-scratch-message nil
      make-backup-files nil
      auto-save-default nil
      ring-bell-function #'ignore
      use-short-answers t
      ;; A block is a tall display string, and a scroll that jumps a
      ;; whole one is what `overblock' exists to avoid; these two are
      ;; what let a wheel walk through one.
      scroll-conservatively 101
      scroll-step 1
      ;; The animations show them, and they make plain that a block
      ;; stands over source lines rather than replacing them.
      display-line-numbers-width 3)

(load-theme 'modus-operandi t)          ; built-in since Emacs 28
(global-display-line-numbers-mode 1)
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(fringe-mode 8)
(blink-cursor-mode -1)

(defun demo-set-font ()
  "Take the first font of the list that this system has.
A nerd font first, because the bars draw their buttons with glyphs from
one; `overblock-glyph' falls back to plain characters where the font is
missing, so a system without one shows words instead of icons."
  (when (display-graphic-p)
    (when-let* ((font (seq-find (lambda (name)
                                  (find-font (font-spec :name name)))
                                '("FiraCode Nerd Font Mono"
                                  "JetBrainsMono Nerd Font Mono"
                                  "Symbols Nerd Font Mono"
                                  "DejaVu Sans Mono"))))
      (set-frame-font (format "%s 12" font) nil t))))
(add-hook 'window-setup-hook #'demo-set-font)
(add-hook 'server-after-make-frame-hook #'demo-set-font)

;;;; The packages

(require 'overblock)
(require 'overblock-md)
(require 'overblock-md-preview)
(require 'overblock-pydoc)
;; Each of these needs a package from an archive — code-cells and
;; comint-mime for the notebook, ESS for the R chunks — so a checkout
;; without them still demonstrates the rest.  `ignore-errors' and not
;; the NOERROR of `require': what is missing is the sibling those files
;; require, and a nested `require' signals whatever the outer one says.
(dolist (feature '(overblock-pycell overblock-rmd))
  (unless (ignore-errors (require feature nil t))
    (message "demo: %s needs a package that is not installed" feature)))

;; A notebook's figures ride on comint-mime, which sends its own setup
;; into the interpreter when the shell starts; without it `plt.show()'
;; opens a window of its own and the cell waits for it to be closed.
;; IPython, because that is what comint-mime speaks.
(with-eval-after-load 'python
  (when-let* ((ipython (executable-find "ipython")))
    (setq python-shell-interpreter ipython
          python-shell-interpreter-args "-i --simple-prompt --no-color-info")))
(when (require 'comint-mime nil t)
  (add-hook 'inferior-python-mode-hook #'comint-mime-setup))

;; Nothing above turns anything on, which is how the packages ship.  The
;; animations turn each mode on where they show it; these hooks are the
;; ordinary way to do it in a configuration of your own.
(add-hook 'markdown-mode-hook #'overblock-md-preview-mode)
(when (fboundp 'overblock-pycell-mode-maybe)
  (add-hook 'code-cells-mode-hook #'overblock-pycell-mode-maybe))
(when (fboundp 'overblock-rmd-mode-maybe)
  (add-hook 'markdown-mode-hook #'overblock-rmd-mode-maybe))

(provide 'demo-init)
;;; init.el ends here
