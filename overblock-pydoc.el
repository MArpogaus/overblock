;;; overblock-pydoc.el --- Python documentation, read as documentation  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Marcel Arpogaus

;; Author: Marcel Arpogaus <znepry.necbtnhf@tznvy.pbz>
;; Assisted-by: Claude:claude-opus-5
;; Keywords: languages, docs, convenience
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

;; `overblock-pydoc-mode' shows the doc strings of a Python buffer as
;; the documentation they are: the triple quotes and the indentation go,
;; the markup is rendered, and the code around them is untouched.  Move
;; point into one and its source comes back to be edited; move away and
;; it reads as documentation again.  A click puts point in it, which
;; comes to the same thing.
;;
;; A doc string is a string that opens a line of its own, and stands
;; either at the top of the file or under a line that ends in a colon —
;; which is what a module, a class and a function doc string have in
;; common.  A string anywhere else is data and is left alone.  The
;; syntax state answers all of it, so the mode needs no parser and no
;; grammar: it works in `python-mode' and in `python-ts-mode' alike.
;;
;; It is the second of the two modes that show `overblock' to be a
;; layer rather than a part of the notebook, and the shorter one: the
;; cycle it runs is `overblock-live-start', the same call
;; `overblock-md-preview-mode' makes, and what is left here is which
;; regions and rendered with what.

;;; Code:

(require 'overblock)
(require 'overblock-md)
(require 'rst)

(defgroup overblock-pydoc nil
  "Python doc strings rendered where they are written."
  :group 'languages
  :prefix "overblock-pydoc-")

(defcustom overblock-pydoc-idle 0.2
  "Seconds of quiet before a doc string is rendered again.
See `overblock-md-preview-idle', which this follows."
  :type 'number)

(defcustom overblock-pydoc-command
  '("pandoc --mathjax --no-highlight -f rst"
    "pandoc --mathjax --no-highlight -f markdown")
  "How to turn a doc string into HTML.
Read as `overblock-md-command' is read — one shell command, or a list
of candidates of which the first one installed is used — and it stands
in its place while a doc string is rendered.

reStructuredText first, because that is what Python's own tools read,
and numpydoc and Sphinx with them.  A project that writes Markdown in
its doc strings puts a Markdown command first, or names one alone.

No highlighting, for the reason `overblock-md-command' gives: shr
reads no CSS class, so what pandoc spends on painting a code block is
spent for nothing."
  :type '(choice string (repeat string)))

(defcustom overblock-pydoc-renderer 'converter
  "How a doc string is rendered.

`converter\' hands it to `overblock-pydoc-command\' and renders the HTML
that comes back with shr: it knows reStructuredText, lays a table out
in columns and fills a paragraph to the window, and the rendering is as
tall as it needs to be.  One process for the buffer, asked and not
waited for.

`fontify\' runs `overblock-pydoc-fontify-mode\' over the doc string and
keeps what its font lock painted — the way eldoc shows what a language
server sends it.  No process at all, and the text stays where the
writer put it, so the rendering is exactly as tall as the source and no
code below it moves.  The markup characters stay visible, coloured
rather than replaced."
  :type '(choice (const :tag "A converter, rendered by shr" converter)
                 (const :tag "Fontified where it stands" fontify)))

(defcustom overblock-pydoc-fontify-mode #'rst-mode
  "Major mode whose font lock renders a doc string.
Read where `overblock-pydoc-renderer\' is `fontify\'.  `rst-mode\' is
built in and knows what Python\'s own tools read: a numpydoc section
title comes back as a title.  A project that writes Markdown in its doc
strings names `gfm-view-mode\', which markdown-mode brings and which
hides the markup it has painted."
  :type 'function)

(defvar-keymap overblock-pydoc-map
  :doc "Keymap on a rendered doc string.
A click shows the source of the doc string, which is what a reader
wants of a rendering they mean to edit."
  "<mouse-1>" #'overblock-live-edit)

;;;; Which regions

(defun overblock-pydoc--documentation-p (start)
  "Return non-nil where the string beginning at START is documentation.
A doc string opens a line of its own, and stands at the top of the file
or under a line that ends in a colon: that is what the doc string of a
module, of a class and of a function have in common.  A string
anywhere else is data — a value assigned, an argument passed — and
means nothing to a reader as prose."
  (save-excursion
    (goto-char start)
    (and (string-blank-p (buffer-substring-no-properties (pos-bol) start))
         (or (bobp)
             (save-excursion
               (forward-line -1)
               ;; Past what stands between the definition and its doc
               ;; string: blank lines, and comments of their own.
               (while (and (not (bobp))
                           (looking-at-p "[[:blank:]]*\\(#.*\\)?$"))
                 (forward-line -1))
               (or (bobp)
                   ;; A colon ends the line a definition opens; a
                   ;; comment may follow it.
                   (looking-at-p ".*:[[:blank:]]*\\(#.*\\)?$")))))))

(defun overblock-pydoc--strings (beg end)
  "Return the bounds of every doc string between BEG and END.
Each is a cons of the position of the opening quote and the one after
the closing quote.  The syntax state says what is a string, which is
why no parser is needed and a string inside a comment is never one."
  (save-excursion
    (goto-char (point-min))
    (let (found)
      (while (re-search-forward "\"\"\"\\|'''" end t)
        (let* ((quotes (match-string-no-properties 0))
               (opened (match-end 0))
               (state (save-match-data (syntax-ppss (1+ (match-beginning 0)))))
               (start (and (nth 3 state) (nth 8 state))))
          (if (not start)
              ;; Quotes that open no string: inside a comment, or the
              ;; closing quotes of a string this loop already took.
              (goto-char opened)
            ;; The end is the same three quotes again.  Not
            ;; `scan-sexps': python-mode gives the first quote of the
            ;; three the syntax of a plain string delimiter, so a scan
            ;; from the string's start reads the first two as an empty
            ;; string, and every doc string came out two characters
            ;; long.
            (let ((finish (save-match-data
                            (goto-char opened)
                            (if (search-forward quotes end t) (point) end))))
              (when (and (>= start beg)
                         (overblock-pydoc--documentation-p start))
                (push (cons start finish) found))
              (goto-char (max finish opened))))))
      (nreverse found))))

;;;; What to render them with

(defun overblock-pydoc--source (beg end)
  "Return the prose of the doc string BEG..END.
The quotes go, and so does the indentation every line shares with the
definition it belongs to: a doc string is written where the code stands
and reads as prose one column from the left."
  (let* ((text (buffer-substring-no-properties beg end))
         (bare (replace-regexp-in-string
                "\\(?:\"\"\"\\|'''\\)\\'" ""
                (replace-regexp-in-string
                 "\\`[rbuRBU]*\\(?:\"\"\"\\|'''\\)" "" text)))
         (lines (split-string bare "\n"))
         ;; The first line stands after the quotes and shares no
         ;; indentation with the rest, so the common indentation is
         ;; measured on the lines that follow it.
         (rest (seq-remove #'string-blank-p (cdr lines)))
         (indent (if rest
                     (apply #'min (mapcar (lambda (line)
                                            (string-match-p "[^[:blank:]]"
                                                            line))
                                          rest))
                   0)))
    (string-trim-right
     (string-join (cons (string-trim (car lines))
                        (mapcar (lambda (line)
                                  (if (> (length line) indent)
                                      (substring line indent)
                                    (string-trim line)))
                                (cdr lines)))
                  "\n"))))

(defun overblock-pydoc--indented (text indent)
  "Return TEXT with INDENT spaces before every line but the first.
A doc string belongs to the definition above it and reads as its
prose: rendered from the first column it stood apart from the code it
documents, and a reader had to look twice to see which was which.

Every line but the first, because the first hangs where the quotes
stood: `overblock--rows\' begins its first row at the block, which is
the opening quote and already that far in, while every row after it
begins at a line start.  Padding the first line as well put it two
indents deep.

`split-string\' keeps the faces of the pieces it cuts, so the rendering
comes back indented and still rendered."
  (if (zerop indent)
      text
    (let ((pad (make-string indent ?\s))
          (lines (split-string text "\n")))
      (string-join (cons (car lines)
                         (mapcar (lambda (line) (concat pad line))
                                 (cdr lines)))
                   "\n"))))

(defun overblock-pydoc--show (beg end &optional html)
  "Render the doc string BEG..END over its own source, and return it.
HTML is what the converter answered for this doc string, where a caller
sent the whole buffer through one process."
  (when-let* ((source (overblock-pydoc--source beg end))
              ((not (string-empty-p source)))
              (overblock-md-command overblock-pydoc-command)
              (rendered (overblock-pydoc--indented
                         (if (eq overblock-pydoc-renderer 'fontify)
                             (overblock-md-fontified
                              source overblock-pydoc-fontify-mode)
                           (overblock-md-rendered source html))
                         (save-excursion (goto-char beg)
                                         (current-indentation))))
              ((not (string-empty-p (string-trim rendered))))
              (block (overblock-show
                      beg end
                      :kind 'pydoc
                      :over (overblock-fill-props
                             (overblock-faced rendered 'font-lock-doc-face)
                             'keymap overblock-pydoc-map
                             'help-echo "mouse-1: edit this doc string")
                      :keymap overblock-pydoc-map
                      :help-echo "mouse-1: edit this doc string")))
    ;; An edit the mode did not see coming leaves a rendering of prose
    ;; that has changed; point entering the string takes it off first,
    ;; so the reader's own typing never reaches this.
    (overblock-stale-when-edited block)
    block))

;;;; When

(defun overblock-pydoc--pending (beg end)
  "Return the doc strings between BEG and END that want rendering.
What is left out: a doc string that carries a rendering already, and the
one point is in — the reader is editing that one, and rendering it
would take the text out from under them."
  (seq-remove (lambda (region)
                (or (<= (car region) (point) (cdr region))
                    (overblock-in (car region) (cdr region) 'pydoc)))
              (overblock-pydoc--strings beg end)))

(defun overblock-pydoc--render-fontified (regions)
  "Render REGIONS with font lock.
Nothing is waited for because nothing is started: the renderings are
there when this returns."
  (dolist (region regions)
    (overblock-pydoc--show (car region) (cdr region))))

(defun overblock-pydoc--render-converted (regions)
  "Render REGIONS with the converter, in one process, asked not awaited.
Measured, eight doc strings cost 145 milliseconds one process apiece
and the reader felt every one; this way they cost 7 and the renderings
arrive together a moment later.

The callback renders each doc string that is still without one: the
reader may have edited or clicked one while the process ran.  A batch
that comes back without its marker between every pair says nothing
about which HTML belongs where, and each doc string is then converted
on its own."
  (let ((overblock-md-command overblock-pydoc-command))
    (when (overblock-md-program)
      (overblock-md-html-batch-async
       (mapcar (lambda (region)
                 (overblock-pydoc--source (car region) (cdr region)))
               regions)
       (lambda (htmls)
         (dolist (region regions)
           (unless (overblock-in (car region) (cdr region) 'pydoc)
             (overblock-pydoc--show (car region) (cdr region)
                                    (pop htmls)))))))))

(defun overblock-pydoc-render-buffer ()
  "Render every doc string of the buffer that wants it.
`overblock-pydoc-renderer' says with what.  This is what
`overblock-live-start' is given, and it is called again whenever the
reader stops."
  (interactive)
  (when-let* ((regions (overblock-pydoc--pending (point-min) (point-max))))
    (if (eq overblock-pydoc-renderer 'fontify)
        (overblock-pydoc--render-fontified regions)
      (overblock-pydoc--render-converted regions))))

;;;; The mode

;;;###autoload
(define-minor-mode overblock-pydoc-mode
  "Render the doc strings of this buffer as documentation.
The doc string point is in shows its source, so it can be edited where
it stands; the rest read as prose.  A click on one puts point in it.

`overblock-pydoc-renderer' says how: a converter and shr, which knows
reStructuredText and lays out a table, or the font lock of
`overblock-pydoc-fontify-mode', which costs no process and leaves every
line where the writer put it."
  :lighter " PyDoc"
  (if overblock-pydoc-mode
      (overblock-live-start 'pydoc
                            #'overblock-pydoc-render-buffer
                            overblock-pydoc-idle)
    (overblock-live-stop)))

(provide 'overblock-pydoc)
;;; overblock-pydoc.el ends here
