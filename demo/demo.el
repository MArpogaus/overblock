;; -*- lexical-binding: t; -*-
(setq package-user-dir "/home/marcel/.emacs.d/packages/pycell/.sandbox")
(package-initialize)
(add-to-list 'load-path "/home/marcel/.emacs.d/packages/pycell")
(require 'pycell) (require 'pixel-scroll)

(setq inhibit-startup-screen t
      ring-bell-function #'ignore
      python-shell-interpreter "ipython"
      python-shell-interpreter-args "-i --simple-prompt --no-color-info"
      pycell-markdown-command "pandoc -f gfm -t html"
      pycell-max-lines 14)
(menu-bar-mode -1) (tool-bar-mode -1) (scroll-bar-mode -1)
;; The look of the recording: Source Code Pro where it is installed,
;; and a plain monospace elsewhere.  Not a nerd font — the bars then
;; draw the plain glyphs `pycell--glyph' falls back to, which is what
;; the picture has always shown.
(set-frame-font (seq-find (lambda (font) (find-font (font-spec :name font)))
                          '("Source Code Pro 13" "DejaVu Sans Mono 13"
                            "Liberation Mono 13" "Monospace 13"))
                nil t)
(set-frame-size (selected-frame) 1120 680 t)
(set-frame-position (selected-frame) 0 0)
(setq-default cursor-type 'bar)
(fringe-mode 12)
(blink-cursor-mode -1)

(defvar demo--frame 0)
(defun demo--snap ()
  "Capture one frame.  Every frame is 0.1 s of the animation."
  (cl-incf demo--frame)
  (let ((coding-system-for-write 'binary))
    (write-region (x-export-frames nil 'png) nil
                  (format "/tmp/demo/frames/f%04d.png" demo--frame)
                  nil 'quiet)))
(defun demo--hold (seconds)
  "Show the current state for SECONDS."
  (dotimes (_ (round (* 10 seconds)))
    (redisplay t)
    (demo--snap)
    (sit-for 0.02)))
(defun demo--pan ()
  "Record the way from the current view up to the top.
One frame per scroll step, so the motion in the frames is as even as
the steps that produced it.  Any refusal to scroll stops the pan and
lands in the trace: scrolling up over a rendered cell is exactly what
used to fail."
  (let ((guard 0)
        (make-cursor-line-fully-visible nil))
    ;; The last evaluation left its "Sent: ..." in the echo area, and
    ;; it would sit there for the whole pan.
    (message nil)
    (goto-char (window-start))
    (while (and (< (cl-incf guard) 400) (> (window-start) (point-min)))
      (condition-case err
          (pixel-scroll-precision-scroll-up 26)
        (error (demo--log "pan stopped at line %d: %S"
                          (line-number-at-pos (window-start)) err)
               (setq guard 999)))
      (redisplay t)
      (demo--snap))
    (set-window-start nil (point-min))
    (set-window-vscroll nil 0 t t)
    (goto-char (point-min))
    (redisplay t)
    (demo--snap)))

(defun demo--frame-cell ()
  "Pan until the boundary line of the cell at point is at the top."
  (let ((target (save-excursion (search-backward "# %%") (pos-bol)))
        (guard 0))
    (while (and (< (cl-incf guard) 400) (< (window-start) target))
      (ignore-errors (pixel-scroll-precision-scroll-down 26))
      (redisplay t)
      (demo--snap))
    (set-window-start nil target)
    (redisplay t)))

(defun demo--log (fmt &rest args)
  (write-region (concat (apply #'format fmt args) "\n") nil "/tmp/demo/trace" t 'quiet))

(defun demo--wait (file)
  (while (not (file-exists-p file)) (sit-for 0.2)))

(defun demo--repl ()
  "Start IPython and wait for its first prompt."
  (demo--log "interpreter=%s found=%s" python-shell-interpreter
             (executable-find python-shell-interpreter))
  (save-window-excursion (run-python nil nil nil))
  (let ((tries 0) proc)
    (while (and (< (cl-incf tries) 150) (not (setq proc (python-shell-get-process))))
      (sit-for 0.2))
    (demo--log "process=%s after %d tries" proc tries)
    (when proc
      (with-current-buffer (process-buffer proc)
        (setq tries 0)
        (while (and (< (cl-incf tries) 200)
                    (not (string-match-p "In \\[1\\]" (buffer-string))))
          (sit-for 0.2))
        (demo--log "prompt after %d tries, tail=%S" tries
                   (buffer-substring (max (point-min) (- (point-max) 80)) (point-max))))
      (with-current-buffer (process-buffer proc) (comint-mime-setup))
      (sit-for 3)
      (demo--log "comint-mime ready"))))

(defun demo--type (text)
  "Insert TEXT the way a person types it."
  (dolist (ch (string-to-list text))
    (insert ch) (redisplay t) (demo--snap)))

(defun demo ()
  (demo--log "demo start")
  (find-file "/tmp/demo/demo.py")
  (python-mode)
  (demo--repl)
  (switch-to-buffer "demo.py")
  (delete-other-windows)
  (goto-char (point-min))
  (code-cells-mode 1)          ; pycell-mode comes along and renders
  (pycell-mode -1)             ; ... but start plain, to show the change
  (redisplay t)
  (write-region "" nil "/tmp/demo/ready")
  (demo--wait "/tmp/demo/go")
  (message nil)
  (make-directory "/tmp/demo/frames" t)
  (demo--log "f%04d plain source" demo--frame)
  (demo--hold 3.0)
  ;; 1. markdown cells render in place
  (pycell-mode 1)
  (demo--log "f%04d markdown rendered" demo--frame)
  (demo--hold 5.0)
  ;; 2. evaluate the numpy cell
  (goto-char (point-min))
  (search-forward "import numpy")
  (demo--frame-cell)
  (demo--hold 1.5)
  (demo--log "f%04d eval numpy" demo--frame)
  (call-interactively #'code-cells-eval)
  (demo--hold 6.0)
  ;; 3. a cell that takes its time, so the spinner and the stopwatch show
  (search-forward "import time")
  (demo--frame-cell)
  (demo--log "f%04d eval sleep" demo--frame)
  (demo--hold 1.5)
  (call-interactively #'code-cells-eval)
  (demo--hold 6.5)
  ;; 4. evaluate the figure cell, with room below it for the figure
  (search-forward "fig, ax = plt.subplots")
  (demo--frame-cell)
  (demo--hold 2.0)
  (call-interactively #'code-cells-eval)
  (demo--hold 8.0)
  ;; comint-mime renders the figure most of the time; when the image
  ;; is not there, one more evaluation settles it.
  (unless (seq-some (lambda (o) (and (overlay-get o 'after-string)
                                     (overblock-image-in
                                      (overlay-get o 'after-string))))
                    (overlays-in (point-min) (point-max)))
    (demo--log "no image, evaluating the figure cell again")
    (call-interactively #'code-cells-eval)
    (demo--hold 6.0))
  ;; keep the figure cell at the top, so cell and figure show together
  (demo--log "f%04d figure result" demo--frame)
  (demo--log "python tail: %S"
             (with-current-buffer (process-buffer (python-shell-get-process))
               (buffer-substring-no-properties
                (max (point-min) (- (point-max) 400)) (point-max))))
  (demo--hold 5.0)
  ;; 5. scroll back up through the blocks, one even step per frame
  (demo--log "f%04d scroll up" demo--frame)
  (demo--pan)
  (demo--log "f%04d at top" demo--frame)
  (demo--hold 3.0)
  (demo--log "frames=%d" demo--frame)
  (demo--log "blocks=%d md=%d images=%d"
             (length (overblock-in (point-min) (point-max) 'result))
             (length (overblock-in (point-min) (point-max) 'markdown))
             (length (seq-filter
                      (lambda (o) (and (overlay-get o 'after-string)
                                       (overblock-image-in (overlay-get o 'after-string))))
                      (overlays-in (point-min) (point-max)))))
  (write-region "" nil "/tmp/demo/done")
  (kill-emacs 0))
(run-with-timer 0.5 nil
                (lambda ()
                  (condition-case err (demo)
                    (error (demo--log "ERROR %S" err)
                           (write-region "" nil "/tmp/demo/failed")))))
