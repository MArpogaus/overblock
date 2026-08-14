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
(load-theme 'modus-vivendi t)
(set-frame-font "Source Code Pro 13" nil t)
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
(defun demo--pan (direction &optional pixels)
  "Scroll one even step per frame in DIRECTION, over PIXELS in total.
With PIXELS nil, scroll up until the window is at the very top."
  (let ((step 26) (moved 0) (guard 0))
    (while (and (< (cl-incf guard) 400)
                (if pixels (< moved pixels)
                  (not (and (= (window-start) (point-min))
                            (zerop (window-vscroll nil t))))))
      (ignore-errors
        (if (eq direction 'down)
            (pixel-scroll-precision-scroll-down step)
          (pixel-scroll-precision-scroll-up step)))
      (cl-incf moved step)
      (redisplay t)
      (demo--snap))))

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
  ;; keep the figure cell at the top, so cell and figure show together
  (demo--log "f%04d figure result" demo--frame)
  (demo--hold 5.0)
  ;; 5. scroll back up through the blocks, one even step per frame
  (demo--log "f%04d scroll up" demo--frame)
  (demo--pan 'up)
  (demo--log "f%04d at top" demo--frame)
  (demo--hold 3.0)
  (demo--log "frames=%d" demo--frame)
  (demo--log "blocks=%d md=%d images=%d"
             (length (pycell--overlays (point-min) (point-max)))
             (length (pycell--overlays (point-min) (point-max) 'pycell-md))
             (length (seq-filter
                      (lambda (o) (and (overlay-get o 'after-string)
                                       (pycell--image (overlay-get o 'after-string))))
                      (overlays-in (point-min) (point-max)))))
  (write-region "" nil "/tmp/demo/done")
  (kill-emacs 0))
(run-with-timer 0.5 nil
                (lambda ()
                  (condition-case err (demo)
                    (error (demo--log "ERROR %S" err)
                           (write-region "" nil "/tmp/demo/failed")))))
