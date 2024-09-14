;;; corfu-terminal.el --- Corfu popup on terminal -*- lexical-binding: t; -*-

;; Copyright (C) 2023, 2024 scturtle.
;; Copyright (C) 2022 Akib Azmain Turja.

;; Author: scturtle <scturtle@gmail.com>
;;         Akib Azmain Turja <akib@disroot.org>
;; Package-Requires: ((emacs "26.1") (corfu "0.36"))

;; This file is not part of GNU Emacs.
;; This file is released under GNU GPL.

;;; Commentary:

;;; Code:

(require 'subr-x)
(require 'corfu)
(require 'cl-lib)

(defgroup corfu-terminal nil
  "Corfu popup on terminal."
  :group 'convenience
  :prefix "corfu-terminal-")

(defvar-local corfu-terminal--overlays nil
  "Overlays object.")

(cl-defmethod corfu--popup-support-p (&context (corfu-terminal-mode (eql t)))
  "Return whether corfu-terminal supports showing popon now."
  t)

(cl-defmethod corfu--popup-hide (&context (corfu-terminal-mode (eql t)))
  "Hide popup."
  (while corfu-terminal--overlays
    (delete-overlay (pop corfu-terminal--overlays))))

(cl-defmethod corfu--popup-show (pos off width lines
                                     &context (corfu-terminal-mode (eql t))
                                     &optional curr lo bar)
  "Show popup at OFF columns before POS.
Show LINES, a list of lines.  Highlight CURRth line as current
selection.  Show a vertical scroll bar of size BAR + 1 from LOth line."
  (corfu--popup-hide) ; Hide the popup first.
  (let* ((bar-width 1)
         (scroll-bar (propertize " " 'face 'corfu-bar))
         (margin-right (make-string bar-width ?\ ))
         (popon-width (+ width bar-width))
         (pos (corfu-terminal--x-y-at-pos (if (posnp pos) (posn-point pos) pos)))
         (popon-pos
          (let ((x (max 0 (min (- (car pos) off)
                               (- (window-max-chars-per-line) popon-width))))
                (y (if (and (<= (window-body-height) (+ (cdr pos) (length lines)))
                            (>= (cdr pos) (length lines)))
                       (- (cdr pos) (length lines))
                     (1+ (cdr pos)))))
            (cons x y))))
    (corfu-terminal--render
     (string-join
      (seq-map-indexed
       (lambda (line line-number)
         (let* ((pad (make-string (- width (string-width line)) ?\ ))
                (bar (if (and lo (<= lo line-number (+ lo bar)))
                         scroll-bar margin-right))
                (str (concat line pad bar))
                (face (if (eq line-number curr)
                          'corfu-current 'corfu-default)))
           (add-face-text-property 0 (length str) face t str)
           str))
       lines)
      "\n")
     popon-width
     popon-pos)))

(defun corfu-terminal--x-y-at-pos (point)
  (let ((window-start-x-y (posn-col-row (posn-at-point (window-start))))
        (point-x-y (posn-col-row (posn-at-point point))))
    (cons (if (or (not truncate-lines) word-wrap)
              (- (car point-x-y) (car window-start-x-y))
            (- (save-excursion (goto-char point) (current-column))
               (window-hscroll)))
          (- (cdr point-x-y) (cdr window-start-x-y)))))

(defun corfu-terminal--is-invisible (point)
  (get-char-property point 'invisible))

(defun corfu-terminal--next-invisible (point &optional end)
  (next-single-char-property-change point 'invisible nil (or end (window-end))))

(defun corfu-terminal--buffer-visible-substring (start end)
  (let ((str nil))
    (save-excursion
      (goto-char start)
      (while (< (point) end)
        (let ((next-change (corfu-terminal--next-invisible (point) end)))
          (if (corfu-terminal--is-invisible (point))
              (push "..." str)
            (push (buffer-substring (point) next-change) str))
          (goto-char next-change))))
    (string-join (reverse str))))

(defun corfu-terminal--make-framebuffer (y height)
  "Create a framebuffer for current window and buffer."
  (let ((framebuffer nil))
    (save-excursion
      (goto-char (window-start))
      (let ((mark (point))
            (next-invisible
             (if (corfu-terminal--is-invisible (point))
                 (point)
               (corfu-terminal--next-invisible (point)))))
        (dotimes (i (floor (window-screen-lines)))
          (if truncate-lines (forward-line 1) (vertical-motion 1))
          (when (< next-invisible (point))
            (let ((next-visible (corfu-terminal--next-invisible next-invisible)))
              (setq next-invisible (corfu-terminal--next-invisible next-visible))
              (while (> next-visible (point))
                (if truncate-lines (forward-line 1) (vertical-motion 1)))))
          (let* ((str (corfu-terminal--buffer-visible-substring mark (point)))
                 (disp-str (string-trim-right str "\n"))
                 (end (if (equal str disp-str) (point) (1- (point)))))
            (when (and (<= y i) (< i (+ y height)))
              ;; (line-string start-point end-point)
              (push (list disp-str mark end) framebuffer)))
          (setq mark (point)))))
    (nreverse framebuffer)))

(defun corfu-terminal--render-lines (framebuffer x lines width)
  "Render LINES at column offset X on FRAMEBUFFER."
  (let ((tab-size tab-width)
        (inhibit-read-only t))
    (with-temp-buffer
      (setq-local tab-width tab-size)
      (dolist (bufferline framebuffer)
        (erase-buffer)
        (insert (car bufferline))
        (goto-char (point-min))
        (move-to-column x t)
        (let ((mark (point)))
          (move-to-column (+ x width) t)
          (setf (car bufferline)
                (concat (buffer-substring (point-min) mark)
                        (pop lines)
                        (buffer-substring (point) (point-max))))))))
  framebuffer)

(defun corfu-terminal--make-overlays (framebuffer)
  "Make overlays to display FRAMEBUFFER on window."
  (let ((blocks nil))
    ;; merge buffer lines by original line range
    (dolist (line framebuffer)
      (let ((range (cons (nth 1 line) (nth 2 line))))
        (unless (equal range (caar blocks))
          (push (list range nil) blocks))
        (push (car line) (cadar blocks))))
    ;; for each original line, create a overlay
    (dolist (block blocks)
      (let ((ov (make-overlay (caar block) (cdar block)))
            (items (reverse (cadr block))))
        (push ov corfu-terminal--overlays)
        (overlay-put ov 'window (selected-window))
        (overlay-put ov 'line-prefix "")
        (overlay-put ov 'wrap-prefix "")
        (overlay-put ov 'display (copy-sequence '(space :width (0))))
        ;; force new line when at EOF
        (when (and (= (caar block) (cdar block) (point-max))
                   (not (equal (buffer-substring (1- (point-max)) (point-max)) "\n")))
          (push "" items))
        (let ((text (string-join items "\n")))
          ;; fix overlay showing underlying font face
          (add-face-text-property 0 (length text) 'default 'append text)
          (overlay-put ov 'before-string text))))))

(defun corfu-terminal--render (text width pos)
  (while corfu-terminal--overlays
    (delete-overlay (pop corfu-terminal--overlays)))
  (let* ((lines (split-string text "\n"))
         (x (+ (car pos) (window-hscroll)))
         (y (cdr pos))
         (framebuffer (corfu-terminal--make-framebuffer y (length lines))))
    (corfu-terminal--render-lines framebuffer x lines width)
    (corfu-terminal--make-overlays framebuffer)))

;;;###autoload
(define-minor-mode corfu-terminal-mode
  "Corfu popup on terminal."
  :global t
  :group 'corfu-terminal)

(provide 'corfu-terminal)
;;; corfu-terminal.el ends here
