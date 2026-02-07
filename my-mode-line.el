;;; my-mode-line.el --- Mode line customizations. -*- lexical-binding: t -*-

;; SPDX-License-Identifier: MIT
;; Author: Shay Elkin <shay@elkin.io>
;; Package-Requires: ((emacs "30.1") (nyan-mode "1.1.3"))

;; This file is not part of GNU Emacs.

;;; Commentary:

;; My most extenstive customization to Emacs: a more sparse mode-line, with nyan-mode scroll bar.

;;; Code:

(use-package nyan-mode
  :custom (nyan-minimum-window-width 0))

(setq mode-line-right-align-edge 'right-fringe
      mode-line-percent-position nil
      vc-display-status 'no-backend
      ;; Remove the square brackets around the counters.
      flymake-mode-line-counter-format '(""
                                         flymake-mode-line-error-counter
                                         flymake-mode-line-warning-counter
                                         flymake-mode-line-note-counter " "))

(defvar my/flymake-empty-counters-propertized-str
  `(:propertize ("✔ ")
                help-echo "mouse-1: Check now"
                local-map ,(let ((map (make-sparse-keymap)))
                             (define-key map [mode-line down-mouse-1] 'flymake-start)
                             map)))

(defun my/flymake-mode-line ()
  "Mode line construct for Flymake information."
  (when (bound-and-true-p flymake-mode)
    (let* ((exception (format-mode-line flymake-mode-line-exception))
           (counters (format-mode-line flymake-mode-line-counters))
           ;; Extract the counters and sum them
           (counters-sum (apply #'+ (mapcar #'string-to-number (split-string counters "[^0-9]+" t)))))
      (list
       exception
       (cond ((> counters-sum 0) counters)
             ((length= exception 0) my/flymake-empty-counters-propertized-str))))))

;; The default value for mode-line-buffer-identification is ("%12b"), I want just " %b ".

(defvar my/propertized-buffer-identification
  (car (propertized-buffer-identification " %b ")))

(defface mode-line-buffer-id-modified
  '((t (:inherit (mode-line-buffer-id diff-changed))))
  "Face used for buffer identification in the mode line, when buffer is modified.")

(defvar my/propertized-buffer-identification-modified
  (let ((copy (copy-sequence my/propertized-buffer-identification)))
    (add-face-text-property 1 (1- (length copy)) 'mode-line-buffer-id-modified t copy)
    copy))

(defun my/mode-line-buffer-identification ()
  "Return `mode-line-buffer-identification' propertized in `warning' when buffer is modified."
  (cond
   ((local-variable-p 'mode-line-buffer-identification) mode-line-buffer-identification)
   ((buffer-modified-p) my/propertized-buffer-identification-modified)
   (t my/propertized-buffer-identification)))

(defun string-pixel-width-face (str face)
  "Return pixel width of STR when rendered with in FACE."
  (let ((copy (copy-sequence str)))
    ;; Append as base face to preserving existing face properties in `str'
    (add-face-text-property 0 (length copy) face t copy)
    (string-pixel-width copy)))

(defconst my/nyan-char-width-px 8)

(defvar-local nyan-cache nil)

(defun my/mode-line-middle ()
  (let* ((left-str (format-mode-line my/mode-line-format-left))
         (left-px  (string-pixel-width-face left-str 'mode-line))
         (right-str (format-mode-line my/mode-line-format-right))
         (right-px  (string-pixel-width-face right-str 'mode-line))
         (space-px (- (window-pixel-width) left-px right-px))
         (nyan-bar-length (1- (/ space-px my/nyan-char-width-px)))
         (draw-nyan (and (display-graphic-p) (> nyan-bar-length 3))))
    (list ""
          (if (not draw-nyan) '(-3 "%p")
            (let ((cache-idx (cons nyan-bar-length (point))))
              (when (not (equal cache-idx (car nyan-cache)))
                (setq nyan-cache (cons cache-idx (nyan-create)))))
            (cdr nyan-cache))
          (propertize " " 'display `(space :align-to (- right-fringe (,right-px)))))))

(defvar my/mode-line-format-left
  '(("" mode-line-mule-info mode-line-client mode-line-modified
     mode-line-remote mode-line-window-dedicated)
    (:eval (my/mode-line-buffer-identification))
    (vc-mode vc-mode) "\t"
    mode-line-position))

(defvar my/mode-line-format-right
  `((:eval (when indent-tabs-mode #("TAB " 0 3 (face warning))))
    mode-line-misc-info
    (:propertize ("" mode-name)
                 help-echo "mouse-1: Display major mode menu\n\
mouse-2: Show help for major mode\n\
mouse-3: Toggle minor modes"
                 mouse-face mode-line-highlight
                 local-map ,mode-line-major-mode-keymap)
    " "
    (:eval (my/flymake-mode-line))
    mode-line-process))

(setq-default mode-line-format
              `(,@my/mode-line-format-left
                (:eval (my/mode-line-middle))
                ,@my/mode-line-format-right))

(provide 'my-mode-line)
;;; my-mode-line.el ends here
