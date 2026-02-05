;;; my-mode-line.el --- Mode line customizations. -*- lexical-binding: t -*-

;; SPDX-License-Identifier: MIT
;; Author: Shay Elkin <shay@elkin.io>
;; Package-Requires: ((use-pacakge "2.4.6") (mood-line "3.1.1") (nyan-mode "1.1.3"))

;; This file is not part of GNU Emacs.

;;; Commentary:

;; My most extenstive customization to Emacs: an even more compact mood-line, with
;; nyan-cat scroll bar.

;;; Code:

(defun my/add-flymake-menu (str)
  (when str
    (propertize str
                'help-echo "mouse-1: List all problems\nmouse-3: Flymake menu"
                'local-map (let ((map (make-sparse-keymap)))
                             (define-key map [mode-line down-mouse-1] 'flymake-show-buffer-diagnostics)
                             (define-key map [mode-line down-mouse-3] flymake-menu)
                             map))))

(defun my/mood-line-segment-checker-format (status error warning note)
  "Format STATUS into a segment string with ERROR, WARNING, and NOTE counts."
  (my/add-flymake-menu
   (pcase status
     ('running
      (format #("%s" 0 2 (face mood-line-status-neutral))
              (mood-line--get-glyph :checker-checking)))
     ('errored
      (format #("%s" 0 2 (face mood-line-status-error))
              (mood-line--get-glyph :checker-errored)))
     ('interrupted
      (format #("%s" 0 2 (face mood-line-status-neutral))
              (mood-line--get-glyph :checker-interrupted)))
     ('finished
      (if (> (+ error warning note) 0)
          (format #("%s %s %s"
                    0 2 (face mood-line-status-error)
                    3 5 (face mood-line-status-warning)
                    6 8 (face mood-line-status-info))
                  error warning note)
        (format #("%s" 0 2 (face mood-line-status-neutral))
                (mood-line--get-glyph :checker-good)))))))

(defun my/mood-line-segment-vc-propertize (&rest _)
  "Advice after `mood-line-segment-vc--update'."
  (when-let* ((vc-str mood-line-segment-vc--text))
    (setq mood-line-segment-vc--text (propertize vc-str
                                                 'help-echo "mouse-1: Version control menu"
                                                 'local-map vc-mode-line-map))))

(defun my/mood-line-segment-encoding ()
  "Return the name of the coding system of the current buffer."
  (when buffer-file-coding-system
    (let ((coding-system (coding-system-plist buffer-file-coding-system)))
      (unless (memq (plist-get coding-system :category)
                    '(coding-category-undecided coding-category-utf-8))
        (propertize (upcase (symbol-name (plist-get coding-system :name)))
                    'face 'mood-line-status-warning)))))

(defun my/modeline-width (str)
  "Return pixel width of STR when rendered with the mode-line face."
  (let ((copy (copy-sequence str)))
    ;; Append mode-line as base face to preserving existing face properties.
    (add-face-text-property 0 (length copy) 'mode-line t copy)
    (string-pixel-width copy)))

(defconst nyan-bar-unit-px 8)

(defun my/mood-line-process-format (oldfun format)
  "Advice for `mood-line--process-format'."
  (if (not (display-graphic-p))
      (funcall oldfun format)
    (let* ((left-str (mood-line--process-segments (car format)))
           (right-str (mood-line--process-segments (cadr format)))
           (left-px (my/modeline-width left-str))
           (right-px (my/modeline-width right-str))
           (empty-px (- (window-pixel-width) left-px right-px)))
      (setq-local nyan-bar-length (1- (/ empty-px nyan-bar-unit-px)))
      (list left-str
            (propertize " " 'display `((space :width
                                              (,(- empty-px
                                                   (* nyan-bar-unit-px
                                                      nyan-bar-length))))))
            (nyan-create)
            right-str))))

(use-package nyan-mode
  :custom (nyan-minimum-window-width 0))

(use-package mood-line
  :after nyan-mode
  :custom-face
  (mood-line-buffer-status-modified ((t (:inherit error :weight bold))))
  (mood-line-major-mode ((t (:weight normal))))
  :custom
  (mood-line-glyph-alist '((:checker-good . ?✔)
                           (:buffer-modified . ?・)))
  (mood-line-format
        (mood-line-defformat
         :left
         (((mood-line-segment-vc) . " ")
          (or (mood-line-segment-buffer-status) (mood-line-segment-client) " ")
          (format-mode-line "%b "
                            ;; Color the whole buffer name if modified.
                            (if (buffer-modified-p)
                                'mood-line-buffer-status-modified
                              'mood-line-buffer-name))
          (propertize (format-mode-line "%l:%c") 'display '(min-width (12.0))))
         :right
         (" "
          ((when indent-tabs-mode #("TAB" 0 3 (face mood-line-status-warning))) . " ")
          ((my/mood-line-segment-encoding) . " ")

          ((propertize (format-mode-line mode-name)
                       'help-echo "mouse-1: Display major mode menu\n\
mouse-2: Show help for major mode\n\
mouse-3: Toggle minor modes"
                       'local-map mode-line-major-mode-keymap) . " ")
          ((mood-line-segment-misc-info)   . " ")
          ((mood-line-segment-process)     . " ")
          ((mood-line-segment-checker)     . " "))))
  :config
  (advice-add 'mood-line--process-format :around #'my/mood-line-process-format)
  ;; Done as advice and not as a call in `mood-line-format' to utilize the caching in mood-line.
  (advice-add 'mood-line-segment-checker--format-status :override #'my/mood-line-segment-checker-format)
  (advice-add 'mood-line-segment-vc--update :after #'my/mood-line-segment-vc-propertize)
  (mood-line-mode))

;; (use-package mood-line-scroll-indicator
;;   :load-path (lambda () (expand-file-name "mood-line-scroll-indicator" elisp-src-dir))
;;   :after mood-line
;;   :config (mood-line-scroll-indicator-mode))

(provide 'my-mode-line)
;;; my-mode-line.el ends here
