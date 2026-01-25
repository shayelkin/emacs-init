;;; early-init.el --- Loaded before packages and GUI are initialized. -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

;; Avoid GC during startup.
(setq gc-cons-threshold most-positive-fixnum)
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold 10000000))) ;; Default is 800000

;; Set frame parameters before it is displayed, to avoid a redraw (hiding the
;; toolbar after frame creation takes 0.2s).
(modify-all-frames-parameters
 '((tool-bar-lines . 0)
   (vertical-scroll-bars . nil)))

(provide 'early-init)
;;; early-init.el ends here
