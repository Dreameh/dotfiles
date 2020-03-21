;;; ui-frames.el --- Loads straight -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:
;; Displaying line numbers in prog mode
(use-package display-line-numbers
  :ensure nil
  :if (> emacs-major-version 25.4)
  :hook  (prog-mode . display-line-numbers-mode)
  :custom
  (display-line-numbers-current-absolute t)
  (display-line-numbers-width 2)
  (display-line-numbers-widen t))

(global-auto-revert-mode 1) ; Reload an open file from disk if it is changed outside of Emacs.

;; Remove some of Emacs UI
(setq initial-scratch-message "") ; When opening a new buffer, don't show the scratch message.
(setq inhibit-startup-message t)
(setq inhibit-startup-echo-area-message t)
(setq ring-bell-function 'ignore)

;; By default, you must type "yes" when confirming destructive actions. Change that so only "y" is required.
(defalias 'yes-or-no-p 'y-or-n-p)

;; Listing buffers
(defalias 'list-buffers 'ibuffer-other-window)

;; Enable ace-window
(use-package ace-window
  :init
  (progn
    (global-set-key [remap other-window] 'ace-window)
    (custom-set-faces
     '(aw-leading-char-face
       ((t (:inherit ace-jump-face-foreground :height 3.0))))))
  :diminish ace-window-mode)

;; Disable blinking cursor
(blink-cursor-mode -1)
(setq-default blink-cursor-delay 0)

(provide 'ui-frames)

;; Local Variables:
;; coding: utf-8
;; End:
;;; ui-frames.el ends here
