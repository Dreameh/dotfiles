;;; change-tabs.el --- Fixes nice "UI" for changing tabs
;;; Commentary:

;;; Code:
;; Add tab-width
(require 'cl-lib)
(require 'transient)


(setq custom-tab-width 4)

(defun increase-size-of-tabs ()
  "Increase the size of a tab."
  (interactive)
  (cl-incf custom-tab-width 2)
  (message "Curent tab-size: %d" custom-tab-width))

(defun decrease-size-of-tabs ()
  "Decrease the size of a tab."
  (interactive)
  (cl-decf custom-tab-width 2)
  (message "Curent tab-size: %d" custom-tab-width))

 (defun disable-tabs ()
   (interactive)
   (setq indent-tabs-mode nil)
   (message "Tabs: %s" indent-tabs-mode))

 (defun enable-tabs ()
   (interactive)
   (local-set-key (kbd "TAB") 'tab-to-tab-stop)
   (setq indent-tabs-mode t)
   (setq tab-width custom-tab-width)
   (message "Tabs: %s" indent-tabs-mode))

(define-transient-command change-tab-to-spaces ()
  "Change tabs to spaces or back"
  ["Actions"
   ("x" "Disable tabs" disable-tabs)
   ("e" "Enable  tabs" enable-tabs)
   ("i" "Increase size" increase-size-of-tabs)
   ("d" "Decrease size" decrease-size-of-tabs)
   ])

(provide 'change-tabs)
;; Local Variables: change-tab-to-spaces, custom-tab-width
;; coding: utf-8
;; End:
;;; change-tabs.el ends here
