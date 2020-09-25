;;; module-gkroam.el --- Loads gkroam -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:
(use-package gkroam
  :straight (:host github
		   :repo "Kinneyzhang/gkroam.el"
		   :branch "master")
  :defer nil
  :init
  (setq gkroam-root-dir "~/Dropbox/Org/gkroam/"
        gkroam-pub-dir "~/Dropbox/Org/gkroam/site/")
  :bind
  (("C-c r G" . gkroam-update-all)
   ("C-c r g" . gkroam-update)
   ("C-c r d" . gkroam-daily)
   ("C-c r f" . gkroam-find)
   ("C-c r e" . gkroam-edit)
   ("C-c r n" . gkroam-smart-new)
   ("C-c r i" . gkroam-insert)
   ("C-c r I" . gkroam-index)
   ("C-c r p" . gkroam-preview)
   ("C-c r v" . gkroam-preview-current)
   ("C-c r t" . gkroam-toggle-brackets))
  :config
  (global-undo-tree-mode))


(provide 'module-gkroam)
;;; module-gkroam.el ends here
