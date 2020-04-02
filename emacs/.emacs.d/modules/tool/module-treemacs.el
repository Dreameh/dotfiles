;;; module-treemacs.el --- Loads lsp -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:
;; Treemacs
 (use-package treemacs
   :bind (:map prog-mode-map
		("C-x t t" . treemacs)
		("C-x t 1" . treemacs-select-window))
   :config (setq treemacs-resize-icons 14))

 (use-package lsp-treemacs
   :after (lsp-mode treemacs)
   :init (lsp-treemacs-sync-mode 1)
   :bind (:map prog-mode-map
		("C-x e l" . lsp-treemacs-errors-list)
		("C-x s l" . lsp-treemacs-symbols)))

 (use-package treemacs-projectile
   :after treemacs projectile)

 (use-package treemacs-magit
   :after treemacs magit)

 (use-package treemacs-icons-dired
   :after treemacs dired
   :config (treemacs-icons-dired-mode))

(provide 'module-treemacs)
;;; module-treemacs.el ends here
