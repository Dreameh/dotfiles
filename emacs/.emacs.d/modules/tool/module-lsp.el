;;; module-lsp.el --- Loads lsp -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:
 (use-package lsp-mode
   :custom
   (lsp-auto-guess-root t)
   (lsp-before-save-edits t)
   (lsp-auto-configure nil)
   (lsp-enable-snippet nil)
   (lsp-prefer-flymake nil)
   :config
   (require 'lsp-clients)
   (setq-default lsp-print-io t))

 (use-package company-lsp
   :after (company lsp)
   :bind (:map lsp-mode-map ("C-." . company-lsp))
   :custom
   (company-lsp-async t)
   (company-lsp-cache-candidates t)
   (company-lsp-enable-snippets nil)
   (company-lsp-enable-recompletion t)
   :config
   (add-to-list 'company-backends #'company-lsp))

 (use-package lsp-ui
   :hook (lsp-mode . lsp-ui-mode)
   :config
   (setq lsp-prefer-flymake nil
         lsp-ui-doc-delay 3.0
	     lsp-ui-sideline-enable t
	     lsp-ui-sideline-show-symbol t)
   :custom (lsp-ui-flycheck-enable t))

 (use-package dap-mode
   :after (lsp)
   :config (dap-mode t) (dap-ui-mode t)
   :bind (:map prog-mode-map
		("C-x s v" . dap-ui-sessions)
		("C-x l v" . dap-ui-locals)))

(provide 'module-lsp)
;;; module-lsp.el ends here
