;;; module-rust.el --- Loads rust -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:
(use-package flycheck-rust)

(use-package rustic
  :hook
  (rust-mode . lsp-deferred)
  (rust-mode . company-mode)
  (flycheck-mode . flycheck-rust-setup)
  :custom
  (rustic-format-on-save t)
  (rustic-indent-method-chain t)
  ;; The default is 'rls
  (rustic-lsp-server 'rust-analyzer))



(provide 'module-rust)
;; coding: utf-8
;;; module-rust.el ends here
