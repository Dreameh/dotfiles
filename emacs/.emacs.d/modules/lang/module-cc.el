;;; module-cc.el --- Loads C/C++ -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:
(use-package company-c-headers
  :after (company)
  :config (add-to-list 'company-backends 'company-c-headers))

(setq auto-mode-alist
     (append '(("\\.C\\'" . c++-mode)
               ("\\.cc\\'" . c++-mode)
               ("\\.cpp\\'" . c++-mode)
               ("\\.c\\'" . c-mode)
               ("\\.h\\'" . c++-mode)
               ("\\.hh\\'" . c++-mode)
               ("\\.hpp\\'" . c++-mode))
               auto-mode-alist))

(use-package cc-mode
  :ensure nil
  :custom
  (ccls-sem-highlight-method 'font-lock)
  (c-basic-offset 2))

(setq c-default-style '((c++-mode  . "stroustrup")
                        (awk-mode  . "awk")
                        (java-mode . "java")
                        (other     . "k&r")))

(setq c-doc-comment-style '((c-mode    . javadoc)
                            (java-mode . javadoc)
                            (pike-mode . javadoc)))

(use-package clang-format
  :bind (("C-M-<tab>" . clang-format-region)))

(use-package ccls
  :defer nil
  :after (lsp-mode)
  :hook ((c-mode c++-mode) . lsp)
  :config
  (setq ccls-executable "/usr/bin/ccls")
  (setq ccls-initialization-options '(:index (:comments 2) :completion (:detailedLabel t)))
  (setq ccls-sem-highlight-method 'font-lock))

(use-package modern-cpp-font-lock
  :hook (c++-mode . modern-c++-font-lock-mode))

(use-package meson-mode
  :hook (meson-mode . company-mode))

;; (use-package google-c-style
;;   :hook
;;   ((c-mode c++-mode) . google-set-c-style)
;;   (c-mode-common . google-make-newline-indent))

(provide 'module-cc)
;; Local Variables:
;; coding: utf-8
;; End:
;;; module-cc.el ends here
