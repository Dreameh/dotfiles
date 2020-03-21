;;; module-java.el --- Loads java -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:

(use-package lsp-java
  :after (lsp-mode)
  :hook (java-mode . lsp-deferred)
  :config
  (require 'dap-java))

;; For gradle build files
(straight-use-package 'groovy-mode)


(defun build-and-run()
  (interactive)
  (gradle-run "build run"))

;; Gradle
(use-package gradle-mode
  :bind ("C-c C-r" . build-and-run)
  :hook (java-mode . (lambda () (gradle-mode 1))))

(straight-use-package 'mvn)

(provide 'module-java)
;;; module-java.el ends here
