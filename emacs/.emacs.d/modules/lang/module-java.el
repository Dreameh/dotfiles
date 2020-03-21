;;; module-java.el --- Loads java -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:
(use-package lsp-java
   :requires (lsp)
   :hook (java-mode . lsp)
   :config
   (require 'dap-java))

 ;; For gradle build files
 (straight-use-package 'groovy-mode)

 ;; Gradle
 (use-package gradle-mode
   :hook (java-mode . (lambda () (gradle-mode 1)))
   :config
   (defun build-and-run()
     (interactive)
     (gradle-run "build run"))
   (define-key gradle-mode-map (kbd "C-c C-r") 'build-and-run))

(straight-use-package 'mvn)

(provide 'module-java)
;;; module-java.el ends here
