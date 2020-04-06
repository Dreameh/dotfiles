;;; module-java.el --- Loads java -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:

(use-package lsp-java
  :hook (java-mode . lsp-deferred)
  :config
  (require 'dap-java)
  (setq lsp-java-vmargs
	(list
         "-noverify"
         "-Xmx1G"
         "-XX:+UseG1GC"
         "-XX:+UseStringDeduplication"
         "-javaagent:/home/dreameh/lang-servers/lombok.jar"))
  (require 'lsp-java-boot)

  ;; to enable the lenses
  (add-hook 'lsp-mode-hook #'lsp-lens-mode)
  (add-hook 'java-mode-hook #'lsp-java-boot-lens-mode))

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
