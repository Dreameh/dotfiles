;;; module-markdown.el --- Loads markdown -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:

;; (use-package markdown-mode
;;   :defer nil
;;   :commands(markdown-mode gfm-mode)
;;   :mode (("README\\.md\\'" . gfm-mode)
;;          ("\\.md\\'" . markdown-mode)
;;          ("\\.markdown\\'" . markdown-mode))
;;   :init (setq markdown-command "pandoc"))

(use-package markdown-mode
  :defer nil
  :mode
  ("\\.\\(md\\|markdown\\)\\'" . markdown-mode))

(use-package markdown-preview-mode
  :if (executable-find "pandoc")
  :after (markdown-mode)
  :custom
  (markdown-command (executable-find "pandoc")))


(provide 'module-markdown)
;; Local Variables:
;; coding: utf-8
;; End:
;;; module-markdown.el ends here

