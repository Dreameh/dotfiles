;;; module-markdown.el --- Loads markdown -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:

(use-package markdown-mode
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

(provide 'module-markdown)
;; Local Variables:
;; coding: utf-8
;; End:
;;; module-markdown.el ends here

