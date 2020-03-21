;;; module-python.el --- Loads python -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:
(defconst drm/python-executable "python3")

(use-package pip-requirements
  :mode ("requirements\\.txt" . pip-requirements-mode)
  :hook (text-mode . pip-requirements-mode))

(setq lsp-python-ms-executable
      "~/python-language-server/output/bin/Release/linux-x64/publish/Microsoft.Python.LanguageServer")

(use-package lsp-python-ms
  :hook (python-mode . (lambda ()
                         (require 'lsp-python-ms)
                         (lsp)))
  :defer 0.3
  :custom
  (lsp-python-ms-dir (expand-file-name "~/python-language-server/output/bin/Release/"))
  (lsp-python-ms-executable "~/python-language-server/output/bin/Release/linux-x64/publish/Microsoft.Python.LanguageServer"))

(provide 'module-python)
;;; module-python.el ends here
