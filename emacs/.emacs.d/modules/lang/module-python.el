;;; module-python.el --- Loads python -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:
(defconst drm/python-executable "python3")

(use-package pip-requirements
  :mode ("requirements\\.txt" . pip-requirements-mode)
  :hook (text-mode . pip-requirements-mode))

(setq lsp-python-ms-executable
      "~/lang-servers/python/output/bin/Release/linux-x64/publish/Microsoft.Python.LanguageServer")

(use-package lsp-python-ms
  :hook (python-mode . lsp-deferred)
  :custom
  (lsp-python-ms-dir (expand-file-name "~/lang-servers/python/output/bin/Release/"))
  (lsp-python-ms-executable "~/lang-servers/python/output/bin/Release/linux-x64/publish/Microsoft.Python.LanguageServer"))

(provide 'module-python)
;;; module-python.el ends here
