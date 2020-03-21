;;; modules.el --- Loads extra modules -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:
(defvar dreameh--extra-modules
  '(module-lsp
    module-treemacs
    module-cc
    module-clisp
    module-clojure
    module-org
    module-markdown
    module-web))

(dolist (module dreameh--extra-modules)
  (require module))

(provide 'modules)
;;; modules.el ends here
