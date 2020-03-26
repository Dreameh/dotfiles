;;; init.el --- Load the full configuration -*- lexical-binding: t -*-
;;; Commentary:

;; This file bootstraps the configuration, which is divided into
;; a number of other files.

;;; Code:
(defun drm/add-string-from-path (dir)
  "Expands directory from DIR."
  (expand-file-name dir user-emacs-directory))

(defun update-load-path ()
  "Update `load-path'."
  (dolist (dir '("core" "modules" "modules/lang" "modules/tool" "modules/org"))
    (push (drm/add-string-from-path dir) load-path)))

(update-load-path)

(defvar dreameh--core-modules
  '(base
    keybinds
    packages
    optimization
    ui-frames
    themes
    text
    file-manage
    git-features
    misc))

(defvar dreameh--extra-modules
  '(module-lsp
    module-treemacs
    module-cc
    module-clisp
    module-clojure
    module-org
    module-markdown
    module-python
    module-java
    module-web))

(dolist (module-core dreameh--core-modules)
  (require module-core))

(dolist (module-extra dreameh--extra-modules)
  (require module-extra))

(provide 'init)
;;; init.el ends here
