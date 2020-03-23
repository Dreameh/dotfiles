;;; init.el --- Load the full configuration -*- lexical-binding: t -*-
;;; Commentary:

;; This file bootstraps the configuration, which is divided into
;; a number of other files.

;;; Code:
(defun drm/add-string-from-path (dir)
  "Expands directory from DIR."
  (expand-file-name dir user-emacs-directory))

;; ;; Load path
;; ;; Optimize: Force "modules" and "core" at the head to reduce the startup time.
(defun update-load-path ()
  "Update `load-path'."
  (dolist (dir '("core" "modules"))
    (push (drm/add-string-from-path dir) load-path)))

(defun add-subdirs-to-load-path ()
  "Add subdirectories to `load-path'."
  (let ((default-directory (drm/add-string-from-path "modules")))
    (normal-top-level-add-subdirs-to-load-path)))

(update-load-path)
(add-subdirs-to-load-path)

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

(dolist (module dreameh--core-modules)
  (require module))

(dolist (module dreameh--extra-modules)
  (require module))

(provide 'init)
;;; init.el ends here
