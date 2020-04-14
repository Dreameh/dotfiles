;;; init.el --- Load the full configuration -*- lexical-binding: t -*-
;;; Commentary:

;; This file bootstraps the configuration, which is divided into
;; a number of other files.

;;; Code:
;; A big contributor to startup times is garbage collection. We up the gc
;; threshold to temporarily prevent it from running, and then reset it later
;; using a hook.
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6)

;; Keep a ref to the actual file-name-handler
(defvar default-file-name-handler-alist file-name-handler-alist)

;; Set the file-name-handler to nil (because regexing is cpu intensive)
(setq file-name-handler-alist nil)

;; Reset file-name-handler-alist after initialization
(add-hook 'emacs-startup-hook
  (lambda ()
    (setq gc-cons-threshold 16777216
          gc-cons-percentage 0.1
          file-name-handler-alist default-file-name-handler-alist)))

(defun drm/add-string-from-path (dir)
  "Expands directory from DIR."
  (expand-file-name dir user-emacs-directory))

(defun update-load-path ()
  "Update `load-path'."
  (dolist (dir '("core" "modules" "personal" "modules/lang" "modules/tool" "modules/org"))
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
    misc
    settings))

(defvar dreameh--tool-modules
  '(module-lsp
    module-treemacs
    module-pdf))

(defvar dreameh--extra-modules
  '(module-cc
    module-clisp
    module-clojure
    module-org
    module-markdown
    module-python
    module-java
    module-rust
    module-web))

(dolist (module-core dreameh--core-modules)
  (require module-core))

(dolist (module-tool dreameh--tool-modules)
  (require module-tool))

(dolist (module-extra dreameh--extra-modules)
  (require module-extra))

(setq-default  frame-title-format '("Dreamomacs" " - " "%b"))

(provide 'init)
;;; init.el ends here
