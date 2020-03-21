;;; init.el --- Load the full configuration -*- lexical-binding: t -*-
;;; Commentary:

;; This file bootstraps the configuration, which is divided into
;; a number of other files.

;;; Code:
;; Always prefer newer byte-code
(setq load-prefer-newer t)

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

;; Font
(set-face-attribute 'default nil :font "Source Code Pro" :height 100)

;; Encoding
(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

;; Yeet the custom file to the magical land of /dev/zero
(if(string-equal system-type "windows-nt")
    (defconst custom-file "~/custom-file")
  (defconst custom-file "/dev/zero"))

(defvar dreameh--core-modules
  '(keybinds
    packages
    optimization
    ui-frames
    themes
    text
    file-manage
    git-features))

(dolist (module dreameh--core-modules)
  (require module))

(defvar dreameh--extra-modules
  '(module-lsp
    module-treemacs
    module-cc
    module-clisp
    module-clojure
    module-org
    module-markdown
    module-python
    module-web))

(dolist (module dreameh--extra-modules)
  (require module))

(require 'change-tabs)

(global-set-key (kbd "M-<tab>") 'change-tab-to-spaces)

(provide 'init)
;;; init.el ends here
