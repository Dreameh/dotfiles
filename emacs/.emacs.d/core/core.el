;;; core.el --- Loads core modules -*- lexical-binding: t -*-
;;; Commentary:
;; Loads all the core modules
;; https://github.com/Lgneous/emacs.d/blob/rewrite/core/core.el
;;; Code:

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

(provide 'core)
;;; core.el ends here
