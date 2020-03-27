;;; file-manage.el --- Loads file management packages -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:

;; (use-package all-the-icons-ivy
;;   :defer 0.3
;;   :after ivy
;;   :config
;;   (setq-default all-the-icons-ivy-file-commands (append all-the-icons-ivy-file-commands '(counsel-projectile-find-file counsel-projectile-find-file-dwim)))
;;   (all-the-icons-ivy-setup)
;;   :diminish ivy-mode)

;; Counsel is a completion "framework" using ivy
(use-package counsel)

(use-package ivy-rich
  :after ivy
  :init (ivy-rich-mode 1))

(use-package all-the-icons-ivy-rich
  :after ivy-rich
  :init (all-the-icons-ivy-rich-mode 1))

(use-package avy
  :bind ("M-s" . avy-goto-char))

(use-package projectile
  :defer nil
  :bind (:map projectile-mode-map
              ("C-c p" . projectile-command-map))
  :custom
  (projectile-indexing-method 'hybrid)
  (projectile-sort-order 'access-time)
  (projectile-enable-caching t)
  (projectile-require-project-root t)
  (projectile-completion-system 'ivy)
  :config
  (projectile-mode t)
  :diminish projectile-mode)

(use-package counsel-projectile
  :disabled
  :after (counsel projectile)
  :config
  (counsel-projectile-mode t)
  (defalias 'projectile-switch-to-buffer 'counsel-projectile-switch-to-buffer)
  (defalias 'projectile-find-dir 'counsel-projectile-find-dir)
  (defalias 'projectile-find-file 'counsel-projectile-find-file)
  (defalias 'projectile-grep 'counsel-projectile-grep)
  (defalias 'projectile-switch-project 'counsel-projectile-switch-project))

(use-package which-key
  :defer nil
  :init (which-key-mode +1))

(provide 'file-manage)

;; Local Variables:
;; coding: utf-8
;; End:
;;; file-manage.el ends here
