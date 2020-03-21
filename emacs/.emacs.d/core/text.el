;;; text.el --- Load text-related stuff -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package smartparens
  :hook (prog-mode . smartparens-mode)
  :custom (sp-escape-quotes-after-insert nil)
  :config (require 'smartparens-config))

(setq show-paren-mode t)
(add-hook 'prog-mode-hook 'show-paren-mode)

(use-package move-text
  :config (move-text-default-bindings))

 (setq-default electric-indent-inhibit t)

 (setq backward-delete-char-untabify-method 'hungry)

 (setq whitespace-style '(face tabs tab-mark trailing))
 (custom-set-faces
	  '(whitespace-tab ((t (:foreground "#636363")))))
 (setq whitespace-display-mappings
	 '((tab-mark 9 [124 9] [92 9])))
(global-whitespace-mode)

(global-hl-line-mode t)

(global-prettify-symbols-mode t)

(use-package swiper
  :defer nil
  :config
  (progn
    (ivy-mode 1)
    (setq ivy-use-virtual-buffers t)
    (global-set-key "\C-s" 'swiper)
    (global-set-key (kbd "C-c C-r") 'ivy-resume)
    (global-set-key (kbd "<f6>") 'ivy-resume)
    (global-set-key (kbd "M-x") 'counsel-M-x)
    (global-set-key (kbd "C-x C-f") 'counsel-find-file)
    (global-set-key (kbd "<f1> f") 'counsel-describe-function)
    (global-set-key (kbd "<f1> v") 'counsel-describe-variable)
    (global-set-key (kbd "<f1> l") 'counsel-load-library)
    (global-set-key (kbd "<f2> i") 'counsel-info-lookup-symbol)
    (global-set-key (kbd "<f2> u") 'counsel-unicode-char)
    (global-set-key (kbd "C-c g") 'counsel-git)
    (global-set-key (kbd "C-c j") 'counsel-git-grep)
    (global-set-key (kbd "C-c k") 'counsel-ag)
    (global-set-key (kbd "C-x l") 'counsel-locate)
    (global-set-key (kbd "C-S-o") 'counsel-rhythmbox)
    (define-key read-expression-map (kbd "C-r") 'counsel-expression-history)))


;;; Text Auto completion
(use-package company
  :defer nil
  :hook (after-init . global-company-mode)
  :custom-face
  (company-tooltip ((t (:foreground "#abb2bf" :background "#30343c"))))
  (company-tooltip-annotation ((t (:foreground "#abb2bf" :background "#30343c"))))
  (company-tooltip-selection ((t (:foreground "#abb2bf" :background "#393f49"))))
  (company-tooltip-mouse ((t (:background "#30343c"))))
  (company-tooltip-common ((t (:foreground "#abb2bf" :background "#30343c"))))
  (company-tooltip-common-selection ((t (:foreground "#abb2bf" :background "#393f49"))))
  (company-preview ((t (:background "#30343c"))))
  (company-preview-common ((t (:foreground "#abb2bf" :background "#30343c"))))
  (company-scrollbar-fg ((t (:background "#30343c"))))
  (company-scrollbar-bg ((t (:background "#30343c"))))
  (company-template-field ((t (:foreground "#282c34" :background "#c678dd"))))
  :custom
  (company-require-match 'never)
  (company-dabbrev-downcase nil)
  (company-tooltip-align-annotations t)
  (company-minimum-prefix-length 2))

(use-package company-quickhelp
  :after (company)
  :config (company-quickhelp-mode))

(use-package company-box
  :after (company)
  :hook (company-mode . company-box-mode))

;; Enable flycheck
(use-package flycheck
  :init (global-flycheck-mode)
  (setq-default flycheck-check-syntax-automatically '(save
                                                      idle-change
                                                      mode-enable))
  :diminish flycheck-mode)

(use-package subword
  :init (global-subword-mode)
  :diminish subword-mode)

(use-package editorconfig
  :defer nil
  :config (editorconfig-mode 1))

(provide 'text)

;; Local Variables:
;; coding: utf-8
;; End:
;;; text.el ends here
