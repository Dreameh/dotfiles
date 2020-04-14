;;; themes.el --- Load theme related things -*- lexical-binding: t -*-
;;; Commentary:

;; This will load doom-modeline and doom themes, as well as dashboard, all to make
;; sure that the UI is clean

;;; Code:

;;
;;; Interface

;; Adding line numbers and column number into the modeline, as well as
;; changing it visually to use the =doom-emacs= modeline, which is just beautiful!
(line-number-mode t)
(column-number-mode t)

;;
;;; Doom Modeline
(use-package doom-modeline
  :defer nil
  :custom
  (doom-modeline-icon t)
  (doom-modeline-major-mode-icon t)
  (doom-modeline-version t)
  (doom-modeline-buffer-file-name-style 'file-name)
  :config
  (doom-modeline-mode))

;; Set custom theme path
(add-to-list 'custom-theme-load-path (expand-file-name "~/.emacs.d/themes/"))

;; Enable doom theme
(use-package doom-themes
  :if window-system
  :custom
  (doom-vibrant-brighter-comments t)
  (doom-vibrant-brighter-modeline t)
  :config
  (setq doom-themes-treemacs-theme "doom-colors")
  (doom-themes-treemacs-config))

(load-theme 'doom-horizon t)
(doom-themes-org-config)

(use-package hide-mode-line
  :hook ((comint-mode help-mode) . hide-mode-line-mode))

(use-package solaire-mode
  :functions persp-load-state-from-file
  :hook
  (prog-mode . turn-on-solaire-mode)
  (minibuffer-setup . solaire-mode-in-minibuffer)
  (after-load-theme . solaire-mode-swap-bg)
  :custom
  (solaire-mode-remap-modeline nil)
  (solaire-mode-remap-fringe nil)
  :config
  (solaire-global-mode 1)
  (solaire-mode-swap-bg)
  (advice-add #'persp-load-state-from-file
  :after #'solaire-mode-restore-persp-mode-buffers))

;;
;;; Dashboard
;;(use-package page-break-lines)

  (defun my/dashboard-banner ()
  """Set a dashboard banner including information on package initialization
   time and garbage collections."""
  (setq dashboard-banner-logo-title
        (format "Emacs ready in %.2f seconds with %d garbage collections."
                (float-time (time-subtract after-init-time before-init-time)) gcs-done)))

(use-package dashboard
  :defer nil
  :if window-system
  :init
  (add-hook 'after-init-hook 'dashboard-refresh-buffer)
  (add-hook 'dashboard-mode-hook 'my/dashboard-banner)
  :config
  (setq dashboard-set-init-info nil
	dashboard-center-content t
	dashboard-page-separator "\n\n"
	dashboard-set-heading-icons t
	dashboard-set-file-icons t
	dashboard-startup-banner 'logo)
    (dashboard-setup-startup-hook))

(provide 'themes)
;; Local Variables:
;; coding: utf-8
;; End:
;;; themes.el ends here
