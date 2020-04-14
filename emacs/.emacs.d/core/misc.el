;;; misc.el --- miscellaneous packages that doesn't really have a home anywhere else.
;;; Commentary:

;;; Code:

(use-package elcord
  :if window-system
  :init (elcord-mode)
  :config
  (setq elcord-use-major-mode-as-main-icon t))

(use-package esup
  :commands (esup))

;; Remember to add personal/settings.el -- wakatime-api-key variable
(use-package wakatime-mode
  :init (global-wakatime-mode)
  :config
  (setq wakatime-cli-path (executable-find "wakatime")))

(provide 'misc)
;; Local Variables:
;; coding: utf-8
;; End:
;;; misc.el ends here
