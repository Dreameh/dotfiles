;;; module-pdf.el --- Loads PDF module -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:

(use-package pdf-tools
  :mode ("\\.[pP][dD][fF]\\'" . pdf-view-mode)
  :magic ("%PDF" . pdf-view-mode)
  :diminish (pdf-view-midnight-minor-mode pdf-view-printer-minor-mode)
  :defines pdf-annot-activate-created-annotations
  :bind (:map pdf-view-mode-map
	      ("C-s" . isearch-forward))
  :config
  (require 'pdf-view)
  (setq pdf-annot-activate-created-annotations t)
  (pdf-tools-install)
  ;; Recover last viewed position
  (when (>= emacs-major-version 26)
    (use-package pdf-view-restore
      :hook (pdf-view-mode . pdf-view-restore-mode)
      :init (setq pdf-view-restore-filename
                  (locate-user-emacs-file ".pdf-view-restore")))))

(provide 'module-pdf)
;;; module-pdf.el ends here
