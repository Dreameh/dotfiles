;;; module-pdf.el --- Loads PDF module -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:

(use-package pdf-view
  :straight pdf-tools
  :diminish ( pdf-view-printer-minor-mode)
  :defines pdf-annot-activate-created-annotations
  :mode ("\\.[pP][dD][fF]\\'" . pdf-view-mode)
  :magic ("%PDF" . pdf-view-mode)
  :bind (:map pdf-view-mode-map
              ("C-s" . isearch-forward))
  :init
  ;;(pdf-tools-install t nil t t) ;; FIRST TIME INSTALL USAGE
  ;; (pdf-tools-install)
  (pdf-loader-install)
  ;; Recover last viewed position
  (setq pdf-annot-activate-created-annotations t)
  (when (>= emacs-major-version 26)
    (use-package pdf-view-restore
      :hook (pdf-view-mode . pdf-view-restore-mode)
      :init (setq pdf-view-restore-filename
                  (locate-user-emacs-file ".pdf-view-restore")))))

(provide 'module-pdf)
;;; module-pdf.el ends here
