;;; funcs.el --- Loads lonely functions -*- lexical-binding: t -*-
;;; Commentary:

;; The home for small functions that are used globally.

;;; Code:
(defconst drm/config-path (concat user-emacs-directory "init.el"))

(defun drm/reload ()
    "Reload the configuration file."
    (interactive)
    (load-file drm/config-path))

(defun drm/edit-config ()
   "Open the configuration file in the current buffer."
   (interactive)
   (find-file drm/config-path))

(provide 'funcs)
;; Local Variables:
;; coding: utf-8
;; End:
;;; funcs.el ends here
