;;; packages.el --- Load straight -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:

;; is required by 26.1 and forward
(when (= emacs-major-version 26)
(setq-default gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3"))

(setq-default straight-use-package-by-default t)
;;; Straight initialization
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; making straight-use-package look "better".
(setq-default use-package-always-defer t)
(straight-use-package 'use-package)

(straight-use-package 'use-package-ensure-system-package)

;; And adding el-patch for easier access to github repositories
(straight-use-package 'el-patch)

(provide 'packages)
;;; packages.el ends here
