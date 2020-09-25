;;; module-org-roam.el --- Loads org roam -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:
(use-package org-roam
  :after org
  :hook (after-init . org-roam-mode)
  :bind (:map org-roam-mode-map
	      (("C-c r l" . org-roam)
	       ("C-c r i" . org-roam-jump-to-index)
	       ("C-c n g" . org-roam-graph)
	       ("C-c n f" . org-roam-find-file)
	       ("C-c n d" . org-roam-dailies-today))
	      :map org-mode-map
	      (("C-c n i" . org-roam-insert))
	      (("C-c L" . org-store-link))
	      (("C-c n I" . org-roam-insert-immediate)))
  :config
  (executable-find "sqlite3")
  (setq org-roam-directory "~/Dropbox/Org/Roam/")
  (setq org-roam-completion-system 'ivy)
  (require 'org-roam-protocol)
  (setq org-roam-capture-templates
          '(("d" "default" plain
             (function org-roam-capture--get-point)
             "%?"
             :file-name "%<%Y-%m-%d-%H%M%S>-${slug}"
             :head "#+TITLE: ${title}\n#+CREATED: %U\n#+LAST_MODIFIED: %U\n\n"
             :unnarrowed t))))

(use-package org-roam-server
  :after org-roam
  :config
    (setq org-roam-server-host "127.0.0.1"
        org-roam-server-port 8080
        org-roam-server-authenticate nil
        org-roam-server-export-inline-images t
        org-roam-server-serve-files nil
        org-roam-server-served-file-extensions '("pdf" "mp4" "ogv")
        org-roam-server-network-poll t
        org-roam-server-network-arrows nil
        org-roam-server-network-label-truncate t
        org-roam-server-network-label-truncate-length 60
        org-roam-server-network-label-wrap-length 20)
    (require 'simple-httpd)
    (setq httpd-root "/var/www")
    (httpd-start))

(use-package company-org-roam
  :after org-roam
  :config
  (add-to-list 'company-backends #'company-org-roam))

(use-package deft
  :after org
  :bind ("C-c d d" . deft)
  :custom
  (deft-recursive t)
  (deft-use-filter-string-for-filename t)
  (deft-default-extension "org")
  (deft-directory "~/Dropbox/Org/gkroam"))

(provide 'module-org-roam)
;;; module-org-roam.el ends here
