;;; module-org.el --- Loads org -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:
(use-package org-plus-contrib
  :defer nil
  :bind
  ("C-c a" . org-agenda)
  ("C-c c" . org-capture)
  :config
  (require 'org-tempo)
  (setq org-directory "~/Dropbox/Org/")
  (add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
  (add-to-list 'org-structure-template-alist '("sh" . "src shell"))
  (setq org-confirm-babel-evaluate nil)
  (setq org-src-window-setup 'current-window)
  (setq org-ellipsis "⤵")
  :custom
  (org-export-html-postamble nil)
  (org-image-actual-width 480)
  (org-src-fontify-natively t)
  (org-pretty-entities t)
  (org-hide-emphasis-markers t)
  (org-startup-with-inline-images t)
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (lisp . t)
     (gnuplot . t)
     (js . t)
     (latex . t )
     (org . t)
     (python . t)
     (shell . t))))

(straight-use-package 'gnuplot)
(straight-use-package 'toc-org)

(use-package org-bullets
  :hook (org-mode . org-bullets-mode))

(straight-use-package 'px)
(straight-use-package 'htmlize)
(straight-use-package 'ox-gfm)

(provide 'module-org)
;;; module-org.el ends here
