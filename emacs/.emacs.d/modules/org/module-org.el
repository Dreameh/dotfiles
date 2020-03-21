;;; module-org.el --- Loads org -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:
(use-package org
  :mode
  ("\\.\\(org\\|ORG\\)\\'" . org-mode)
  :hook
  (org-babel-after-execute . org-redisplay-inline-images)
  :custom
  (org-file-apps
   '((auto-mode . emacs)
     ("\\.x?html?\\'" . "/usr/bin/firefox -private-window %s")
     ("\\.pdf\\(::[0-9]+\\)?\\'" . "epdfview %s")))

  (org-directory "~/Documents/org")
  (org-export-html-postamble nil)
  (org-image-actual-width 480)
  (org-src-fontify-natively t)
  (org-src-tab-acts-natively t)
  (org-pretty-entities t)
  (org-hide-emphasis-markers t)
  (org-startup-with-inline-images t)
  (org-babel-python-command "ipython3 -i --simple-prompt")
  :config
  (add-to-list 'org-structure-template-alist
       '("el" "#+BEGIN_SRC emacs-lisp\n?\n#+END_SRC"))
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (lisp . t)
     (gnuplot . t)
     (js . t)
     (latex . t )
     (org . t)
     (python . t)
     (shell . t)
     )))

(use-package toc-org
  :after org
  :hook (org-mode . toc-org-enable))

(use-package org-bullets
  :hook (org-mode . org-bullets-mode))

(use-package px
  :after org)

(use-package htmlize
  :after org)

;; TODO:
(use-package ox-gfm
  :after org)

(provide 'module-org)
;;; module-org.el ends here
