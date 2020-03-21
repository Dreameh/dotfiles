;;; git-features.el --- Loads git -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:
(use-package magit
  :ensure-system-package (git)
  :bind ("C-c g" . magit-status))

(use-package forge)

(use-package git-gutter
  :config (global-git-gutter-mode 't)
  :diminish git-gutter-mode)

(use-package git-gutter-fringe)

(defconst fringe-size '3 "Default fringe width.")

;;; Setting up the fringe
;; switches order of fringe and margin
(setq-default fringes-outside-margins t)

;; standardize fringe width
(fringe-mode fringe-size)
(push `(left-fringe  . ,fringe-size) default-frame-alist)
(push `(right-fringe . ,fringe-size) default-frame-alist)

;; colored fringe "bars"
(define-fringe-bitmap 'git-gutter-fr:added
	  [224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224]
	  nil nil 'center)
(define-fringe-bitmap 'git-gutter-fr:modified
	  [224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224]
	  nil nil 'center)
(define-fringe-bitmap 'git-gutter-fr:deleted
	  [0 0 0 0 0 0 0 0 0 0 0 0 0 128 192 224 240 248]
	  nil nil 'center)

;; Bootstrap
(add-hook 'text-mode 'git-gutter-mode)
(add-hook 'prog-mode 'git-gutter-mode)
(add-hook 'conf-mode 'git-gutter-mode)

(provide 'git-features)

;; Local Variables:
;; coding: utf-8
;; End:
;;; git-features.el ends here
