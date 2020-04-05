;;; module-clisp.el --- Loads clisp -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:

;; (use-package slime-company
;;   :after (company))

;; (defun dreameh-start-slime()
;;   "Start slime when opening a Lisp file."
;;   (unless (slime-connected-p)
;;     (save-excursion (slime))))

;; (use-package slime
;;   :bind (:map slime-mode-map
;;   ("C-c C-e" . slime-load-file)
;;   ("C-c C-k" . slime-compile-and-load-file)
;;   ("TAB"     . slime-indent-and-complete-symbol)
;;   ("C-c i"   . slime-inspect)
;;   ("C-c C-s" . slime-selector))
;;   :hook
;;   (text-mode . turn-on-auto-fill)
;;   (lisp-mode . turn-on-auto-fill)
;;   (slime-mode . dreameh-start-slime)
;;   :custom
;;   (slime-contribs '(slime-fancy slime-company slime-banner))
;;   (inferior-lisp-program "sbcl"))

(defvar inferior-lisp-program "sbcl")

(defun dreameh-start-sly()
  "Start sly when opening a Lisp file."
  (unless (sly-connected-p)
    (save-excursion (sly))))

(use-package sly
  :hook
  (text-mode . turn-on-auto-fill)
  (lisp-mode . turn-on-auto-fill)
  (sly-mode . dreameh-start-sly)
  :bind (:map sly-mode-map
	      ("C-c C-e" . sly-load-file)
	      ("C-c C-k" . sly-compile-and-load-file)
	      ("TAB"     . sly-mrepl-indent-and-complete-symbol)
	      ("C-c i"   . sly-inspect)
	      ("C-c C-s" . sly-selector))
  :config
  (sly-setup '(sly-fancy))
  (setq sly-autodoc-use-multiline t
	sly-complete-symbol*-fancy t
	sly-kill-without-query-p t
	sly-repl-history-remove-duplicates t
	sly-repl-history-trim-whitespaces t
	sly-net-coding-system 'utf-8-unix))

(use-package sly-macrostep
  :after sly)
(use-package sly-repl-ansi-color
  :config (push 'sly-repl-ansi-color sly-contribs))

(use-package sly-quicklisp
  :config
  (push 'sly-quicklisp sly-contribs))

(use-package sly-asdf
  :config (push 'sly-asdf sly-contribs))

(provide 'module-clisp)
;;; module-clisp.el ends here
