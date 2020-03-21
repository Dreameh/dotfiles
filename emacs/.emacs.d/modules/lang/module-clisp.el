;;; module-clisp.el --- Loads clisp -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:
(use-package slime-company
  :after (company))

(defun dreameh-start-slime()
  "Start slime when opening a Lisp file."
  (unless (slime-connected-p)
    (save-excursion (slime))))

(use-package slime
  :bind (:map slime-mode-map
  ("C-c C-e" . slime-load-file)
  ("C-c C-k" . slime-compile-and-load-file)
  ("TAB"     . slime-indent-and-complete-symbol)
  ("C-c i"   . slime-inspect)
  ("C-c C-s" . slime-selector))
  :hook
  (text-mode . turn-on-auto-fill)
  (lisp-mode . turn-on-auto-fill)
  (slime-mode . dreameh-start-slime)
  :custom
  (slime-contribs '(slime-fancy slime-company slime-banner))
  (inferior-lisp-program "sbcl"))

;; (defun dreameh-start-slime()
;;   "Start slime when opening a Lisp file."
;;   (unless (slime-connected-p)
;;     (save-excursion (slime))))

;; (add-hook 'slime-mode-hook 'dreameh-start-slime)

(provide 'module-clisp)
;;; module-clisp.el ends here
