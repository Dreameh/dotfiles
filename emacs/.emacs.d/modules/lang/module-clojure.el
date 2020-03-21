;;; module-clojure.el --- Loads clojure -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:
(straight-use-package 'clojure-mode)

(use-package cider
  :bind
  (:map cider-repl-mode-map
        ("C-l" . cider-repl-clear-buffer))
  :custom
  (cider-print-fn 'fipp)
  (cider-repl-display-help-banner nil)
  (cider-repl-pop-to-buffer-on-connect nil)
  (cider-repl-display-in-current-window nil)
  (cider-font-lock-dynamically t))

(straight-use-package 'elein)

(provide 'module-clojure)
;;; module-clojure.el ends here
