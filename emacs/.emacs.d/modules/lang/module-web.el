;;; module-web.el --- Loads web related stuff -*- lexical-binding: t -*-
;;; Commentary:
;; For now all of it is taken from: https://github.com/kkhan01/dotfiles/blob/master/emacs/.emacs.d/config.org#html
;;; Code:
(use-package web-mode
  :mode
  (("\\.html?\\'"       . web-mode)
   ("\\.phtml\\'"       . web-mode)
   ("\\.tpl\\.php\\'"   . web-mode)
   ("\\.blade\\.php\\'" . web-mode)
   ("\\.[agj]sp\\'"     . web-mode)
   ("\\.as[cp]x\\'"     . web-mode)
   ("\\.erb\\'"         . web-mode)
   ("\\.mustache\\'"    . web-mode)
   ("\\.djhtml\\'"      . web-mode)
   ("\\.jsx\\'"         . web-mode)
   ("\\.tsx\\'"         . web-mode))
  :config
  ;; Highlight the element under the cursor.
  (setq-default web-mode-enable-current-element-highlight t)
  ;; built in color for most themes dont work well with my eyes
  (eval-after-load "web-mode"
    '(set-face-background 'web-mode-current-element-highlight-face "LightCoral"))
  :custom
  (web-mode-attr-indent-offset 2)
  (web-mode-block-padding 2)
  (web-mode-css-indent-offset 2)
  (web-mode-code-indent-offset 2)
  (web-mode-comment-style 2)
  (web-mode-enable-current-element-highlight t)
  (web-mode-markup-indent-offset 2))

;; emmet-mode deserves a function to pull up cheatsheet. This is a powerful fork of “zencoding”.
(defun shan/emmet-mode-cheatsheet ()
  "Open emmet mode cheatsheet."
  (interactive)
  (browse-url-generic "https://docs.emmet.io/cheatsheet-a5.pdf"))

(use-package emmet-mode
  :hook
  ((css-mode  . emmet-mode)
   (php-mode  . emmet-mode)
   (sgml-mode . emmet-mode)
   (rjsx-mode . emmet-mode)
   (web-mode  . emmet-mode)))

(use-package typescript-mode
  :hook (typescript-mode . lsp)
  :mode (("\\.ts\\'" . typescript-mode)
         ("\\.tsx\\'" . typescript-mode)))

;;; General JS/TS Projects
;; Makes emacs use the node modules. Especially helpful for versions of tools and for stuff like prettier.
(use-package add-node-modules-path
  :hook ((web-mode . add-node-modules-path)
         (rjsx-mode . add-node-modules-path)))

(use-package prettier-js
  :hook ((js-mode . prettier-js-mode)
         (typescript-mode . prettier-js-mode)
         (rjsx-mode . prettier-js-mode)))

(use-package tide
  :after (typescript-mode js2-mode company flycheck)
  :hook
  (((js2-mode . typescript-mode) . tide-setup)
   ((js2-mode . typescript-mode) . tide-hl-identifier-mode)
   (before-save . tide-format-before-save))
  :config
  (flycheck-add-next-checker 'typescript-tide 'javascript-eslint)
  (flycheck-add-next-checker 'tsx-tide 'javascript-eslint))


;; React
(use-package rjsx-mode
  :mode
  (("\\.js\\'"   . rjsx-mode)
   ("\\.jsx\\'"  . rjsx-mode)
   ("\\.json\\'" . javascript-mode))
  :magic ("/\\*\\* @jsx React\\.DOM \\*/" "^import React")
  :init
  (setq-default rjsx-basic-offset 2)
  (setq-default rjsx-global-externs '("module" "require" "assert" "setTimeout" "clearTimeout" "setInterval" "clearInterval" "location" "__dirname" "console" "JSON")))

(provide 'module-web)
;;; module-web.el ends here
