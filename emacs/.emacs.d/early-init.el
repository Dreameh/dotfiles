;;; early-init.el -- Early initialization.  -*- lexical-binding: t; -*-
;;; Commentary:
;;
;; Emacs 27 introduces early-init.el, which is run before init.el,
;; before package and UI initialization happens.
;;

;;; Code:
;; Defer gc further back in the startup process
(setq gc-cons-threshold most-positive-fixnum)

;; Not enabling packages at startup for a good reason
(setq package-enable-at-startup nil)

;; prevent un-styled emacs by disabling UI elements early.
;; Faster to disable these here (before they've been initialized)
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)
(when (featurep 'ns)
  (push '(ns-transparent-titlebar . t) default-frame-alist))

(advice-add #'x-apply-session-resources :override #'ignore)
;;; early-init.el ends here
