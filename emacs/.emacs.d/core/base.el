;;; base.el --- Loads base config -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:

;; Scrolling ""might"" lag unless you have this
(setq-default scroll-margin 0
              scroll-conservatively 10000
              scroll-preserve-screen-position t
              mouse-wheel-progressive-speed nil)


;; More performant rapid scrolling over unfontified regions. May cause brief
;; spells of inaccurate fontification immediately after scrolling.
(setq fast-but-imprecise-scrolling t)

;; Increase the maximum stack depth (the default is 1000).
(setq max-specpdl-size 2000)

;; Turn off backups and autosaves so we don't have ~ and # files strewn about the working directory. I've
;; tried storing backups in my home directory as suggested by http://stackoverflow.com/q/151945/46237, but
;; still I see the occasional backup file in the working directory for some reason.
(setq make-backup-files nil)
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))
(setq auto-save-default nil)

;; Disable Emacs' write-lock, which creates temporary .#files when saving. This crashes coffeescript --watch.
;; https://github.com/jashkenas/coffeescript/issues/985
(setq create-lockfiles nil)

(setq vc-follow-symlinks t) ; Don't ask confirmation to follow symlinks to edit files.

(setq idle-update-delay 1)
(setq ad-redefinition-action 'accept)
(setq-default apropos-do-all t)


;; Font
(add-to-list 'default-frame-alist '(font . "Source Code Pro-12"))
;; (set-face-attribute 'default t :font "Source Code Pro-12")

;; Encoding
(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

;; Yeet the custom file to the magical land of /dev/zero
(setq custom-file
      (if (memq system-type '(gnu/linux
			      darwin))
	  "/dev/null" "NUL"))

(provide 'base)
;; Local Variables:
;; coding: utf-8
;; End:
;;; base.el ends here
