;;; optimization.el --- Loads optimizations  -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:

;; Fix GC
;; To speed up minibuffer commands (like helm and ivy), we defer garbage
;; collection while the minibuffer is active.
(defun drm-defer-garbage-collection-h ()
  "Setting gc cons threshold to most positive fixnum."
  (setq gc-cons-threshold most-positive-fixnum))

(defun drm-restore-garbage-collection-h ()
  "Defer it so that commands launched immediately after will enjoy the benefits."
  (run-at-time
   1 nil (lambda () (setq gc-cons-threshold 16777216))))

(add-hook 'minibuffer-setup-hook #'drm-defer-garbage-collection-h)
(add-hook 'minibuffer-exit-hook #'drm-restore-garbage-collection-h)

;; Not restoring these to their defaults will cause stuttering/freezes.
(add-hook 'emacs-startup-hook #'drm-restore-garbage-collection-h)

;; In noninteractive sessions, prioritize non-byte-compiled source files to
;; prevent the use of stale byte-code. Otherwise, it saves us a little IO time
;; to skip the mtime checks on every *.elc file.
(setq load-prefer-newer noninteractive)

;; Scrolling ""might"" lag unless you have this
(setq-default scroll-margin 0
              scroll-conservatively 10000
              scroll-preserve-screen-position t
              mouse-wheel-progressive-speed nil)


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
(setq apropos-do-all t)

;;
;;; Optimizations

;; Disable bidirectional text rendering for a modest performance boost. Of
;; course, this renders Emacs unable to detect/display right-to-left languages
;; (sorry!), but for us left-to-right language speakers/writers, it's a boon.
(setq-default bidi-display-reordering 'left-to-right)

;; Reduce rendering/line scan work for Emacs by not rendering cursors or regions
;; in non-focused windows.
(setq-default cursor-in-non-selected-windows nil)
(setq highlight-nonselected-windows nil)

;; More performant rapid scrolling over unfontified regions. May cause brief
;; spells of inaccurate fontification immediately after scrolling.
(setq fast-but-imprecise-scrolling t)

;; Resizing the Emacs frame can be a terribly expensive part of changing the
;; font. By inhibiting this, we halve startup times, particularly when we use
;; fonts that are larger than the system default (which would resize the frame).
(setq frame-inhibit-implied-resize t)

;; Don't ping things that look like domain names.
(setq ffap-machine-p-known 'reject)

;; This is literary a package that makes the emacs directory be more organized
(use-package no-littering
  :straight t)

(provide 'optimization)
;;; optimization.el ends here
