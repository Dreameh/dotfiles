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
(setq load-prefer-newer t)

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

;; Don't ping things that look like domain names.
(setq ffap-machine-p-known 'reject)

(provide 'optimization)
;;; optimization.el ends here
