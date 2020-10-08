;;; early-init.el --- -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(setq read-process-output-max (* 1024 1024)
      gc-cons-threshold most-positive-fixnum)

(setq package-enable-at-startup nil)
(advice-add #'package--ensure-init-file :override #'ignore)

;; before initial gui

(when (fboundp 'tooltip-mode)
  (tooltip-mode -1))
(when (fboundp 'menu-bar-mode)
  (menu-bar-mode -1))
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))
(when (fboundp 'horizontal-scroll-bar-mode)
  (horizontal-scroll-bar-mode -1))

(setq default-frame-alist '((vertical-scroll-bars . nil)
                            (tool-bar-lines . 0)
                            (menu-bar-lines . 0)
                            (fullscreen . nil)))
(let ((no-border '(internal-border-width . 0)))
  (add-to-list 'default-frame-alist no-border)
  (add-to-list 'initial-frame-alist no-border))

(setq frame-inhibit-implied-resize t)
(advice-add #'x-apply-session-resources :override #'ignore)

(provide 'early-init)
;; Local Variables:
;; coding: utf-8
;; no-byte-compile: t
;; End:
;;; early-init.el ends here
