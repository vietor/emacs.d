;;; early-init.el --- -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(setq read-process-output-max (* 3 1024 1024))

(setq package-check-signature nil
      package-enable-at-startup nil)
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


;; Adjust garbage collection

(defvar gc-idle-timer
  (run-with-idle-timer 60 t #'garbage-collect))

(let ((better-gc-cons-threshold  (* 24 1024 1024))
      (origin-file-name-handler-alist file-name-handler-alist))
  (setq gc-cons-threshold most-positive-fixnum
        file-name-handler-alist nil)

  (add-hook 'emacs-startup-hook
            (lambda ()
              (setq gc-cons-threshold better-gc-cons-threshold
                    file-name-handler-alist origin-file-name-handler-alist)))

  (add-hook 'minibuffer-setup-hook
            (lambda ()
              (setq gc-cons-threshold most-positive-fixnum)))

  (add-hook 'minibuffer-exit-hook
            (lambda ()
              (garbage-collect)
              (setq gc-cons-threshold better-gc-cons-threshold))))

(provide 'early-init)
;;; early-init.el ends here
