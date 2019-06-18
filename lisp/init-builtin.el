;;; init-builtin.el --- -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(defalias 'yes-or-no-p 'y-or-n-p)

(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)
(put 'narrow-to-defun 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)

(global-unset-key (kbd "<f1>"))
(global-unset-key (kbd "<f2>"))
(global-unset-key (kbd "<f3>"))
(global-unset-key (kbd "<f4>"))
(global-unset-key (kbd "<f5>"))
(global-unset-key (kbd "<f6>"))
(global-unset-key (kbd "<f7>"))
(global-unset-key (kbd "<f8>"))
(global-unset-key (kbd "<f9>"))
(global-unset-key (kbd "<f10>"))
(global-unset-key (kbd "<f11>"))
(global-unset-key (kbd "<f12>"))

(global-unset-key (kbd "C-z"))
(global-unset-key (kbd "C-x C-z"))
(global-unset-key (kbd "C-SPC"))

;; disable files

(setq create-lockfiles nil
      make-backup-files nil
      auto-save-default nil
      auto-save-list-file-prefix nil)
(when system-is-win
  (setq w32-get-true-file-attributes nil))

;; disable gui

(setq use-dialog-box nil)
(setq use-file-dialog nil)

(blink-cursor-mode 0)
(setq visible-bell nil)
(setq ring-bell-function 'ignore)

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

;; initial gui

(when window-system
  (mouse-wheel-mode t)
  (setq mouse-wheel-scroll-amount '(1
                                    ((shift) . 5)
                                    ((control)))))

(when (fboundp 'electric-pair-mode)
  (add-hook 'after-init-hook 'electric-pair-mode))
(when (fboundp 'electric-indent-mode)
  (add-hook 'after-init-hook 'electric-indent-mode))

(setq default-frame-alist '((vertical-scroll-bars . nil)
                            (tool-bar-lines . 0)
                            (menu-bar-lines . 0)
                            (fullscreen . nil)))
(let ((no-border '(internal-border-width . 0)))
  (add-to-list 'default-frame-alist no-border)
  (add-to-list 'initial-frame-alist no-border))

(when system-is-mac
  (setq ns-pop-up-frames nil)
  (setq mac-command-modifier 'meta)
  (setq mac-option-modifier 'none)
  (set-fontset-font t 'symbol
                    (font-spec :family "Apple Color Emoji") nil 'prepend))

(when (fboundp 'toggle-frame-fullscreen)
  (global-set-key (kbd "C-<f10>") 'toggle-frame-fullscreen))

;; initial editor

(setq-default tab-width 4
              indent-tabs-mode nil)
(global-set-key (kbd "RET") 'newline-and-indent)

(setq inhibit-startup-screen t)
(setq initial-scratch-message "")
(setq-default major-mode 'text-mode)
(setq initial-major-mode 'text-mode)

(cua-selection-mode t)
(delete-selection-mode t)
(add-hook 'after-init-hook 'show-paren-mode)
(add-hook 'after-init-hook 'transient-mark-mode)

(setq-default case-fold-search t
              truncate-lines nil
              truncate-partial-width-windows nil
              indicate-empty-lines t
              column-number-mode t
              shift-select-mode nil
              sentence-end-double-space nil
              mouse-yank-at-point t
              save-interprogram-paste-before-kill t
              scroll-preserve-screen-position 'always
              set-mark-command-repeat-pop t)

(add-hook 'after-init-hook 'global-auto-revert-mode)
(setq global-auto-revert-non-file-buffers t
      auto-revert-verbose nil)
(after-load 'autorevert
  (diminish 'auto-revert-mode))

(setq-default show-trailing-whitespace nil)
(dolist (hook '(prog-mode-hook text-mode-hook conf-mode-hook))
  (add-hook hook (lambda () (setq show-trailing-whitespace t))))

(when (fboundp 'display-line-numbers-mode)
  (setq-default display-line-numbers-width 3)
  (global-set-key (kbd "M-g l") 'display-line-numbers-mode))

(when (fboundp 'global-prettify-symbols-mode)
  (add-hook 'after-init-hook 'global-prettify-symbols-mode))

(when window-system
  (dolist (hook '(prog-mode-hook text-mode-hook conf-mode-hook))
    (add-hook hook 'hl-line-mode)))

;; helper for aproject

(after-aproject-change
 (setq frame-title-format '("Emacs @ " aproject-rootdir))
 (setq save-place-file (aproject-store-file "saved-places"))
 (setq eshell-directory-name (aproject-store-file "eshell/"))
 (setq url-configuration-directory (aproject-store-file "url/")))

;; key binds

(global-set-key (kbd "C-.")     'set-mark-command)
(global-set-key (kbd "C-x C-.") 'pop-global-mark)
(global-set-key (kbd "M-g q")   'keyboard-escape-quit)

(global-set-key (kbd "<f1>")    'help-command)
(global-set-key (kbd "C-h")     'delete-backward-char)

(global-set-key (kbd "M-g r")   'replace-string)

(define-key global-map [remap list-buffers] 'ibuffer)

;; fix render slow

(when system-is-win
  (setq inhibit-compacting-font-caches t))
(setq-default  bidi-display-reordering nil)

(provide 'init-builtin)
;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
;;; init-builtin.el ends here
