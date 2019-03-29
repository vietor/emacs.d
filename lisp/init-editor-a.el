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

(setq-default major-mode 'text-mode)
(setq initial-major-mode 'text-mode)

(setq inhibit-startup-screen t)
(setq initial-scratch-message "")

(setq create-lockfiles nil)
(setq make-backup-files nil)
(setq auto-save-default nil)
(setq auto-save-list-file-prefix nil)

(setq use-dialog-box nil)
(setq use-file-dialog nil)
(setq-default
 truncate-lines nil
 truncate-partial-width-windows nil)
(when (fboundp 'menu-bar-mode)
  (menu-bar-mode -1))

(setq visible-bell nil)
(setq ring-bell-function 'ignore)
(setq track-eol nil)
(setq echo-keystrokes 0.1)
(setq-default case-fold-search t)

(when (fboundp 'electric-pair-mode)
  (add-hook 'after-init-hook 'electric-pair-mode))
(when (fboundp 'electric-indent-mode)
  (add-hook 'after-init-hook 'electric-indent-mode))

(setq-default
 column-number-mode t
 shift-select-mode nil
 sentence-end-double-space nil
 indicate-empty-lines t
 scroll-preserve-screen-position 'always
 set-mark-command-repeat-pop t)
(blink-cursor-mode 0)
(delete-selection-mode t)
(add-hook 'after-init-hook 'show-paren-mode)

(setq-default indent-tabs-mode nil)
(global-set-key (kbd "RET") 'newline-and-indent)

(when window-system
  (tooltip-mode -1)
  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  (set-scroll-bar-mode nil)
  (mouse-wheel-mode t)
  (setq mouse-wheel-scroll-amount '(1
                                    ((shift) . 5)
                                    ((control)))))

(setq default-frame-alist '((vertical-scroll-bars . nil)
                            (tool-bar-lines . 0)
                            (menu-bar-lines . 0)
                            (fullscreen . nil)))
(let ((no-border '(internal-border-width . 0)))
  (add-to-list 'default-frame-alist no-border)
  (add-to-list 'initial-frame-alist no-border))

(add-hook 'after-init-hook 'global-auto-revert-mode)
(setq global-auto-revert-non-file-buffers t
      auto-revert-verbose nil)
(after-load 'autorevert
  (diminish 'auto-revert-mode))

(setq-default show-trailing-whitespace nil)
(dolist (hook '(prog-mode-hook text-mode-hook conf-mode-hook))
  (add-hook hook (lambda () (setq show-trailing-whitespace t))))

(when *is-os-win*
  (setq w32-get-true-file-attributes nil)
  (setq inhibit-compacting-font-caches t))

(when *is-os-mac*
  (setq mac-command-modifier 'meta)
  (setq mac-option-modifier 'none)
  (set-fontset-font
   t 'symbol
   (font-spec :family "Apple Color Emoji") nil 'prepend))

(when (fboundp 'toggle-frame-fullscreen)
  (global-set-key (kbd "C-<f10>") 'toggle-frame-fullscreen))

(after-aproject-change
 (setq frame-title-format '("Emacs @ " aproject-rootdir))
 (setq save-place-file (aproject-store-file "saved-places"))
 (setq eshell-directory-name (aproject-store-file "eshell/"))
 (setq url-configuration-directory (aproject-store-file "url/")))

(global-set-key (kbd "C-.")     'set-mark-command)
(global-set-key (kbd "C-x C-.") 'pop-global-mark)
(global-set-key (kbd "M-g q")   'keyboard-escape-quit)

(define-key global-map [remap list-buffers] 'ibuffer)

(provide 'init-editor-a)
