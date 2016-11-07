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
(setq initial-scratch-message "")
(setq inhibit-startup-message t)
(setq inhibit-startup-echo-area-message t)

(setq create-lockfiles nil)
(setq make-backup-files nil)
(setq auto-save-default nil)
(setq auto-save-list-file-prefix nil)

(menu-bar-mode -1)
(setq use-file-dialog nil)
(setq use-dialog-box nil)
(setq-default
  truncate-lines nil
  truncate-partial-width-windows nil)

(setq visible-bell nil)
(setq ring-bell-function 'ignore)
(setq track-eol nil)
(setq echo-keystrokes 0.1)
(setq-default case-fold-search t)

(global-linum-mode 1)
(column-number-mode t)
(global-font-lock-mode 1)
(global-hl-line-mode t)
(when (fboundp 'electric-pair-mode)
  (electric-pair-mode))
(when (fboundp 'electric-indent-mode)
  (electric-indent-mode 1))
(setq-default indent-tabs-mode nil)
(global-set-key (kbd "RET") 'newline-and-indent)

(delete-selection-mode t)
(setq-default
  shift-select-mode nil
  sentence-end-double-space nil
  indicate-empty-lines t
  scroll-preserve-screen-position 'always
  set-mark-command-repeat-pop t)

(show-paren-mode 1)
(setq show-paren-style 'expression)

(when window-system
  (tooltip-mode -1)
  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  (set-scroll-bar-mode nil)
  (set-default 'cursor-type 'bar)
  (mouse-wheel-mode t)
  (setq mouse-yank-at-point t)
  (setq mouse-wheel-scroll-amount '(3))
  (setq mouse-wheel-progressive-speed nil)
  (setq display-time-format "(%m/%d %H:%M)")
  (display-time)
  (load-theme 'deeper-blue' t))

(blink-cursor-mode -1)
(setq default-frame-alist '((vertical-scroll-bars . nil)
                            (tool-bar-lines . 0)
                            (menu-bar-lines . 0)
                            (fullscreen . nil)))
(let ((no-border '(internal-border-width . 0)))
  (add-to-list 'default-frame-alist no-border)
  (add-to-list 'initial-frame-alist no-border))

(global-auto-revert-mode)
(setq global-auto-revert-non-file-buffers t)
(setq auto-revert-verbose nil)

(setq-default show-trailing-whitespace t)
(dolist (hook '(compilation-mode-hook minibuffer-setup-hook))
  (add-hook hook (lambda () (setq show-trailing-whitespace nil))))

(when os-windows
  (setq w32-get-true-file-attributes nil))

(when os-mac
  (setq mac-command-modifier 'meta)
  (setq mac-option-modifier 'none)
  (setq default-input-method "MacOSX")
  (set-fontset-font
    t 'symbol
    (font-spec :family "Apple Color Emoji") nil 'prepend))

(after-aproject-change
  (setq frame-title-format '("Emacs @ " aproject-rootdir))
  (setq save-place-file (aproject-store-file "saved-places"))
  (setq eshell-directory-name (aproject-store-file "eshell/"))
  (setq url-configuration-directory (aproject-store-file "url/")))

(global-set-key (kbd "C-.")     'set-mark-command)
(global-set-key (kbd "C-x C-.") 'pop-global-mark)

(provide 'init-editor)