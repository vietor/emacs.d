;;; init-builtin.el --- -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(setq confirm-kill-processes nil)
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
(global-unset-key (kbd "C-SPC"))
(global-unset-key (kbd "C-x C-z"))
(global-unset-key (kbd "C-x C-e"))

(bind-key "C-." 'set-mark-command)
(bind-key "C-x C-." 'pop-global-mark)
(bind-key "M-g q" 'keyboard-escape-quit)

(bind-key "<f1>" 'help-command)
(bind-key "C-h" 'delete-backward-char)

(bind-key "M-g j" 'imenu)
(bind-key "M-g r" 'replace-string)

;; disable files

(setq create-lockfiles nil
      make-backup-files nil
      auto-save-default nil
      auto-save-list-file-prefix nil)

;; disable gui

(setq use-dialog-box nil)
(setq use-file-dialog nil)

(blink-cursor-mode 0)
(setq visible-bell nil)
(setq ring-bell-function 'ignore)

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

(when system-is-mac
  (setq ns-pop-up-frames nil)
  (setq mac-command-modifier 'meta)
  (setq mac-option-modifier 'none)
  (set-fontset-font t 'symbol
                    (font-spec :family "Apple Color Emoji") nil 'prepend))

(when (fboundp 'toggle-frame-fullscreen)
  (bind-key "C-<f10>" 'toggle-frame-fullscreen))

(after-aproject-change
 (setq frame-title-format '("Emacs @ " aproject-rootdir)))

;; initial editor

(setq-default tab-width 4
              standard-indent 4
              nxml-child-indent 4
              nxml-attribute-indent 4
              indent-tabs-mode nil)
(bind-key "RET" 'newline-and-indent)

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

(setq-default show-trailing-whitespace nil)
(dolist (hook '(prog-mode-hook text-mode-hook conf-mode-hook))
  (add-hook hook (lambda () (setq show-trailing-whitespace t))))

(when (fboundp 'display-line-numbers-mode)
  (setq-default display-line-numbers-width 3)
  (bind-key "M-g l" 'display-line-numbers-mode))

;; fix some slow

(when system-is-win
  (setq  w32-pipe-read-delay 0
         w32-pipe-buffer-size (* 64 1024)
         w32-get-true-file-attributes nil
         inhibit-compacting-font-caches t))
(setq-default bidi-display-reordering nil)

(use-package eldoc
  :ensure nil
  :diminish)

(use-package whitespace
  :ensure nil
  :bind ("M-g w" . whitespace-mode)
  :init
  (setq-default whitespace-style
                '(face spaces tabs newline space-mark tab-mark newline-mark)))

(use-package autorevert
  :ensure nil
  :diminish
  :hook (after-init . global-auto-revert-mode)
  :init
  (setq global-auto-revert-non-file-buffers t
        auto-revert-verbose nil))

(use-package uniquify
  :ensure nil
  :init
  (setq uniquify-buffer-name-style 'reverse)
  (setq uniquify-separator " • ")
  (setq uniquify-after-kill-buffer-p t)
  (setq uniquify-ignore-buffers-re "^\\*"))

(use-package dired
  :ensure nil
  :bind (("C-x C-j" . dired-jump)
         ("C-x 4 C-j" . dired-jump-other-window))
  :init
  (setq-default dired-dwim-target t)
  (put 'dired-find-alternate-file 'disabled nil))

(use-package ibuffer
  :ensure nil
  :bind ([remap list-buffers] . ibuffer)
  :config
  (fullframe ibuffer ibuffer-quit)
  (setq ibuffer-show-empty-filter-groups nil)
  (setq ibuffer-filter-group-name-face 'font-lock-doc-face)
  (setq ibuffer-saved-filter-groups
        '(("beautify"
           ("Default"
            (name . "^[^\\*]"))
           ("Undefined"
            (name . "^\\*")))))
  (add-hook 'ibuffer-mode-hook
            (lambda ()
              (ibuffer-switch-to-saved-filter-groups "beautify"))))

(provide 'init-builtin)
;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
;;; init-builtin.el ends here
