(global-unset-key (kbd "C-z"))
(global-unset-key (kbd "C-SPC"))
(global-set-key (kbd "<f1>") 'help-command)
(global-set-key (kbd "C-h") 'delete-backward-char)

(when window-system
  (require-package 'dracula-theme)
  (load-theme 'dracula t))

(require-package 'highlight-escape-sequences)
(add-hook 'after-init-hook 'hes-mode)

(require-package 'whole-line-or-region)
(add-hook 'after-init-hook 'whole-line-or-region-mode)
(after-load 'whole-line-or-region
  (diminish 'whole-line-or-region-local-mode))

(require-package 'browse-kill-ring)
(setq browse-kill-ring-separator "\f")
(global-set-key (kbd "M-Y") 'browse-kill-ring)
(after-load 'browse-kill-ring
  (define-key browse-kill-ring-mode-map (kbd "C-g") 'browse-kill-ring-quit)
  (define-key browse-kill-ring-mode-map (kbd "M-n") 'browse-kill-ring-forward)
  (define-key browse-kill-ring-mode-map (kbd "M-p") 'browse-kill-ring-previous))

(when (maybe-require-package 'avy)
  (global-set-key (kbd "C-;") 'avy-goto-word-or-subword-1))

(require-package 'move-dup)
(global-set-key [M-up] 'md/move-lines-up)
(global-set-key [M-down] 'md/move-lines-down)
(global-set-key [M-S-up] 'md/move-lines-up)
(global-set-key [M-S-down] 'md/move-lines-down)

(require-package 'expand-region)
(global-set-key (kbd "C-=") 'er/expand-region)

(require-package 'multiple-cursors)
(defun mc/save-lists ())
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-unset-key (kbd "M-<down-mouse-1>"))
(global-set-key (kbd "M-<mouse-1>") 'mc/add-cursor-on-click)

;; ivy
(when (maybe-require-package 'ivy)
  (require-package 'counsel)

  (setq-default ivy-use-virtual-buffers t
                ivy-virtual-abbreviate 'fullpath
                ivy-count-format ""
                ivy-magic-tilde nil
                ivy-dynamic-exhibit-delay-ms 150
                ivy-initial-inputs-alist '((man . "^")
                                           (woman . "^")))
  (setq-default counsel-mode-override-describe-bindings t)

  (add-hook 'after-init-hook 'ivy-mode)
  (after-load 'ivy
    (diminish 'ivy-mode))

  (add-hook 'after-init-hook 'counsel-mode)
  (after-load 'counsel
    (diminish 'counsel-mode)))

(when (maybe-require-package 'smex)
  (after-aproject-change
   (setq smex-save-file (aproject-store-file "smex")))
  (global-set-key [remap execute-extended-command] 'smex))

(provide 'init-editor-b)
