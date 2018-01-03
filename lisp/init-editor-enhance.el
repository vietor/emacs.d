(global-unset-key (kbd "C-z"))
(global-unset-key (kbd "C-SPC"))
(global-set-key (kbd "<f1>") 'help-command)
(global-set-key (kbd "C-h") 'delete-backward-char)

(when window-system
  (require-package 'dracula-theme)
  (load-theme 'dracula t))

(require-package 'undo-tree)
(global-undo-tree-mode)
(diminish 'undo-tree-mode)

(require-package 'page-break-lines)
(global-page-break-lines-mode)
(diminish 'page-break-lines-mode)

(require-package 'highlight-escape-sequences)
(hes-mode)

(require-package 'whole-line-or-region)
(whole-line-or-region-mode t)
(diminish 'whole-line-or-region-mode)
(make-variable-buffer-local 'whole-line-or-region-mode)

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

;; wgrep
(require-package 'wgrep)
(setq wgrep-enable-key "e")
(setq wgrep-auto-save-buffer t)

;; ag
(when (executable-find "ag")
  (require-package 'ag)
  (require-package 'wgrep-ag)
  (setq-default ag-reuse-window t)
  (setq-default ag-reuse-buffers t)
  (setq-default ag-highlight-search t)
  (defun ag-project-root-at-aproject (filepath)
    (if aproject-project aproject-rootdir filepath))
  (setq-default ag-project-root-function 'ag-project-root-at-aproject))

;; ivy
(when (maybe-require-package 'ivy)
  (require-package 'counsel)

  (setq-default ivy-use-virtual-buffers t
                ivy-virtual-abbreviate 'fullpath
                ivy-count-format ""
                ivy-magic-tilde nil
                ivy-initial-inputs-alist '((man . "^")
                                           (woman . "^")))
  (setq-default counsel-mode-override-describe-bindings t)

  (ivy-mode 1)
  (diminish 'ivy-mode)

  (counsel-mode 1)
  (diminish 'counsel-mode))

(provide 'init-editor-enhance)
