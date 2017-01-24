(global-unset-key (kbd "C-z"))
(global-unset-key (kbd "C-SPC"))
(global-set-key (kbd "<f1>") 'help-command)
(global-set-key (kbd "C-h") 'delete-backward-char)

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
(after-load 'grep
  (setq wgrep-enable-key "e")
  (define-key grep-mode-map (kbd "C-x C-s") 'wgrep-save-all-buffers))

;; ag
(when (executable-find "ag")
  (require-package 'ag)
  (require-package 'wgrep-ag)
  (setq-default ag-reuse-window t)
  (setq-default ag-highlight-search t)
  (defun ag-project-root-at-aproject (filepath)
    (if aproject-project aproject-rootdir filepath))
  (setq-default ag-project-root-function 'ag-project-root-at-aproject))

;; heml
(when (maybe-require-package 'helm)
  (require 'helm-config)

  (setq helm-M-x-fuzzy-match t
        helm-buffers-fuzzy-matching t
        helm-recentf-fuzzy-match    t
        helm-semantic-fuzzy-match t
        helm-imenu-fuzzy-match    t)

  (helm-mode 1)
  (diminish 'helm-mode)

  (define-key global-map [remap occur] 'helm-occur)
  (define-key global-map [remap dabbrev-expand] 'helm-dabbrev)
  (define-key global-map [remap find-file] 'helm-find-files)
  (define-key global-map [remap list-buffers] 'helm-buffers-list)
  (define-key global-map [remap bookmark-jump] 'helm-filtered-bookmarks)

  (global-set-key (kbd "M-x")       'helm-M-x)
  (global-set-key (kbd "C-x C-m")   'helm-M-x)
  (global-set-key (kbd "C-x b")     'helm-mini)
  (global-set-key (kbd "M-y")       'helm-show-kill-ring))

(provide 'init-editor-enhance)
