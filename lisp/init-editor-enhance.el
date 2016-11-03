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

;; heml
(require-package 'helm)
(require 'helm-config)

(helm-mode 1)
(diminish 'helm-mode)
(define-key global-map [remap occur] 'helm-occur)
(define-key global-map [remap dabbrev-expand] 'helm-dabbrev)
(define-key global-map [remap find-file] 'helm-find-files)
(define-key global-map [remap list-buffers] 'helm-buffers-list)
(define-key global-map [remap bookmark-jump] 'helm-filtered-bookmarks)

(global-set-key (kbd "M-x")       'helm-M-x)
(global-set-key (kbd "C-x C-m")   'helm-M-x)
(global-set-key (kbd "M-y")       'helm-show-kill-ring)

(provide 'init-editor-enhance)
