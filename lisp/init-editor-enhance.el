(global-unset-key (kbd "C-z"))
(global-unset-key (kbd "C-SPC"))
(global-unset-key (kbd "<f3>"))
(global-unset-key (kbd "<f4>"))
(global-unset-key (kbd "<f10>"))
(global-unset-key (kbd "<f11>"))
(global-set-key (kbd "<f1>") 'help-command)
(global-set-key (kbd "C-h") 'delete-backward-char)

(require-package 'undo-tree)
(global-undo-tree-mode)
(diminish 'undo-tree-mode)

(require-package 'nlinum)
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

(require-package 'helm)
(require 'helm-config)

(global-set-key (kbd "M-x")       'helm-M-x)
(global-set-key (kbd "M-y")       'helm-show-kill-ring)
(global-set-key (kbd "C-x b")     'helm-buffers-list)
(global-set-key (kbd "C-x C-f")   'helm-find-files)

(provide 'init-editor-enhance)
