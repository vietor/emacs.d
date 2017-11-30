(setq tab-always-indent 'complete)
(add-to-list 'completion-styles 'initials t)
(setq completion-cycle-threshold 5)

(require-package 'company)
(after-load 'company
  (diminish 'company-mode))
(add-hook 'after-init-hook 'global-company-mode)

(provide 'init-company)
