(setq tab-always-indent 'complete)
(add-to-list 'completion-styles 'initials t)
(setq completion-cycle-threshold 5)

(require-package 'company)
(require-package 'helm-company)

(add-hook 'after-init-hook 'global-company-mode)

(after-load 'company
  (diminish 'company-mode)
  (define-key company-mode-map (kbd "C-:") 'helm-company)
  (define-key company-active-map (kbd "C-:") 'helm-company))

(provide 'init-company)
