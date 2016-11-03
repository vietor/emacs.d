(setq tab-always-indent 'complete)
(add-to-list 'completion-styles 'initials t)
(setq completion-cycle-threshold 5)

(require-package 'company)
(add-hook 'after-init-hook 'global-company-mode)

(after-load 'company
  (diminish 'company-mode)
  (setq-default company-backends '(company-capf
                                   company-files
                                   (company-dabbrev-code company-gtags company-keywords)
                                   company-dabbrev)))

(provide 'init-company)
