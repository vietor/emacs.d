;;; init-company.el --- -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(setq tab-always-indent 'complete)
(add-to-list 'completion-styles 'initials t)

(require-package 'company)
(add-hook 'after-init-hook 'global-company-mode)
(global-set-key (kbd "M-C-/") 'company-complete)
(with-eval-after-load 'company
  (setq company-dabbrev-downcase nil
        company-backends '((company-dabbrev-code company-gtags company-keywords)
                           company-files company-dabbrev))
  (diminish 'company-mode)
  (define-key company-mode-map (kbd "M-/") 'company-complete)
  (define-key company-active-map (kbd "M-/") 'company-other-backend)
  (define-key company-active-map (kbd "C-n") 'company-select-next)
  (define-key company-active-map (kbd "C-p") 'company-select-previous)
  (setq-default company-dabbrev-other-buffers 'all
                company-tooltip-align-annotations t))

(provide 'init-company)
;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
;;; init-company.el ends here
