;;; init-company.el --- -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(setq tab-always-indent 'complete)
(add-to-list 'completion-styles 'initials t)

(use-package 'company)
(add-hook 'after-init-hook 'global-company-mode)
(after-load 'company
  (dolist (backend '(company-eclim company-semantic))
    (delq backend company-backends))
  (diminish 'company-mode)
  (define-key company-mode-map (kbd "M-/") 'company-complete)
  (define-key company-active-map (kbd "M-/") 'company-other-backend)
  (define-key company-active-map (kbd "C-n") 'company-select-next)
  (define-key company-active-map (kbd "C-p") 'company-select-previous)
  (setq company-dabbrev-downcase nil)
  (setq-default company-dabbrev-other-buffers 'all
                company-tooltip-align-annotations t)
  (global-set-key (kbd "M-C-/") 'company-complete))

(provide 'init-company)
;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
;;; init-company.el ends here
