;;; init-company.el --- -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(setq tab-always-indent 'complete)
(add-to-list 'completion-styles 'initials t)

(use-package company
  :diminish
  :bind (("M-C-/" . company-complete)
         :map company-mode-map
         ("M-/" . company-complete)
         :map company-active-map
         ("M-/" . company-other-backend)
         ("C-n" . company-select-next)
         ("C-p" . company-select-previous))
  :init
  (add-hook 'after-init-hook 'global-company-mode)
  :config
  (setq company-dabbrev-downcase nil
        company-backends '(company-capf
                           company-files
                           (company-dabbrev-code company-gtags company-keywords)
                           company-dabbrev))
  (setq-default company-dabbrev-other-buffers 'all
                company-tooltip-align-annotations t))

(provide 'init-company)
;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
;;; init-company.el ends here
