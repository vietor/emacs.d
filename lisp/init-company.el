;;; init-company.el --- -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(setq tab-always-indent 'complete)
(add-to-list 'completion-styles 'initials t)

(use-package company
  :diminish
  :bind (("C-M-/" . company-complete)
         :map company-mode-map
         ("M-/" . company-complete)
         ([remap completion-at-point] . 'company-complete)
         ([remap indent-for-tab-command] . 'company-indent-or-complete-common)
         :map company-active-map
         ("M-/" . company-other-backend)
         ("C-n" . company-select-next)
         ("C-p" . company-select-previous)
         ("C-d" . 'company-show-doc-buffer)
         ("M-." . company-show-location))
  :hook (after-init . global-company-mode)
  :config
  (defconst optimized-company-backends '(company-capf
                                         company-files
                                         (company-dabbrev-code company-gtags company-keywords)
                                         company-dabbrev))
  (setq company-dabbrev-downcase nil
        company-backends optimized-company-backends)
  (setq-default company-dabbrev-other-buffers 'all
                company-tooltip-align-annotations t))

(provide 'init-company)
;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
;;; init-company.el ends here
