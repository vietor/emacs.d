;;; init-flycheck.el --- -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package flycheck
  :init
  (add-hook 'after-init-hook 'global-flycheck-mode)
  :config
  (setq flycheck-display-errors-function #'flycheck-display-error-messages-unless-error-list))

(use-package flycheck-color-mode-line
  :after flycheck
  :init
  (add-hook 'flycheck-mode-hook 'flycheck-color-mode-line-mode))

(provide 'init-flycheck)
;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
;;; init-flycheck.el ends here
