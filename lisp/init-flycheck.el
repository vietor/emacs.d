;;; init-flycheck.el --- -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require-package 'flycheck)
(add-hook 'after-init-hook 'global-flycheck-mode)
(after-load 'flycheck
  (setq flycheck-display-errors-function #'flycheck-display-error-messages-unless-error-list))

(require-package 'flycheck-color-mode-line)
(add-hook 'flycheck-mode-hook 'flycheck-color-mode-line-mode)

(provide 'init-flycheck)
;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
;;; init-flycheck.el ends here
