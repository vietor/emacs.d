(when (maybe-require-package 'flycheck)
  (global-flycheck-mode)
  (setq flycheck-display-errors-function #'flycheck-display-error-messages-unless-error-list)
  (after-load 'flycheck
    (setq flycheck-gcc-language-standard "c++11")
    (setq flycheck-clang-language-standard "c++11")))

(provide 'init-flycheck)
