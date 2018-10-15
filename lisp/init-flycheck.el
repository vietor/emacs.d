(when (maybe-require-package 'flycheck)
  (add-hook 'after-init-hook 'global-flycheck-mode)
  (setq flycheck-display-errors-function #'flycheck-display-error-messages-unless-error-list)
  (after-load 'flycheck
    (setq flycheck-gcc-language-standard "c++11")
    (setq flycheck-clang-language-standard "c++11"))

  (when (maybe-require-package 'flycheck-color-mode-line)
    (add-hook 'flycheck-mode-hook 'flycheck-color-mode-line-mode)))

(provide 'init-flycheck)
