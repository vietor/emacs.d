;;; init-web.el --- -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; html

(use-package 'web-mode)
(add-to-list 'auto-mode-alist '("\\.njk?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(after-load 'web-mode
  (add-to-list 'web-mode-indentation-params '("lineup-args" . nil))
  (add-to-list 'web-mode-indentation-params '("lineup-calls" . nil))
  (add-to-list 'web-mode-indentation-params '("lineup-concats" . nil))
  (add-to-list 'web-mode-indentation-params '("lineup-ternary" . nil)))

(after-load 'company-dabbrev-code
  (add-to-list 'company-dabbrev-code-modes 'web-mode))

;; css

(setq-default css-fontify-colors nil)

(use-package 'sass-mode)
(unless (fboundp 'scss-mode)
  (use-package 'scss-mode))
(setq-default scss-compile-at-save nil)

(provide 'init-web)
;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
;;; init-web.el ends here
