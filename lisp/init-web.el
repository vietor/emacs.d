;;; init-web.el --- -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; html

(use-package web-mode
  :mode ("\\.njk?\\'" "\\.html?\\'")
  :config
  (add-to-list 'web-mode-indentation-params '("lineup-args" . nil))
  (add-to-list 'web-mode-indentation-params '("lineup-calls" . nil))
  (add-to-list 'web-mode-indentation-params '("lineup-concats" . nil))
  (add-to-list 'web-mode-indentation-params '("lineup-ternary" . nil)))

;; css

(setq-default css-fontify-colors nil)

(provide 'init-web)
;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
;;; init-web.el ends here
