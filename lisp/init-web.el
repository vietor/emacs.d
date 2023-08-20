;;; init-web.el --- -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; html

(use-package web-mode
  :ensure t
  :mode ("\\.njk?\\'" "\\.html?\\'")
  :config
  (add-to-list 'web-mode-indentation-params '("lineup-args" . nil))
  (add-to-list 'web-mode-indentation-params '("lineup-calls" . nil))
  (add-to-list 'web-mode-indentation-params '("lineup-concats" . nil))
  (add-to-list 'web-mode-indentation-params '("lineup-ternary" . nil)))

;; css

(setq-default css-fontify-colors nil)

(provide 'init-web)
;;; init-web.el ends here
