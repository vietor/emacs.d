;;; init-progmodes.el --- -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; php

(use-package php-mode
  :ensure t
  :bind (:map php-mode-map
              ("C-." . nil)))

;; golang

(use-package go-mode
  :ensure t
  :when (executable-find "go")
  :hook ((go-mode . eglot-ensure)
         (before-save . gofmt-before-save)))

;; rust-lang

(use-package rust-mode
  :ensure t
  :when (executable-find "rust-analyzer")
  :hook (rust-mode . eglot-ensure)
  :config
  (setq rust-format-on-save t))

(provide 'init-progmodes)
;;; init-progmodes.el ends here
