;;; init-progmodes.el --- -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; php

(use-package php-mode
  :bind (:map php-mode-map
              ("C-." . nil)))

;; golang

(use-package go-mode
  :when (executable-find "go")
  :hook (before-save . gofmt-before-save)
  :init
  (after-aproject-change
   (setenv "GOPATH" aproject-rootdir)))

(provide 'init-progmodes)
;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
;;; init-progmodes.el ends here
