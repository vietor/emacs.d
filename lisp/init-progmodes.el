;;; init-progmodes.el --- -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; php

(use-package php-mode
  :bind (:map php-mode-map
              ("C-." . nil)))

;; python

(use-package python
  :mode ("SConstruct\\'" "SConscript\\'"))

(use-package pip-requirements)

(use-package py-autopep8
  :when (executable-find "autopep8")
  :init
  (defun python-mode-beautify ()
    "Deep indent for python."
    (py-autopep8-buffer)
    (when system-is-win
      (goto-char (point-min))
      (while (search-forward "\r" nil t) (replace-match ""))))
  (add-to-list 'ya-formatter-beautify-alist '(python-mode . python-mode-beautify)))

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
