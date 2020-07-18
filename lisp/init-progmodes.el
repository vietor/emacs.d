;;; init-progmodes.el --- -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; php

(use-package php-mode
  :bind (:map php-mode-map
              ("C-." . nil)))

;; python

(use-package python
  :mode (("SConstruct\\'" . python-mode)
         ("SConscript\\'" . python-mode)))

(use-package pip-requirements)

(use-package py-autopep8
  :when (executable-find "autopep8")
  :config
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
  :commands (gofmt-before-save)
  :init
  (add-hook 'before-save-hook 'gofmt-before-save)
  :config
  (after-aproject-change (setenv "GOPATH" aproject-rootdir)))

(provide 'init-progmodes)
;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
;;; init-progmodes.el ends here
