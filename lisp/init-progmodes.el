;;; init-progmodes.el --- -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; php

(require-package 'php-mode)
(with-eval-after-load 'php-mode
  (define-key php-mode-map (kbd "C-.") nil))

;; python

(setq auto-mode-alist
      (append '(("SConstruct\\'" . python-mode)
                ("SConscript\\'" . python-mode))
              auto-mode-alist))

(when (executable-find "autopep8")
  (require-package 'py-autopep8)

  (defun python-mode-beautify ()
    "Deep indent for python."
    (py-autopep8)
    (when system-is-win
      (goto-char (point-min))
      (while (search-forward "\r" nil t) (replace-match ""))))
  (add-to-list 'buffer-beautify-alist '(python-mode . python-mode-beautify)))

;; golang

(when (executable-find "go")
  (require-package 'go-mode)
  (add-hook 'before-save-hook 'gofmt-before-save)
  (after-aproject-change (setenv "GOPATH" aproject-rootdir)))

(provide 'init-progmodes)
;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
;;; init-progmodes.el ends here
