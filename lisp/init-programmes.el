;;; init-programmes.el --- -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; php

(when (and (executable-find "php")
           (maybe-require-package 'php-mode))

  (after-load 'php-mode
    (define-key php-mode-map (kbd "C-.") nil)))

;; python

(when (executable-find "python")
  (require-package 'py-autopep8)

  (setq auto-mode-alist
        (append '(("SConstruct\\'" . python-mode)
                  ("SConscript\\'" . python-mode))
                auto-mode-alist))

  (defun python-deep-buffer-indent ()
    "Deep indent for python."
    (py-autopep8)
    (when *is-os-win*
      (goto-char (point-min))
      (while (search-forward "\r" nil t) (replace-match ""))))

  (add-hook 'python-mode-hook
            (lambda ()
              (setq deep-buffer-indent-function 'python-deep-buffer-indent))))

;; golang

(when (executable-find "go")
  (require-package 'go-mode)

  (after-aproject-change
   (setenv "GOPATH" aproject-rootdir))
  (add-hook 'before-save-hook 'gofmt-before-save))

(provide 'init-programmes)
;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
;;; init-programmes.el ends here
