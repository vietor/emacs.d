;;; init-cxx.el --- -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'google-c-style)

(let ((c-c++-hooks '(c-mode-hook
                     c++-mode-hook
                     c-ts=mode-hook
                     c++-ts-mode-hook)))
  (dolist (hook c-c++-hooks)
    (add-hook hook 'google-set-c-style))

  (when (executable-find "clangd")
    (dolist (hook c-c++-hooks)
      (add-hook hook 'eglot-ensure))))

(provide 'init-cxx)
;;; init-cxx.el ends here
