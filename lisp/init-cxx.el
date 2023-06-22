;;; init-cxx.el --- -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'google-c-style)

(add-hook 'c-mode-hook 'google-set-c-style)
(add-hook 'c++-mode-hook 'google-set-c-style)

(when (executable-find "clangd")
  (add-hook 'c-mode-hook 'eglot-ensure)
  (add-hook 'c++-mode-hook 'eglot-ensure))

(provide 'init-cxx)
;;; init-cxx.el ends here
