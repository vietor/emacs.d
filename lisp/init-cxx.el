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
;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
;;; init-cxx.el ends here
