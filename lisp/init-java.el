;;; init-java.el --- -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package scala-mode)

(use-package groovy-mode
  :when (executable-find "gradle")
  :mode "\\.gradle\\'")

(use-package java
  :ensure nil
  :hook (java-mode . eglot-ensure))

(provide 'init-java)
;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
;;; init-java.el ends here
