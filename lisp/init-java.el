;;; init-java.el --- -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package scala-mode)

(use-package groovy-mode
  :when (executable-find "gradle")
  :mode "\\.gradle\\'")

(use-package lsp-java
  :hook (java-mode . lsp-deferred)
  :init
  (after-aproject-change
   (setq lsp-java-workspace-dir (aproject-store-file "workspace")
         lsp-java-workspace-cache-dir (aproject-store-file "workspace-cache")))
  :config
  (setq lsp-java-save-action-organize-imports nil)
  (defun lsp-java--ensure-server (_client callback error-callback _update?)
    (message "Please install `eclipse.jdt.ls` for yourself")))

(provide 'init-java)
;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
;;; init-java.el ends here
