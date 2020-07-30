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
   (setq lsp-java-workspace-cache-dir (aproject-store-file "workspace-cache/")))
  :config
  (setq lsp-java-save-action-organize-imports nil
        lsp-java-autobuild-enabled nil
        lsp-java-format-settings-url "https://raw.githubusercontent.com/google/styleguide/gh-pages/eclipse-java-google-style.xml"
        lsp-java-format-settings-profile "GoogleStyle"))

(provide 'init-java)
;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
;;; init-java.el ends here
