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
  (setq lsp-java-workspace-dir (expand-file-name "java-workspace" user-space-directory))
  (after-aproject-change
   (setq lsp-java-workspace-cache-dir (aproject-store-file "workspace-cache/")))
  :config
  (setq lsp-java-autobuild-enabled nil
        lsp-java-selection-enabled nil
        lsp-java-save-action-organize-imports nil
        lsp-java-import-gradle-wrapper-enabled nil
        lsp-java-format-settings-profile "GoogleStyle"
        lsp-java-format-settings-url (expand-file-name "etc/eclipse-java-google-style.xml" user-emacs-directory)))

(provide 'init-java)
;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
;;; init-java.el ends here
