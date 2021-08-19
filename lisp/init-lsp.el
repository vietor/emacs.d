;;; init-lsp.el --- -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package eglot
  :config
  (setq-default eglot-autoshutdown t
                eglot-sync-connect 1
                eglot-connect-timeout 60
                eglot-ignored-server-capabilites '(:hoverProvider
                                                   :documentHighlightProvider))
    (add-hook 'eglot-managed-mode-hook
            (lambda ()
              (flycheck-mode -1)
              (setq company-backends optimized-company-backends)))
  (setq-default eglot-workspace-configuration
                `((:java.format.settings.profile . "GoogleStyle")
                  (:java.format.settings.url . ,(concat "file:///" (expand-file-name "etc/eclipse-java-google-style.xml" user-emacs-directory)))))
  :init
  (before-aproject-change
   (eglot-shutdown-all))
  (add-to-list 'ya-formatter-beautify-minor-alist
               '(eglot--managed-mode . eglot-format-buffer)))

(provide 'init-lsp)
;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
;;; init-lsp.el ends here
