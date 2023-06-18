;;; init-eglot.el --- -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package eglot
  :demand
  :bind (:map eglot-mode-map
              ("C-c r" . eglot-rename)
              ("C-c o" . eglot-code-actions)
              ("C-c C-r" . eglot-reconnect))
  :init
  (before-aproject-change
   (eglot-shutdown-all))
  (add-to-list 'ya-formatter-beautify-minor-alist
               '(eglot--managed-mode . eglot-format-buffer))
  (when system-is-win
    (advice-add #'eglot--apply-text-edits :after #'ya-formatter-x-clean-eol))
  :config
  (setq-default eglot-autoshutdown t
                eglot-sync-connect 1
                eglot-connect-timeout 60
                eglot-events-buffer-size 0
                eglot-autoreconnect 1
                eglot-send-changes-idle-time 0.75
                eglot-confirm-server-initiated-edits nil
                eglot-ignored-server-capabilities '(:hoverProvider
                                                    :colorProvider
                                                    :codeLensProvider
                                                    :inlayHintProvider
                                                    :signatureHelpProvider
                                                    :foldingRangeProvider
                                                    :documentLinkProvider
                                                    :documentHighlightProvider
                                                    :documentOnTypeFormattingProvider))

  ;; ignore some feature
  (dolist (type '(eglot-note eglot-warning eglot-error))
    (cl-remf (symbol-plist type) 'flymake-overlay-control))

  ;; language workspace configuration
  (defvar eglot-language-configuration-alist nil)
  (defun eglot-language-configuration-on (server)
    (let* ((language-id (car (eglot--language-ids server)))
           (language-configuration (cdr (assoc language-id
                                               eglot-language-configuration-alist))))
      (when language-configuration
        (setq-default eglot-workspace-configuration (funcall language-configuration)))))
  (add-hook 'eglot-connect-hook #'eglot-language-configuration-on))

(provide 'init-eglot)
;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
;;; init-eglot.el ends here
