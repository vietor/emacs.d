;;; init-lsp.el --- -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package eglot
  :config
  (add-to-list 'eglot-stay-out-of 'eldoc)
  (add-hook 'eglot-managed-mode-hook
            (lambda ()
              (flycheck-mode -1)
              (setq company-backends optimized-company-backends)))
  (setq-default eglot-autoshutdown t
                eglot-sync-connect 1
                eglot-connect-timeout 60
                eglot-events-buffer-size 0
                eglot-ignored-server-capabilites '(:documentHighlightProvider
                                                   :hoverProvider
                                                   :signatureHelpProvider
                                                   :documentLinkProvider
                                                   :codeLensProvider
                                                   :colorProvider
                                                   :foldingRangeProvider
                                                   :documentOnTypeFormattingProvider))

  ;; TODO
  ;; (setq-default eglot-workspace-configuration
  ;;               `((:java.format.settings.url . ,(concat "file:///" (expand-file-name "etc/eclipse-java-google-style.xml" user-emacs-directory)))))

  ;; ignore some feature
  (cl-loop for type in '(eglot-note eglot-warning eglot-error )
           do (let ((foc (get type 'flymake-overlay-control)))
                (setq foc (cl-remove 'keymap foc :key #'car))
                (setq foc (cl-remove 'mouse-face foc :key #'car))
                (put type 'flymake-overlay-control foc)))

  (bind-keys :map eglot-mode-map
             ("C-c o" . eglot-code-actions))

  :init

  (advice-add #'eglot-code-actions :after #'ya-formatter-x-clean-eol)
  (advice-add #'eglot-format-buffer :after #'ya-formatter-x-clean-eol)

  (before-aproject-change
   (eglot-shutdown-all))
  (add-to-list 'ya-formatter-beautify-minor-alist
               '(eglot--managed-mode . eglot-format-buffer)))

(provide 'init-lsp)
;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
;;; init-lsp.el ends here
