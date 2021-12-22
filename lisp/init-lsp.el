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
                eglot-confirm-server-initiated-edits nil
                eglot-ignored-server-capabilites '(:documentHighlightProvider
                                                   :hoverProvider
                                                   :signatureHelpProvider
                                                   :documentLinkProvider
                                                   :codeLensProvider
                                                   :colorProvider
                                                   :foldingRangeProvider
                                                   :documentOnTypeFormattingProvider))

  ;; ignore some feature
  (dolist (type '(eglot-note eglot-warning eglot-error))
    (let ((foc (get type 'flymake-overlay-control)))
      (dolist (key '(keymap mouse-face))
        (setq foc (cl-remove key foc :key #'car)))
      (put type 'flymake-overlay-control foc)))

  (bind-keys :map eglot-mode-map
             ("C-c o" . eglot-code-actions))

  ;; language workspace configuration
  (defvar eglot-language-configuration-alist nil)
  (defun eglot-language-configuration-on (server)
    (let ((configuration (cdr (assoc (eglot--language-id server)
                                     eglot-language-configuration-alist))))
      (when configuration (funcall configuration))))
  (add-hook 'eglot-connect-hook #'eglot-language-configuration-on)

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
