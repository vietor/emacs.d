;;; init-eglot.el --- -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package eglot
  :demand
  :bind(:map eglot-mode-map
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
                eglot-confirm-server-initiated-edits nil
                eglot-ignored-server-capabilities '(:documentHighlightProvider
                                                    :hoverProvider
                                                    :documentLinkProvider
                                                    :signatureHelpProvider
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

  ;; language workspace configuration
  (defvar eglot-language-configuration-alist nil)
  (defun eglot-language-configuration-on (server)
    (let ((configuration (cdr (assoc (eglot--language-id server)
                                     eglot-language-configuration-alist))))
      (when configuration (funcall configuration))))
  (add-hook 'eglot-connect-hook #'eglot-language-configuration-on))

(provide 'init-eglot)
;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
;;; init-eglot.el ends here
