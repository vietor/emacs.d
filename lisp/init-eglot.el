;;; init-eglot.el --- -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package eglot
  :ensure t
  :demand t
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

  (defun eglot-language-found-id (server)
    (string-remove-suffix "-ts"
                          (if (fboundp 'eglot--language-id)
                              (eglot--language-id server)
                            (car (eglot--language-ids server)))))

  (defun eglot-language-etc-file (file)
    (expand-file-name (concat "etc/" file) user-emacs-directory))

  (defun eglot-language-etc-file-url (file)
    (concat "file://" (if (eq system-type 'windows-nt) "/") (eglot-language-etc-file file)))

  (defun eglot-language-etc-json-read (file)
    (ignore-errors
      (with-temp-buffer
        (insert-file-contents (eglot-language-etc-file file))
        (goto-char (point-min))
        (when (search-forward "{{ETC-" nil t)
          (replace-string "{{ETC-PATH}}" (eglot-language-etc-file ""))
          (goto-char (point-min))
          (replace-string "{{ETC-URL}}" (eglot-language-etc-file-url "")))
        (goto-char (point-min))
        (json-parse-buffer :object-type 'plist :false-object :json-false))))

  ;; language workspace configuration
  (defun eglot-language-workspace-configuration (server)
    (let* ((language-id (eglot-language-found-id server))
           (language-configuration-file (concat "lsp-" language-id "-settings.json")))
      (or (eglot-language-etc-json-read language-configuration-file) ())))
  (setq-default eglot-workspace-configuration 'eglot-language-workspace-configuration))

(provide 'init-eglot)
;;; init-eglot.el ends here
