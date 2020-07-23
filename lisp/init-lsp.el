;;; init-lsp.el --- -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package lsp-mode
  :diminish
  :bind (:map lsp-mode-map
              ("C-c C-d" . lsp-describe-thing-at-point)
              ([remap xref-find-definitions] . lsp-find-definition)
              ([remap xref-find-references] . lsp-find-references))
  :init
  (setq lsp-keymap-prefix "C-c l"
        read-process-output-max (* 1024 1024))
  :init
  (add-to-list 'ya-formatter-beautify-minor-alist '(lsp-mode . lsp-format-buffer))
  :config
  (setq lsp-prefer-capf t
        lsp-keep-workspace-alive nil
        lsp-signature-auto-activate nil
        lsp-modeline-code-actions-enable nil

        lsp-enable-file-watchers nil
        lsp-enable-file-watchers nil

        lsp-auto-configure nil
        lsp-enable-snippet nil
        lsp-enable-folding nil
        lsp-enable-semantic-highlighting nil
        lsp-enable-symbol-highlighting nil
        lsp-enable-text-document-color nil

        lsp-enable-indentation nil
        lsp-enable-on-type-formatting nil)

  (add-hook 'lsp-mode-hook 'lsp-flycheck-enable)

  ;; disable auto download
  (defun lsp--install-server-internal())

  ;; use aproject

  (setq lsp-auto-guess-root t)
  (defun lsp--suggest-project-root ()
    "Advice the project root."
    aproject-rootdir)

  (before-aproject-change
   (setq lsp-restart 'ignore)
   (mapcar 'delete-process (process-list)))

  (after-aproject-change
   (setq lsp-restart 'auto-restart)
   (setq lsp-session-file (aproject-store-file ".lsp-session"))))

(provide 'init-lsp)
;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
;;; init-lsp.el ends here
