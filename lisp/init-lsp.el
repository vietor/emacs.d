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
        lsp-server-install-dir (expand-file-name "lsp" user-space-directory))
  (add-to-list 'ya-formatter-beautify-minor-alist '(lsp-mode . lsp-format-buffer))
  (before-aproject-change
   (let ((lsp-restart 'ignore))
     (mapcar 'delete-process (process-list))))
  (after-aproject-change
   (setq lsp-session-file (aproject-store-file "lsp-session")))
  :config
  (setq lsp-lens-enable nil
        lsp-ui-doc-enable nil
        lsp-ui-sideline-enable nil

        lsp-eldoc-enable-hover nil
        lsp-completion-show-kind nil
        lsp-completion-show-detail nil

        lsp-headerline-breadcrumb-enable nil
        lsp-modeline-code-actions-enable nil
        lsp-modeline-diagnostics-enable nil
        lsp-signature-auto-activate nil
        lsp-signature-render-documentation nil

        lsp-enable-snippet nil
        lsp-enable-folding nil
        lsp-enable-indentation nil
        lsp-enable-on-type-formatting nil
        lsp-enable-text-document-color nil
        lsp-enable-symbol-highlighting nil

        lsp-keep-workspace-alive nil
        lsp-enable-file-watchers nil
        lsp-enable-dap-auto-configure nil)

  (setq lsp-auto-guess-root t)
  (defun lsp--suggest-project-root ()
    aproject-rootdir)

  (defun lsp-install-server (update?))
  (defun lsp--install-server-internal (client &optional update?)
    (message "Please install the required language server yourself"))
  (defun filter-lsp--completing-read (orig-fun &rest args)
    (if (string-match-p "installed automatically:" (car args))
        (car (nth 1 args))
      (apply orig-fun args)))
  (advice-add 'lsp--completing-read :around #'filter-lsp--completing-read))

(provide 'init-lsp)
;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
;;; init-lsp.el ends here
