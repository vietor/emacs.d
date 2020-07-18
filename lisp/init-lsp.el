;;; init-lsp.el --- -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require-package 'lsp-mode)
(setq lsp-keymap-prefix "M-g i"
      read-process-output-max (* 1024 1024))
(with-eval-after-load 'lsp-mode
  (setq lsp-prefer-capf t
        lsp-keep-workspace-alive nil
        lsp-signature-auto-activate nil
        lsp-modeline-code-actions-enable nil

        lsp-enable-file-watchers nil
        lsp-enable-file-watchers nil

        lsp-auto-configure nil
        lsp-enable-folding nil
        lsp-enable-semantic-highlighting nil
        lsp-enable-symbol-highlighting nil
        lsp-enable-text-document-color nil

        lsp-enable-indentation nil
        lsp-enable-on-type-formatting nil)

  (lsp-flycheck-enable)

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
