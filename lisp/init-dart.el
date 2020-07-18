;;; init-dart.el --- -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(when (executable-find "dart")
  (require-package 'dart-mode)
  (require-package 'lsp-dart)
  (add-hook 'dart-mode-hook #'lsp-deferred)
  (with-eval-after-load 'lsp-dart
    (setq lsp-dart-outline nil
          lsp-dart-flutter-outline nil
          lsp-dart-flutter-fringe-colors nil
          lsp-dart-flutter-widget-guides nil
          lsp-dart-dap-flutter-track-widget-creation nil

          lsp-dart-main-code-lens nil
          lsp-dart-test-code-lens nil))

  (add-to-list 'ya-formatter-beautify-alist '(dart-mode . lsp-format-buffer)))

(provide 'init-dart)
;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
;;; init-dart.el ends here
