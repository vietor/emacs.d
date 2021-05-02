;;; init-dart.el --- -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package dart-mode
  :when (executable-find "dart")
  :hook (dart-mode . eglot-ensure)
  :init
  (defun dart-get-analysis_server()
    (let* ((dart-exe (executable-find "dart"))
           (dart-sdk-dir (file-truename (locate-dominating-file dart-exe "bin"))))
      (concat dart-sdk-dir "bin/cache/dart-sdk/bin/snapshots/analysis_server.dart.snapshot")))

  (add-to-list 'eglot-server-programs
               `(dart-mode . ("dart" ,(dart-get-analysis_server) "--lsp"))))

(use-package lsp-dart
  :disabled t
  :after dart-mode
  :hook (dart-mode . lsp-deferred)
  :config
  (setq lsp-dart-line-length 160
        lsp-dart-outline nil
        lsp-dart-closing-labels nil
        lsp-dart-main-code-lens nil
        lsp-dart-test-code-lens nil
        lsp-dart-flutter-outline nil
        lsp-dart-flutter-fringe-colors nil
        lsp-dart-flutter-widget-guides nil))

(provide 'init-dart)
;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
;;; init-dart.el ends here
