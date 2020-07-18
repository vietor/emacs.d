;;; init-dart.el --- -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package dart-mode
  :when (executable-find "dart"))

(use-package lsp-dart
  :after dart-mode
  :hook (dart-mode . lsp-deferred)
  :config
  (setq lsp-dart-outline nil
        lsp-dart-flutter-outline nil
        lsp-dart-flutter-fringe-colors nil
        lsp-dart-flutter-widget-guides nil)
  (add-to-list 'ya-formatter-beautify-alist '(dart-mode . lsp-format-buffer)))

(provide 'init-dart)
;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
;;; init-dart.el ends here
