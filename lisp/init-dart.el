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

(provide 'init-dart)
;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
;;; init-dart.el ends here
