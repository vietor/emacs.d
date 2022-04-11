;;; init-dart.el --- -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package dart-mode
  :when (executable-find "dart")
  :hook (dart-mode . eglot-ensure))

(provide 'init-dart)
;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
;;; init-dart.el ends here
