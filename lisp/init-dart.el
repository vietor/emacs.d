;;; init-dart.el --- -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package dart-mode
  :when (executable-find "dart")
  :hook (dart-mode . eglot-ensure))

(provide 'init-dart)
;;; init-dart.el ends here
