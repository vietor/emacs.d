;;; init-dart.el --- -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package dart-mode
  :ensure t
  :when (executable-find "dart")
  :hook (dart-mode . eglot-ensure))

(provide 'init-dart)
;;; init-dart.el ends here
