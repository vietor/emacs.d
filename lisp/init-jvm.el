;;; init-jvm.el --- -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(want-package 'scala-mode)

(when (executable-find "gradle")
  (want-package 'groovy-mode)
  (add-to-list 'auto-mode-alist '("\\.gradle\\'" . groovy-mode)))

(provide 'init-jvm)
;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
;;; init-jvm.el ends here
