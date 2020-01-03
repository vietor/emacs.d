;;; init-java.el --- -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package 'scala-mode)

(when (executable-find "gradle")
  (use-package 'groovy-mode)
  (add-to-list 'auto-mode-alist '("\\.gradle\\'" . groovy-mode)))

(provide 'init-java)
;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
;;; init-java.el ends here
