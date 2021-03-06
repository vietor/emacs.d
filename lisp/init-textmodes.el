;;; init-textmodes.el --- -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; cmake

(use-package cmake-mode)

;; markdown

(use-package markdown-mode
  :init
  (add-to-list 'ya-formatter-disabled-modes 'markdown-mode))

;; yaml

(use-package yaml-mode
  :mode "\\.yml\\.erb\\'")

(provide 'init-textmodes)
;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
;;; init-textmodes.el ends here
