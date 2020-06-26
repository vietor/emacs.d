;;; init-textmodes.el --- -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; markdown

(require-package 'markdown-mode)
(add-to-list 'ya-formatter-disabled-modes 'markdown-mode)

;; yaml

(require-package 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml\\.erb\\'" . yaml-mode))

(provide 'init-textmodes)
;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
;;; init-textmodes.el ends here
