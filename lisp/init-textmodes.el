;;; init-textmodes.el --- -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; markdown

(require-package 'markdown-mode)
(add-to-list 'buffer-indent-disabled-modes 'markdown-mode)

(provide 'init-textmodes)
;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
;;; init-textmodes.el ends here
