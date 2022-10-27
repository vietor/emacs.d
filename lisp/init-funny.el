;;; init-funny.el --- -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package erc
  :ensure nil
  :config
  (setq erc-hide-list '("JOIN" "PART" "QUIT")))

(provide 'init-funny)
;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
;;; init-funny.el ends here
