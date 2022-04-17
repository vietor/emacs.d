;;; init-irc.el --- -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package erc
  :ensure nil
  :config
  (setq erc-hide-list '("JOIN" "PART" "QUIT")))

(provide 'init-irc)
;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
;;; init-irc.el ends here
