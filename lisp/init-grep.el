;;; init-grep.el --- -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; grep

(setq-default grep-highlight-matches t
              grep-scroll-output t)

(when system-is-mac
  (setq-default locate-command "mdfind"))

;; wgrep

(use-package wgrep
  :config
  (setq wgrep-enable-key "e"
        wgrep-auto-save-buffer t))

;; gtags

(use-package agtags
  :when (executable-find "global")
  :init
  (after-aproject-change
   (agtags-update-root aproject-rootdir))
  :config
  (require 'agtags-xref)

  (defun agtags-mode-on()
    (agtags-mode 1)
    (diminish 'agtags-mode))

  (setq agtags-key-prefix "C-x t")
  (setq agtags-global-treat-text t)

  (agtags-bind-keys)
  (dolist (hook '(prog-mode-hook text-mode-hook))
    (add-hook hook 'agtags-mode-on))
  (add-to-list 'xref-backend-functions 'agtags-xref-backend))

(provide 'init-grep)
;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
;;; init-grep.el ends here
