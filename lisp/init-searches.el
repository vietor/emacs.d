;;; init-searches.el --- -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; grep

(setq-default grep-highlight-matches t
              grep-scroll-output t)

(when system-is-mac
  (setq-default locate-command "mdfind"))

;; wgrep

(require-package 'wgrep)
(with-eval-after-load 'wgrep
  (setq wgrep-enable-key "e")
  (setq wgrep-auto-save-buffer t))

;; gtags

(when (executable-find "global")
  (require-package 'agtags)
  (require 'agtags-xref)

  (defun agtags-mode-on()
    (agtags-mode 1)
    (diminish 'agtags-mode))

  (setq agtags-global-treat-text t)

  (agtags-bind-keys)
  (dolist (hook '(prog-mode-hook text-mode-hook))
    (add-hook hook 'agtags-mode-on))
  (add-to-list 'xref-backend-functions 'agtags-xref-backend)

  (after-aproject-change (agtags-update-root aproject-rootdir)))

(provide 'init-searches)
;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
;;; init-searches.el ends here
