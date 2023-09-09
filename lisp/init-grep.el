;;; init-grep.el --- -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; grep
(require 'grep)

(setq-default grep-highlight-matches t
              grep-scroll-output t)

(when (eq system-type 'darwin)
  (setq-default locate-command "mdfind"))

;; wgrep

(use-package wgrep
  :ensure t
  :config
  (setq wgrep-enable-key "e"
        wgrep-auto-save-buffer t))

;; ripgrep

(use-package rg
  :ensure t
  :when (executable-find "rg")
  :bind (("M-g t s" . rg-dwim-project-dir)))

;; gtags

(use-package agtags
  :ensure t
  :when (executable-find "global")
  :init
  (after-aproject-change
   (agtags-update-root aproject-rootdir))
  :config
  (defun agtags-mode-on()
    (agtags-mode 1)
    (diminish 'agtags-mode))

  (setq agtags-global-treat-text t)

  (agtags-bind-keys)
  (dolist (hook '(prog-mode-hook text-mode-hook))
    (add-hook hook 'agtags-mode-on))

  (dolist (item '("GPATH" "GTAGS" "GRTAGS"))
    (add-to-list 'grep-find-ignored-files item)))

(provide 'init-grep)
;;; init-grep.el ends here
