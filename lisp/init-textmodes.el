;;; init-textmodes.el --- -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; cmake

(use-package cmake-mode
  :ensure t)

;; markdown

(use-package markdown-mode
  :ensure t
  :init
  (add-to-list 'ya-formatter-disabled-modes 'markdown-mode))

;; yaml

(use-package yaml-mode
  :ensure t
  :mode "\\.yml\\.erb\\'")

(provide 'init-textmodes)
;;; init-textmodes.el ends here
