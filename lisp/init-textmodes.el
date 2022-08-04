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

;; xml
(use-package nxml-mode
  :ensure nil
  :when (executable-find "tidy")
  :config
  (defun nxml-mode-beautify (beg end)
    (interactive "r")
    (unless (use-region-p)
      (setq beg (point-min)
            end (point-max)))
    (shell-command-on-region beg end "tidy -xml -q -i --indent-spaces 4 --wrap 0" (current-buffer) t "*tidy-errors*" t))
  (add-to-list 'ya-formatter-beautify-alist '(nxml-mode . nxml-mode-beautify)))

(provide 'init-textmodes)
;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
;;; init-textmodes.el ends here
