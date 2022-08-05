;;; init-nxml.el --- -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package nxml-mode
  :ensure nil
  :init
  (setq nxml-slash-auto-complete-flag t)

  (when (executable-find "tidy")
    (defun nxml-mode-beautify (beg end)
      (interactive "r")
      (unless (use-region-p)
        (setq beg (point-min)
              end (point-max)))
      (shell-command-on-region beg end "tidy -xml -q -i --wrap 0" (current-buffer) t "*tidy-errors*" t))
    (add-to-list 'ya-formatter-beautify-alist '(nxml-mode . nxml-mode-beautify))))

(provide 'init-nxml)
;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
;;; init-nxml.el ends here
