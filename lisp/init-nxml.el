;;; init-nxml.el --- -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package nxml-mode
  :ensure nil
  :init
  (setq nxml-slash-auto-complete-flag t)

  (defun nxml-mode-beautify (begin end)
    (interactive "r")
    (unless (use-region-p)
      (setq beg (point-min)
            end (point-max)))
    (save-excursion
      (goto-char begin)
      (while (search-forward-regexp ">[ \t]*<[^/]" end t)
        (backward-char 2) (insert "\n") (incf end))
      (goto-char begin)
      (while (search-forward-regexp "<.*?/.*?>[ \t]*<" end t)
        (backward-char) (insert "\n") (incf end))
      (goto-char begin)
      (while (search-forward-regexp "\\(<\\([a-zA-Z][-:A-Za-z0-9]*\\)\\|['\"]\\) \\(xmlns[=:]\\)" end t)
        (goto-char (match-end 0))
        (backward-char 6) (insert "\n") (incf end))
      (indent-region begin end nil)))

  (add-to-list 'ya-formatter-beautify-alist '(nxml-mode . nxml-mode-beautify)))

(provide 'init-nxml)
;;; init-nxml.el ends here
