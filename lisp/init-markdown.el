(when (maybe-require-package 'markdown-mode)
  (push 'markdown-mode buffer-indent-ignore-modes)
  (add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
  (add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode)))

(provide 'init-markdown)
