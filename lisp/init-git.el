(require-package 'git-blamed)
(require-package 'gitignore-mode)
(require-package 'gitconfig-mode)

(when (maybe-require-package 'magit)
  (setq-default magit-diff-refine-hunk t)
  (global-set-key (kbd "C-x g") 'magit-status))

(after-load 'magit
  (fullframe magit-status magit-mode-quit-window))

(when (maybe-require-package 'git-commit)
  (add-hook 'git-commit-mode-hook 'goto-address-mode))

(provide 'init-git)
