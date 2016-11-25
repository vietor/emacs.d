(require-package 'git-blamed)
(require-package 'gitignore-mode)
(require-package 'gitconfig-mode)
(require-package 'git-messenger)

(when (maybe-require-package 'magit)
  (setq-default magit-diff-refine-hunk t)
  (global-set-key (kbd "C-<f12>") 'magit-status))

(when (maybe-require-package 'git-commit)
  (add-hook 'git-commit-mode-hook 'goto-address-mode))

(global-set-key (kbd "C-x v p") #'git-messenger:popup-message)

(provide 'init-git)
