(unless (version< emacs-version "24.4")
  (require-package 'magit)
  (require-package 'gitignore-mode)
  (require-package 'gitconfig-mode)
  (require-package 'git-messenger)
  (require-package 'git-timemachine)

  (setq-default
   magit-process-popup-time 10
   magit-diff-refine-hunk t
   magit-completing-read-function 'magit-ido-completing-read)

  (add-hook 'git-commit-mode-hook 'goto-address-mode)
  (after-load 'session
    (when (boundp 'session-mode-disable-list)
      (add-to-list 'session-mode-disable-list 'git-commit-mode)))

  (global-set-key (kbd "C-<f12>") 'magit-status)
  (global-set-key (kbd "C-x v f") 'vc-git-grep)
  (global-set-key (kbd "C-x v p") #'git-messenger:popup-message))

(provide 'init-git)
