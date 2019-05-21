;;; init-git.el --- -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require-package 'gitignore-mode)
(require-package 'gitconfig-mode)

(when (maybe-require-package 'magit)
  (setq-default magit-diff-refine-hunk t)
  (global-set-key (kbd "C-x g") 'magit-status))

(after-load 'magit
  (fullframe magit-status magit-mode-quit-window))

(provide 'init-git)
;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
;;; init-git.el ends here
