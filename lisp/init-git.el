;;; init-git.el --- -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(setq vc-handled-backends '(Git))

(use-package gitignore-mode)
(use-package gitconfig-mode)

(use-package git-timemachine
  :when (executable-find "git")
  :bind (:map vc-prefix-map
              ("t" . git-timemachine-toggle)))

(use-package magit
  :when (executable-find "git")
  :bind ("C-x g" . magit-status)
  :config
  (setq-default magit-diff-refine-hunk t)
  (fullframe magit-status magit-mode-quit-window)

  (defun magit-vc-print-log (&optional prompt)
    (interactive "P")
    (if (and (buffer-file-name)
             (eq 'Git (vc-backend (buffer-file-name))))
        (if prompt
            (magit-log-buffer-file-popup)
          (magit-log-buffer-file t))
      (vc-print-log)))
  (bind-key "l" 'magit-vc-print-log vc-prefix-map))

(provide 'init-git)
;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
;;; init-git.el ends here
