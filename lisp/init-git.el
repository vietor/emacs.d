;;; init-git.el --- -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(setq vc-handled-backends '(Git))

(require-package 'gitignore-mode)
(require-package 'gitconfig-mode)

(when (executable-find "git")
  (require-package 'magit)
  (global-set-key (kbd "C-x g") 'magit-status)
  (with-eval-after-load 'magit
    (setq-default magit-diff-refine-hunk t)
    (fullframe magit-status magit-mode-quit-window))

  (defun magit-vc-print-log (&optional prompt)
    (interactive "P")
    (if (and (buffer-file-name)
             (eq 'Git (vc-backend (buffer-file-name))))
        (if prompt
            (magit-log-buffer-file-popup)
          (magit-log-buffer-file t))
      (vc-print-log)))
  (with-eval-after-load 'vc
    (define-key vc-prefix-map (kbd "l") 'magit-vc-print-log)))

(provide 'init-git)
;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
;;; init-git.el ends here
