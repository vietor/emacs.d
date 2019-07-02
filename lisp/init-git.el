;;; init-git.el --- -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(setq vc-handled-backends '(Git))

(want-package 'gitignore-mode)
(want-package 'gitconfig-mode)

(when (try-want-package 'magit)
  (setq-default magit-diff-refine-hunk t)
  (global-set-key (kbd "C-x g") 'magit-status)

  (defun magit-vc-print-log (&optional prompt)
    (interactive "P")
    (if (and (buffer-file-name)
             (eq 'Git (vc-backend (buffer-file-name))))
        (if prompt
            (magit-log-buffer-file-popup)
          (magit-log-buffer-file t))
      (vc-print-log)))
  (after-load 'vc
    (define-key vc-prefix-map (kbd "l") 'magit-vc-print-log)))

(after-load 'magit
  (fullframe magit-status magit-mode-quit-window))

(provide 'init-git)
;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
;;; init-git.el ends here
