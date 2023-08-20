;;; init-project.el --- -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package aproject
  :ensure t
  :bind ("C-x p" . aproject-change-project)
  :config
  (setq desktop-restore-eager 1)
  (setq aproject-plugin-environ t)
  (add-to-list 'vc-directory-exclusion-list aproject-dirname))

(provide 'init-project)
;;; init-project.el ends here
