;;; init-ibuffer.el --- -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package ibuffer
  :bind ([remap list-buffers] . ibuffer)
  :config
  (setq ibuffer-show-empty-filter-groups nil
        ibuffer-filter-group-name-face 'font-lock-doc-face
        ibuffer-saved-filter-groups '(("beautify"
                                       ("Org" (mode . org-mode))
                                       ("Magit" (name . "^magit"))
                                       ("Temporary" (name . "^\\*")))))
  (add-hook 'ibuffer-mode-hook
            (lambda ()
              (ibuffer-switch-to-saved-filter-groups "beautify"))))

(provide 'init-ibuffer)
;;; init-ibuffer.el ends here
