;;; init-ivy.el --- -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package ivy
  :diminish
  :bind (:map ivy-minibuffer-map
              ("RET" . ivy-alt-done)
              ("C-j" . ivy-immediate-done)
              ("C-RET" . ivy-immediate-done))
  :init
  (add-hook 'after-init-hook 'ivy-mode)
  :config
  (setq-default ivy-use-virtual-buffers t
                ivy-virtual-abbreviate 'fullpath
                ivy-count-format ""
                ivy-magic-tilde nil
                ivy-dynamic-exhibit-delay-ms 150
                ivy-use-selectable-prompt t))


(use-package counsel
  :after ivy
  :diminish
  :bind ("M-g f o" . counsel-recentf)
  :init
  (add-hook 'after-init-hook 'counsel-mode)
  :config
  (setq-default counsel-mode-override-describe-bindings t
                ivy-initial-inputs-alist '((Man-completion-table . "^")
                                           (woman . "^"))))

(use-package swiper
  :after ivy
  :bind (:map ivy-mode-map
              ("M-s /" . swiper-thing-at-point)))

(use-package ivy-xref
  :after ivy
  :init
  (setq xref-show-xrefs-function #'ivy-xref-show-xrefs)
  (when (>= emacs-major-version 27)
    (setq xref-show-definitions-function #'ivy-xref-show-defs)))

(provide 'init-ivy)
;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
;;; init-ivy.el ends here
