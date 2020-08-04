;;; init-ivy.el --- -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package smex)

(use-package ivy
  :diminish
  :bind (:map ivy-minibuffer-map
              ("RET" . ivy-alt-done)
              ("C-j" . ivy-immediate-done)
              ("C-RET" . ivy-immediate-done))
  :hook (after-init . ivy-mode)
  :config
  (setq-default ivy-use-virtual-buffers t
                ivy-virtual-abbreviate 'fullpath
                ivy-count-format ""
                ivy-magic-tilde nil
                ivy-dynamic-exhibit-delay-ms 150
                ivy-use-selectable-prompt t))

(use-package counsel
  :diminish
  :hook (after-init . counsel-mode)
  :bind ("M-g f o" . counsel-recentf)
  :init
  (setq-default counsel-mode-override-describe-bindings t)
  :config
  (setq-default ivy-initial-inputs-alist '((Man-completion-table . "^")
                                           (woman . "^"))))

(use-package swiper
  :bind (:map ivy-mode-map
              ("M-s /" . swiper-thing-at-point)))

(use-package ivy-xref
  :after ivy
  :init
  (setq xref-show-xrefs-function #'ivy-xref-show-xrefs)
  (when (>= emacs-major-version 27)
    (setq xref-show-definitions-function #'ivy-xref-show-defs)))

(use-package ivy-rich
  :after ivy
  :hook (ivy-mode . ivy-rich-mode)
  :init
  (setq ivy-virtual-abbreviate 'abbreviate
        ivy-rich-switch-buffer-align-virtual-buffer nil
        ivy-rich-path-style 'abbrev)
  :config
  (setcdr (assq t ivy-format-functions-alist) #'ivy-format-function-line))

(provide 'init-ivy)
;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
;;; init-ivy.el ends here
