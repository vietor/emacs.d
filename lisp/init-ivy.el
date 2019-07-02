;;; init-ivy.el --- -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(need-package 'ivy)
(add-hook 'after-init-hook 'ivy-mode)
(after-load 'ivy
  (diminish 'ivy-mode)
  (setq-default ivy-use-virtual-buffers t
                ivy-virtual-abbreviate 'fullpath
                ivy-count-format ""
                ivy-magic-tilde nil
                ivy-dynamic-exhibit-delay-ms 150
                ivy-use-selectable-prompt t
                ivy-initial-inputs-alist '((Man-completion-table . "^")
                                           (woman . "^")))
  (define-key ivy-minibuffer-map (kbd "RET") #'ivy-alt-done)
  (dolist (k '("C-j" "C-RET"))
    (define-key ivy-minibuffer-map (kbd k) #'ivy-immediate-done)))

(need-package 'counsel)
(add-hook 'after-init-hook 'counsel-mode)
(after-load 'counsel
  (diminish 'counsel-mode)
  (setq-default counsel-mode-override-describe-bindings t))

(need-package 'swiper)
(after-load 'ivy
  (define-key ivy-mode-map (kbd "M-s /") 'swiper-thing-at-point))

(need-package 'ivy-xref)
(if (< emacs-major-version 27)
    (setq xref-show-xrefs-function #'ivy-xref-show-xrefs)
  (setq xref-show-definitions-function #'ivy-xref-show-defs))

(provide 'init-ivy)
;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
;;; init-ivy.el ends here
