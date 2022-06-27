;;; init-flymake.el --- -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(unless (version< emacs-version "28.1")
  (setq eldoc-documentation-function 'eldoc-documentation-compose))

(use-package flymake
  :bind(:map flymake-mode-map
             ("M-n" . flymake-goto-next-error)
             ("M-p" . flymake-goto-prev-error))
  :init
  (dolist (hook '(prog-mode-hook text-mode-hook))
    (add-hook hook 'flymake-mode)))

(use-package flymake-flycheck
  :after flymake
  :config
  (setq-default flycheck-disabled-checkers
                (append (default-value 'flycheck-disabled-checkers)
                        '(emacs-lisp emacs-lisp-checkdoc emacs-lisp-package)))


  (defun active-flymake-flycheck ()
    (setq-local flymake-diagnostic-functions
                (append flymake-diagnostic-functions
                        (flymake-flycheck-all-chained-diagnostic-functions))))
  (add-hook 'flymake-mode-hook 'active-flymake-flycheck))

(provide 'init-flymake)
;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
;;; init-flymake.el ends here
