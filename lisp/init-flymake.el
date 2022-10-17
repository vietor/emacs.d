;;; init-flymake.el --- -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package flymake
  :bind (:map flymake-mode-map
              ("M-n" . flymake-goto-next-error)
              ("M-p" . flymake-goto-prev-error))
  :config
  (setq elisp-flymake-byte-compile-load-path load-path)
  :init
  (dolist (hook '(prog-mode-hook text-mode-hook))
    (add-hook hook 'flymake-mode)))

(use-package flymake-flycheck
  :after flymake
  :config
  (defun active-flymake-flycheck ()
    (setq-local flymake-diagnostic-functions
                (append flymake-diagnostic-functions
                        (flymake-flycheck-all-chained-diagnostic-functions))))
  (add-hook 'flymake-mode-hook 'active-flymake-flycheck))


(unless (version< emacs-version "28.1")
  (setq eldoc-documentation-function 'eldoc-documentation-compose)

  (add-hook 'flymake-mode-hook
            (lambda ()
              (setq eldoc-documentation-functions
                    (cons 'flymake-eldoc-function
                          (delq 'flymake-eldoc-function eldoc-documentation-functions))))))

(provide 'init-flymake)
;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
;;; init-flymake.el ends here
