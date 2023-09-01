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
  :ensure t
  :after flymake
  :config
  (with-eval-after-load 'flycheck
    (setq-default flycheck-disabled-checkers
                  (append (default-value 'flycheck-disabled-checkers)
                          '(emacs-lisp emacs-lisp-checkdoc emacs-lisp-package))))

  (defun flymake-flycheck-on ()
    (setq-local flymake-diagnostic-functions
                (seq-uniq (append flymake-diagnostic-functions
                                  (flymake-flycheck-all-chained-diagnostic-functions)))))

  (add-hook 'flymake-mode-hook 'flymake-flycheck-on))


(unless (version< emacs-version "28.1")
  (setq eldoc-documentation-function 'eldoc-documentation-compose)

  (defun fix-eldoc-documentation ()
    (add-hook 'eldoc-documentation-functions 'flymake-eldoc-function nil t))

  (add-hook 'flymake-mode-hook 'fix-eldoc-documentation))

(provide 'init-flymake)
;;; init-flymake.el ends here
