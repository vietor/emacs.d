;;; init-flymake.el --- -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package flymake
  :init
  (dolist (hook '(prog-mode-hook text-mode-hook))
    (add-hook hook 'flymake-mode)))

(use-package flymake-flycheck
  :after flymake
  :config
  (setq-default flycheck-disabled-checkers
                (append (default-value 'flycheck-disabled-checkers)
                        '(emacs-lisp emacs-lisp-checkdoc emacs-lisp-package))))

(provide 'init-flymake)
;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
;;; init-flymake.el ends here
