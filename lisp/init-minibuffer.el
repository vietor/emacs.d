;;; init-minibuffer.el --- -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package selectrum
  :hook (after-init . selectrum-mode))

(use-package orderless
  :init
  (defun fix-orderless-in-minibuffer ()
    (setq-local completion-styles '(substring orderless)))
  (add-hook 'minibuffer-setup-hook 'fix-orderless-in-minibuffer))

(provide 'init-minibuffer)
;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
;;; init-minibuffer.el ends here
