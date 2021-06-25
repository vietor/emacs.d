;;; init-minibuffer.el --- -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package vertico
  :hook (after-init . vertico-mode))

(use-package orderless
  :after vertico
  :init
  (defun fix-orderless-in-minibuffer ()
    (setq-local completion-styles '(substring orderless)))
  (add-hook 'minibuffer-setup-hook fix-orderless-in-minibuffer))

(use-package consult
  :after vertico
  :bind (([remap switch-to-buffer] . consult-buffer)
         ([remap switch-to-buffer-other-window] . consult-buffer-other-window)
         ([remap switch-to-buffer-other-frame] . consult-buffer-other-frame)
         ([remap goto-line] . consult-goto-line))
  :init
  (setq-default consult-project-root-function 'aproject-project-root))

(use-package marginalia
  :hook (after-init . marginalia-mode))

(provide 'init-minibuffer)
;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
;;; init-minibuffer.el ends here
