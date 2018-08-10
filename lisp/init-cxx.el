(require 'kernel-c-style)
(require 'google-c-style)

(c-add-style "kernel" kernel-c-style)
(c-add-style "google" google-c-style)

(defadvice c-set-style (after tab-width activate)
  (when indent-tabs-mode
    (setq tab-width c-basic-offset)))

(defvar cxx-style "kernel")

(defun cxx-mode-setup ()
    (c-set-style cxx-style))

(add-hook 'c-mode-hook 'cxx-mode-setup)
(add-hook 'c++-mode-hook 'cxx-mode-setup)

(provide 'init-cxx)
