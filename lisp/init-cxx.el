(require 'kernel-c-style)
(require 'google-c-style)

(defun is-cxx-mode ()
  (or (eq major-mode 'c-mode)
      (eq major-mode 'c++-mode)))

(add-hook 'c-mode-common-hook
          (lambda ()
            (when (is-cxx-mode)
              (kernel-set-c-style))
            (when indent-tabs-mode
              (setq tab-width c-basic-offset))))

(after-load 'flycheck
  (add-hook 'c-mode-common-hook
            (lambda ()
              (when (is-cxx-mode)
                (setq flycheck-gcc-language-standard "c++11")
                (setq flycheck-clang-language-standard "c++11")))))

(provide 'init-cxx)
