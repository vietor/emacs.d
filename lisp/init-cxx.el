(require 'kernel-c-style)
(require 'google-c-style)

(add-hook 'c-mode-hook
          (lambda ()
            (kernel-set-c-style)
            (when indent-tabs-mode
              (setq tab-width c-basic-offset))))

(after-load 'flycheck
  (add-hook 'c-mode-hook
            (lambda ()
              (setq flycheck-gcc-language-standard "c++11")
              (setq flycheck-clang-language-standard "c++11"))))

(provide 'init-cxx)
