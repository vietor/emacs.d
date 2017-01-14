;; html
(require-package 'web-mode)
(require 'web-mode)

(add-to-list 'auto-mode-alist '("\\.jsx?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))

(add-to-list 'web-mode-indentation-params '("lineup-args" . nil))
(add-to-list 'web-mode-indentation-params '("lineup-calls" . nil))
(add-to-list 'web-mode-indentation-params '("lineup-concats" . nil))
(add-to-list 'web-mode-indentation-params '("lineup-ternary" . nil))

(defvar web-mode-standard-indent 2)
(add-hook 'web-mode-hook
          (lambda ()
            (setq web-mode-css-indent-offset web-mode-standard-indent)
            (setq web-mode-code-indent-offset web-mode-standard-indent)
            (setq web-mode-markup-indent-offset web-mode-standard-indent)))

(after-load 'company-dabbrev-code
  (add-to-list 'company-dabbrev-code-modes 'web-mode))

;; css
(require-package 'sass-mode)

(provide 'init-web)
