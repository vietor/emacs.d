;;; init-web.el --- -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; html

(require-package 'web-mode t)

(add-to-list 'auto-mode-alist '("\\.njk?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))

(add-to-list 'web-mode-indentation-params '("lineup-args" . nil))
(add-to-list 'web-mode-indentation-params '("lineup-calls" . nil))
(add-to-list 'web-mode-indentation-params '("lineup-concats" . nil))
(add-to-list 'web-mode-indentation-params '("lineup-ternary" . nil))

(defvar web-mode-standard-indent 4)
(add-hook 'web-mode-hook
          (lambda ()
            (setq web-mode-css-indent-offset web-mode-standard-indent)
            (setq web-mode-code-indent-offset web-mode-standard-indent)
            (setq web-mode-markup-indent-offset web-mode-standard-indent)))

(after-load 'company-dabbrev-code
  (add-to-list 'company-dabbrev-code-modes 'web-mode))

;; more css

(require-package 'sass-mode)
(unless (fboundp 'scss-mode)
  (require-package 'scss-mode))
(setq-default scss-compile-at-save nil)

(provide 'init-web)
;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
;;; init-web.el ends here
