;;; init-javascript.el --- -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; json

(use-package 'json-mode)

(add-hook 'json-mode-hook
          (lambda ()
            (setq json-reformat:pretty-string? t)
            (setq deep-buffer-indent-function 'json-mode-beautify)))

;; javascript

(use-package 'js2-mode)
(use-package 'rjsx-mode)
(when (executable-find "js-beautify")
  (use-package 'web-beautify))

(after-load 'js2-mode
  (define-key js2-mode-map (kbd "M-.") nil)
  (setq-default js2-bounce-indent-p nil)
  (setq-default js2-mode-show-parse-errors nil
                js2-mode-show-strict-warnings nil)
  (add-hook 'js2-mode-hook
            (lambda ()
              (setq mode-name "JS2")
              (setq blink-matching-paren nil)
              (when (fboundp 'web-beautify-js)
                (setq deep-buffer-indent-function 'web-beautify-js))
              (unless (flycheck-get-checker-for-buffer)
                (setq-local js2-mode-show-parse-errors t)
                (setq-local js2-mode-show-strict-warnings t)))))

(after-load 'rjsx-mode
  (define-key rjsx-mode-map "<" nil)
  (define-key rjsx-mode-map ">" nil)
  (define-key rjsx-mode-map (kbd "C-d") nil)
  (add-hook 'rjsx-mode-hook
            (lambda ()
              (setq mode-name "JS2-JSX")
              (setq deep-buffer-indent-function nil))))

(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.jsx\\'" . rjsx-mode))

;; helper for company-mode

(after-load 'company-gtags
  (add-to-list 'company-gtags-modes 'js2-mode))

(after-load 'company-dabbrev-code
  (add-to-list 'company-dabbrev-code-modes 'js2-mode))

(provide 'init-javascript)
;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
;;; init-javascript.el ends here
