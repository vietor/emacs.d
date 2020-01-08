;;; init-javascript.el --- -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; json

(require-package 'json-mode)
(after-load 'json-mode
  (add-to-list 'buffer-beautify-alist '(json-mode . json-mode-beautify)))

;; javascript

(require-package 'js2-mode)
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(after-load 'js2-mode
  (define-key js2-mode-map (kbd "M-.") nil)
  (setq-default js2-bounce-indent-p nil
                js2-mode-show-parse-errors nil
                js2-mode-show-strict-warnings nil)

  (add-hook 'js2-mode-hook
            (lambda ()
              (setq mode-name "JS2")
              (unless (flycheck-get-checker-for-buffer)
                (setq-local js2-mode-show-parse-errors t)
                (setq-local js2-mode-show-strict-warnings t)))))

(when (executable-find "js-beautify")
  (require-package 'web-beautify)
  (add-to-list 'buffer-beautify-alist '(js2-mode . web-beautify-js)))

(require-package 'rjsx-mode)
(add-to-list 'auto-mode-alist '("\\.jsx\\'" . rjsx-mode))
(after-load 'rjsx-mode
  (define-key rjsx-mode-map "<" nil)
  (define-key rjsx-mode-map ">" nil)
  (define-key rjsx-mode-map (kbd "C-d") nil)
  (add-hook 'rjsx-mode-hook
            (lambda ()
              (setq mode-name "JS2-JSX"))))

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
