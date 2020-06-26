;;; init-javascript.el --- -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(add-to-list 'auto-append-environ-path "node_modules/.bin")

;; json

(require-package 'json-mode)
(with-eval-after-load 'json-mode
  (add-to-list 'ya-formatter-beautify-alist '(json-mode . json-mode-beautify)))

;; javascript

(require-package 'js2-mode)
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(with-eval-after-load 'js2-mode
  (define-key js2-mode-map (kbd "M-.") nil)
  (setq-default js2-bounce-indent-p nil
                js2-mode-show-parse-errors nil
                js2-mode-show-strict-warnings nil)
  (defun js2-mode-on ()
    (setq mode-name "JS2")
    (unless (flycheck-get-checker-for-buffer)
      (setq-local js2-mode-show-parse-errors t)
      (setq-local js2-mode-show-strict-warnings t)))
  (add-hook 'js2-mode-hook 'js2-mode-on))

(when (executable-find "js-beautify")
  (require-package 'web-beautify)
  (add-to-list 'ya-formatter-beautify-alist '(js2-mode . web-beautify-js)))

(require-package 'rjsx-mode)
(with-eval-after-load 'rjsx-mode
  (define-key rjsx-mode-map "<" nil)
  (define-key rjsx-mode-map ">" nil)
  (define-key rjsx-mode-map (kbd "C-d") nil)
  (defun rjsx-mode-on ()
    (setq mode-name "JS2-JSX"))
  (add-hook 'rjsx-mode-hook 'rjsx-mode-on))

;; helper for company-mode

(with-eval-after-load 'company-gtags
  (add-to-list 'company-gtags-modes 'js2-mode))

(with-eval-after-load 'company-dabbrev-code
  (add-to-list 'company-dabbrev-code-modes 'js2-mode))

(provide 'init-javascript)
;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
;;; init-javascript.el ends here
