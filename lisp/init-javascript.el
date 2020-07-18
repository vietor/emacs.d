;;; init-javascript.el --- -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(add-to-list 'auto-append-environ-path "node_modules/.bin")

;; json

(use-package json-mode
  :init
  (add-to-list 'ya-formatter-beautify-alist '(json-mode . json-mode-beautify)))

;; javascript

(use-package js2-mode
  :mode "\\.js\\'"
  :bind (:map js2-mode-map
              ("M-." . nil))
  :config
  (setq-default js2-bounce-indent-p nil
                js2-mode-show-parse-errors nil
                js2-mode-show-strict-warnings nil)
  (defun js2-mode-on ()
    (setq mode-name "JS2")
    (unless (flycheck-get-checker-for-buffer)
      (setq-local js2-mode-show-parse-errors t)
      (setq-local js2-mode-show-strict-warnings t)))
  (add-hook 'js2-mode-hook 'js2-mode-on))

(use-package web-beautify
  :when (executable-find "js-beautify")
  :init
  (add-to-list 'ya-formatter-beautify-alist '(js2-mode . web-beautify-js)))

(use-package rjsx-mode
  :bind (:map rjsx-mode-map
              ("<" . nil)
              (">" . nil)
              ("C-d" . nil))
  :config
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
