;;; init-javascript.el --- -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(add-to-list 'auto-append-environ-path "node_modules/.bin")

;; json

(use-package json-mode
  :init
  (add-to-list 'ya-formatter-beautify-alist '(json-mode . json-mode-beautify)))

;; javascript

(use-package js-mode
  :ensure nil
  :mode "\\.\\(js\\|es6\\)\\(\\.erb\\)?\\'"
  :init
  (setq-default js-indent-level 2))

(use-package prettier-js
  :when (executable-find "prettier")
  :commands (prettier-js)
  :init
  (add-to-list 'ya-formatter-beautify-alist '(js-mode . prettier-js)))

;; helper for company-mode

(with-eval-after-load 'company-gtags
  (add-to-list 'company-gtags-modes 'js-mode))

(with-eval-after-load 'company-dabbrev-code
  (add-to-list 'company-dabbrev-code-modes 'js-mode))

(provide 'init-javascript)
;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
;;; init-javascript.el ends here
