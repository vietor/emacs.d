;;; init-javascript.el --- -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(add-to-list 'auto-append-environ-path "node_modules/.bin")

;; json

(use-package json-mode
  :init
  (defun fix-json-mode-beautify()
    (call-interactively 'json-mode-beautify))
  (add-to-list 'ya-formatter-beautify-alist '(json-mode . fix-json-mode-beautify)))

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

(provide 'init-javascript)
;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
;;; init-javascript.el ends here
