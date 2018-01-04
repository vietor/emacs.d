;; json
(require-package 'json-mode)

(add-hook 'json-mode-hook
          (lambda ()
            (setq json-reformat:pretty-string? t)
            (setq deep-buffer-indent-function 'json-mode-beautify)))

;; javascript
(require-package 'js2-mode)

(add-hook 'js-mode-hook 'js2-minor-mode)
(add-hook 'js2-mode-hook #'js2-imenu-extras-mode)
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(add-to-list 'interpreter-mode-alist '("node" . js2-mode))

(when (executable-find "prettier")
  (require-package 'prettier-js))

(when (executable-find "js-beautify")
  (require-package 'web-beautify))

(add-hook 'js2-mode-hook
          (lambda ()
            (setq mode-name "JS2")
            (when (fboundp 'web-beautify-js)
              (setq deep-buffer-indent-function 'web-beautify-js))))

(add-hook 'js2-jsx-mode-hook
          (lambda ()
            (setq mode-name "JS2-JSX")
            (setq deep-buffer-indent-function nil)))

(after-load 'company-gtags
  (add-to-list 'company-gtags-modes 'js2-mode))

(after-load 'company-dabbrev-code
  (add-to-list 'company-dabbrev-code-modes 'js2-mode))

(provide 'init-javascript)
