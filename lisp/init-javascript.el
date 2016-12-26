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

(when (executable-find "js-beautify")
  (require-package 'web-beautify)
  (add-hook 'js2-mode-hook
            (lambda ()
              (setq deep-buffer-indent-function 'web-beautify-js))))

(after-load 'company-gtags
  (add-to-list 'company-gtags-modes 'js2-mode))

(provide 'init-javascript)
