(when (maybe-require-package 'php-mode)
  (maybe-require-package 'smarty-mode)
  (after-load 'php-mode
    (define-key php-mode-map (kbd "C-.") nil)))

(provide 'init-php)
