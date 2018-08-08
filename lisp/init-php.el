(when (maybe-require-package 'php-mode)
  (maybe-require-package 'smarty-mode)
  (after-load 'php-mode
    (define-key php-mode-map (kbd "C-.") nil))

  (when (maybe-require-package 'company-php)
    (after-load 'company
      (add-hook 'php-mode-hook
                (lambda () (sanityinc/local-push-company-backend 'company-ac-php-backend))))))

(provide 'init-php)
