(setq auto-mode-alist
      (append '(("SConstruct\\'" . python-mode)
                ("SConscript\\'" . python-mode))
              auto-mode-alist))

(require-package 'py-autopep8)
(require-package 'pip-requirements)

(defun python-deep-buffer-indent ()
  (py-autopep8)
  (when running-os-windows
    (goto-char (point-min))
    (while (search-forward "\r" nil t) (replace-match ""))))

(add-hook 'python-mode-hook
          (lambda ()
            (setq deep-buffer-indent-function 'python-deep-buffer-indent)))

(provide 'init-python)
