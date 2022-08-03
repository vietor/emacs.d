;;; init-python.el --- -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package python
  :ensure nil
  :mode ("SConstruct\\'" "SConscript\\'")
  :hook (python-mode . eglot-ensure)
  :init
  (setq python-shell-interpreter "python3")

  (defun python-workspace-configuration()
    (setq eglot-workspace-configuration
          '((pylsp
             (configurationSources . ["flake8"])
             (plugins
              (flake8 (enabled . t))
              (mypy (enabled . t))
              (mccabe (enabled . :json-false))
              (pyflakes (enabled . :json-false))
              (pycodestyle (enabled . :json-false)))))))
  (add-to-list 'eglot-language-configuration-alist '("python" . python-workspace-configuration)))

(use-package pip-requirements)

(provide 'init-python)
;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
;;; init-python.el ends here
