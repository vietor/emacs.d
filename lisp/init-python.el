;;; init-python.el --- -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package python
  :mode ("SConstruct\\'" "SConscript\\'")
  :hook ((python-mode . eglot-ensure)
         (python-ts-mode . eglot-ensure))
  :init
  (setq python-shell-interpreter "python3"))

(use-package pip-requirements
  :ensure t)

(provide 'init-python)
;;; init-python.el ends here
