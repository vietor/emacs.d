;;; init-python.el --- -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package python
  :ensure nil
  :mode ("SConstruct\\'" "SConscript\\'")
  :hook (python-mode . eglot-ensure)
  :init
  (setq python-shell-interpreter "python3"))

(use-package pip-requirements)

(use-package py-autopep8
  :when (executable-find "autopep8")
  :init
  (defun python-mode-beautify ()
    "Deep indent for python."
    (py-autopep8-buffer)
    (when system-is-win
      (ya-formatter-x-clean-eol)))
  (add-to-list 'ya-formatter-beautify-alist '(python-mode . python-mode-beautify)))

(provide 'init-python)
;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
;;; init-python.el ends here
