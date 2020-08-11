;;; init-python.el --- -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package python
  :ensure nil
  :mode ("SConstruct\\'" "SConscript\\'")
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
      (goto-char (point-min))
      (while (search-forward "\r" nil t) (replace-match ""))))
  (add-to-list 'ya-formatter-beautify-alist '(python-mode . python-mode-beautify)))

(use-package anaconda-mode
  :diminish
  :after python
  :hook ((python-mode . anaconda-mode)
         (anaconda-mode . anaconda-eldoc-mode))
  :bind (:map anaconda-mode-map
              ("M-?" . nil))
  :init
  (setq anaconda-mode-installation-directory (expand-file-name "anaconda-mode" user-space-directory)))

(use-package company-anaconda
  :after (company python)
  :config
  (add-to-list 'company-backends 'company-anaconda))

(provide 'init-python)
;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
;;; init-python.el ends here
