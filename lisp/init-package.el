;;; init-package.el --- -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'seq)
(require 'subr-x)
(require 'package)

(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)
(setq package-user-dir
      (expand-file-name (format "elpa-%s.%s" emacs-major-version emacs-minor-version)
                        user-emacs-directory))

(when (fboundp 'package--save-selected-packages)
  (defun fix-save-selected-packages (&optional value)
    (when value
      (setq package-selected-packages value)))
  (advice-add 'package--save-selected-packages :override #'fix-save-selected-packages))

(unless (bound-and-true-p package--initialized)
  (setq package-check-signature nil
        package-enable-at-startup nil)
  (package-initialize))

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-and-compile
  (require 'use-package)
  (setq use-package-always-ensure t
        use-package-expand-minimally t))

;;; necessary packages

(use-package bind-key)
(use-package diminish)
(use-package fullframe)

(use-package aproject
  :bind ("C-x p" . aproject-change-project)
  :init
  (setq aproject-plugin-environ t)
  (defun aproject-project-root()
    "Return the project directory by `aproject`''"
    aproject-rootdir))

(with-eval-after-load 'project
  (defun project-current ()
    "Return the project instance hack by `aproject`''"
    (cons 'transient (aproject-project-root))))

(provide 'init-package)
;; Local Variables:
;; byte-compile-warnings: (not free-vars)
;; End:
;;; init-package.el ends here
