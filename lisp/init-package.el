;;; init-package.el --- -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'seq)
(require 'subr-x)
(require 'package)

;; preload for custom package-archives
(let ((preload-file (expand-file-name "preload.el" user-emacs-directory)))
  (when (file-exists-p preload-file)
    (load preload-file)))

(dolist (archive '(("melpa" . "https://melpa.org/packages/")))
  (unless (assoc (car archive) package-archives)
    (add-to-list 'package-archives archive t)))

(setq package-user-dir
      (expand-file-name (format "elpa-%s.%s" emacs-major-version emacs-minor-version)
                        user-emacs-directory))

(unless (bound-and-true-p package--initialized)
  (setq package-check-signature nil
        package-enable-at-startup nil)
  (package-initialize))

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

(setq use-package-always-ensure t
      use-package-expand-minimally t)

;;; necessary packages

(use-package bind-key)
(use-package diminish)
(use-package fullframe)

(provide 'init-package)
;;; init-package.el ends here
