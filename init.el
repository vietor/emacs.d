;; -*- lexical-binding: t -*-

(let ((minver "24.4"))
  (when (version< emacs-version minver)
    (error "Your Emacs is too old -- this config requires v%s or higher" minver)))
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

(defconst *is-os-mac* (eq system-type 'darwin))
(defconst *is-os-win* (eq system-type 'windows-nt))

(setq gc-cons-threshold (* 128 1024 1024))
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold (* 20 1024 1024))))

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

(require 'init-preinit nil t)
(require 'init-compat)
(require 'init-package)
(require 'init-preload)
(require 'init-environ)

(require 'init-editor-a)
(require 'init-editor-b)
(require 'init-editor-z)
(require 'init-sessions)
(require 'init-locales)
(require 'init-windows)

(require 'init-git)
(require 'init-grep)
(require 'init-gtags)
(require 'init-company)
(require 'init-flycheck)

(require 'init-cxx)
(require 'init-jvm)
(require 'init-python)
(require 'init-golang)
(require 'init-web)
(require 'init-php)
(require 'init-javascript)
(require 'init-markdown)

(when (file-exists-p custom-file)
  (load custom-file))

(provide 'init)
