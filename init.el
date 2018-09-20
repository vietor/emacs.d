;; -*- lexical-binding: t -*-
(setq debug-on-error t)

(when (version< emacs-version "24.4")
  (error "You must run emacs 24.4 or later!"))
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

(let ((normal-gc-cons-threshold (* 20 1024 1024))
      (init-gc-cons-threshold (* 128 1024 1024)))
  (setq gc-cons-threshold init-gc-cons-threshold)
  (add-hook 'emacs-startup-hook
            (lambda () (setq gc-cons-threshold normal-gc-cons-threshold))))

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

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))
