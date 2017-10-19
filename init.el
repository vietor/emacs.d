;; Disaple emacs fool
;; (package-initialize)

(when (version< emacs-version "24.4")
  (error "You must run emacs 24.4 or later!"))

(setq gc-cons-threshold (* 128 1024 1024))
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

(require 'init-preinit nil t)
(require 'init-compat)
(require 'init-package)
(require 'init-preload)
(require 'init-environ)

(require 'init-editor)
(require 'init-editor-enhance)
(require 'init-editor-extends)
(require 'init-sessions)
(require 'init-locales)
(require 'init-windows)

(require 'init-git)
(require 'init-gtags)
(require 'init-company)
(require 'init-flycheck)

(require 'init-cxx)
(require 'init-jvm)
(require 'init-python)
(require 'init-web)
(require 'init-javascript)
(require 'init-markdown)

(when (file-exists-p custom-file)
  (load custom-file))

