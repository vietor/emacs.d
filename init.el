(when (version< emacs-version "24")
  (error "You must run emacs 24 or later!"))

(setq gc-cons-threshold (* 128 1024 1024))
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

(require 'init-compat)
(require 'init-package)
(require 'init-aproject)
(require 'init-environ)
(require 'init-preload)

(require 'init-editor)
(require 'init-editor-enhance)
(require 'init-editor-extends)
(require 'init-sessions)
(require 'init-locales)
(require 'init-windows)

(require 'init-git)
(require 'init-gtags)
(require 'init-company)

(require 'init-cxx)
(require 'init-scala)
(require 'init-python)
(require 'init-web)
(require 'init-javascript)
