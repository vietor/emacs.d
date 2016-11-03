(eval-when-compile (require 'cl))
(unless (>= emacs-major-version 24)
  (error "You must run emacs 24 or later!"))
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

(require 'init-compat)
(require 'init-package)
(require 'init-aproject)
(require 'init-environ)
(require 'init-preload)

(require 'init-gtags)

(require 'init-editor)
(require 'init-editor-enhance)
(require 'init-editor-extends)
(require 'init-sessions)
(require 'init-locales)
(require 'init-windows)

(require 'init-javascript)
