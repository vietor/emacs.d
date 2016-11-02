(eval-when-compile (require 'cl))
(unless (>= emacs-major-version 24)
  (error "You must run emacs 24 or later!"))
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

(require 'init-compat)
(require 'init-package)
(require 'init-aproject)
(require 'init-environ)

