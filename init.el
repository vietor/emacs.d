;;; init.el --- -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; (setq debug-on-error t)

(let ((minver "26.1"))
  (when (version< emacs-version minver)
    (error "Your Emacs is too old -- this config requires v%s or higher" minver))
  (unless (memq system-type '(darwin windows-nt gnu/linux))
    (error "I am not working on the current operating system, bye")))
(unless (boundp 'early-init-file)
  (load (concat (file-name-directory load-file-name) "early-init") nil t))

(defconst system-is-mac (eq system-type 'darwin))
(defconst system-is-win (eq system-type 'windows-nt))

(add-hook 'emacs-startup-hook
          (lambda ()
            (message "Emacs loaded in %s with %d garbage collections."
                     (format "%.2f seconds"
                             (float-time
                              (time-subtract after-init-time before-init-time)))
                     gcs-done)))

;;----------------------------------------------------------------------------
;; The `load-path' config

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

(defun optimize-site-lisp-to-load-path (&rest _)
  "Add subdirectories to `load-path'."
  (let ((default-directory (expand-file-name "site-lisp" user-emacs-directory)))
    (add-to-list 'load-path default-directory)
    (normal-top-level-add-subdirs-to-load-path)))
(advice-add #'package-initialize :after #'optimize-site-lisp-to-load-path)

;;----------------------------------------------------------------------------
;; Bootstrap config

(defconst user-emacs-space-directory
  (expand-file-name "../.emacs.space/" user-emacs-directory))
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

;;----------------------------------------------------------------------------
;; Load configs

(require 'init-package)
(require 'init-project)
(require 'init-environ)
(require 'init-locales)

(require 'init-builtin)
(require 'init-enhance)
(require 'init-windows)

(require 'init-corfu)
(require 'init-flymake)

(require 'init-git)
(require 'init-grep)
(require 'init-ibuffer)
(require 'init-minibuffer)

(require 'init-eglot)
(require 'init-java)
(require 'init-dart)

(require 'init-org)
(require 'init-cxx)
(require 'init-web)
(require 'init-javascript)
(require 'init-nxml)
(require 'init-python)
(require 'init-textmodes)
(require 'init-progmodes)

(require 'init-http)
(require 'init-docker)

;;----------------------------------------------------------------------------
;; Load customize

(when (file-exists-p custom-file)
  (load custom-file))

(provide 'init)

;; Local Variables:
;; coding: utf-8
;; no-byte-compile: t
;; End:
;;; init.el ends here
