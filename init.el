;;; init.el --- -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; (setq debug-on-error t)

(let ((minver "25.1"))
  (when (version< emacs-version minver)
    (error "Your Emacs is too old -- this config requires v%s or higher" minver))
  (unless (memq system-type '(darwin windows-nt gnu/linux))
    (error "I am not working on the current operating system, bye")))
(unless (boundp 'early-init-file)
  (load (concat (file-name-directory load-file-name) "early-init") nil t))

(defconst system-is-mac (eq system-type 'darwin))
(defconst system-is-win (eq system-type 'windows-nt))

;;----------------------------------------------------------------------------
;; The `load-path' config

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(let ((default-directory (expand-file-name "site-lisp" user-emacs-directory)))
  (add-to-list 'load-path default-directory)
  (normal-top-level-add-subdirs-to-load-path))

;;----------------------------------------------------------------------------
;; Adjust garbage collection
(defvar gc-auto-timer (run-with-idle-timer 15 t #'garbage-collect))

(let ((large-threshold  (* 128 1024 1024))
      (target-threshold  (* 24 1024 1024))
      (origin-handler-alist file-name-handler-alist))
  (setq gc-cons-threshold large-threshold
        file-name-handler-alist nil)
  (add-hook 'emacs-startup-hook
            (lambda ()
              (setq gc-cons-threshold target-threshold
                    file-name-handler-alist origin-handler-alist)

              (if (boundp 'after-focus-change-function)
                  (add-function :after after-focus-change-function
                                (lambda ()
                                  (unless (frame-focus-state)
                                    (garbage-collect))))
                (add-hook 'focus-out-hook 'garbage-collect))

              (defun gc-minibuffer-setup-hook ()
                (setq gc-cons-threshold large-threshold))
              (defun gc-minibuffer-exit-hook ()
                (setq gc-cons-threshold target-threshold))
              (add-hook 'minibuffer-setup-hook #'gc-minibuffer-setup-hook)
              (add-hook 'minibuffer-exit-hook #'gc-minibuffer-exit-hook))))

;;----------------------------------------------------------------------------
;; Bootstrap config

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

;;----------------------------------------------------------------------------
;; Load configs

(require 'init-packages)
(require 'init-environs)
(require 'init-locales)

(require 'init-builtin)
(require 'init-enhance)
(require 'init-windows)

(require 'init-company)
(require 'init-flycheck)

(require 'init-ivy)
(require 'init-git)
(require 'init-grep)

(require 'init-org)
(require 'init-cxx)
(require 'init-web)
(require 'init-java)
(require 'init-javascript)
(require 'init-textmodes)
(require 'init-progmodes)

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
