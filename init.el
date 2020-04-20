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

(let ((init-threshold  (* 128 1024 1024))
      (target-threshold  (* 20 1024 1024)))
  (setq gc-cons-threshold init-threshold)
  (add-hook 'emacs-startup-hook
            (lambda () (setq gc-cons-threshold target-threshold))))

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

(require 'init-org)
(require 'init-ivy)
(require 'init-flycheck)
(require 'init-company)
(require 'init-git)
(require 'init-searches)

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
