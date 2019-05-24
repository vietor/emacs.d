;;; init.el --- -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; (setq debug-on-error t)

(let ((minver "25.1"))
  (when (version< emacs-version minver)
    (error "Your Emacs is too old -- this config requires v%s or higher" minver)))

(let ((init-threshold  (* 128 1024 1024))
      (target-threshold  (* 20 1024 1024)))
  (setq gc-cons-threshold init-threshold)
  (add-hook 'emacs-startup-hook
            (lambda () (setq gc-cons-threshold target-threshold))))

(defconst *is-os-mac* (eq system-type 'darwin))
(defconst *is-os-win* (eq system-type 'windows-nt))

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

(require 'init-locales)
(require 'init-packages)
(require 'init-environs)

(require 'init-builtin)
(require 'init-enhance)
(require 'init-windows)

(require 'init-flycheck)
(require 'init-company)
(require 'init-git)
(require 'init-searches)

(require 'init-cxx)
(require 'init-jvm)
(require 'init-web)
(require 'init-javascript)
(require 'init-programmes)

(when (file-exists-p custom-file)
  (load custom-file))

(provide 'init)
;; Local Variables:
;; coding: utf-8
;; no-byte-compile: t
;; End:
;;; init.el ends here
