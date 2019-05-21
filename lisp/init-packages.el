;;; init-packages.el --- -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;;; package

(require 'package)

(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)
(setq package-user-dir
      (expand-file-name (format "elpa-%s.%s" emacs-major-version emacs-minor-version)
                        user-emacs-directory))

(defun require-package (package &optional force)
  "Install given PACKAGE, required when FORCE."
  (when (not (package-installed-p package))
    (unless (assoc package package-archive-contents)
      (package-refresh-contents))
    (package-install package))
  (if (not force) t (require package)))

(defun maybe-require-package (package &optional force)
  "Try install given PACKAGE, required when FORCE."
  (condition-case err
      (require-package package force)
    (error
     (message "Couldn't install optional package `%s': %S" package err)
     nil)))

(setq package-pinned-packages '())
(setq package-enable-at-startup nil)
(package-initialize)

;;; necessary functons

(unless (featurep 'subr-x)
  (require 'subr-x))

(defalias 'after-load 'with-eval-after-load)

(unless (fboundp 'buffer-major-mode)
  (defun buffer-major-mode (buffer)
    (buffer-local-value 'major-mode buffer)))

;;; necessary packages

(require-package 'aproject)
(require-package 'diminish)
(require-package 'fullframe)

(global-set-key (kbd "C-x p") 'aproject-change-project)

(provide 'init-packages)
;; Local Variables:
;; byte-compile-warnings: (not free-vars)
;; End:
;;; init-packages.el ends here
