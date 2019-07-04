;;; init-packages.el --- -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;;; package

(require 'package)

(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)
(setq package-user-dir
      (expand-file-name (format "elpa-%s.%s" emacs-major-version emacs-minor-version)
                        user-emacs-directory))

(defun use-package (package &optional no-refresh)
  "Install given PACKAGE, If NO-REFRESH is non-nil, not refresh package lists."
  (when (not(package-installed-p package))
    (if (or (assoc package package-archive-contents) no-refresh)
        (package-install package)
      (progn
        (package-refresh-contents)
        (use-package package t)))))

(defun try-use-package (package &optional no-refresh)
  "Try install given PACKAGE, If NO-REFRESH is non-nil, not refresh package lists."
  (condition-case err
      (progn (use-package package no-refresh) t)
    (error
     (message "Couldn't install optional package `%s': %S" package err)
     nil)))

(setq package-enable-at-startup nil)
(package-initialize)

;;; necessary functons
(require 'seq)
(require 'subr-x)

(defalias 'after-load 'with-eval-after-load)

(unless (fboundp 'buffer-major-mode)
  (defun buffer-major-mode (buffer)
    (buffer-local-value 'major-mode buffer)))

;;; necessary packages

(use-package 'aproject)
(use-package 'diminish)
(use-package 'fullframe)

(global-set-key (kbd "C-x p") 'aproject-change-project)

(provide 'init-packages)
;; Local Variables:
;; byte-compile-warnings: (not free-vars)
;; End:
;;; init-packages.el ends here
