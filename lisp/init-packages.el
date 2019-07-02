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

(defun want-package (package &optional req)
  "Install given PACKAGE, require it when REQ."
  (when (not (package-installed-p package))
    (unless (assoc package package-archive-contents)
      (package-refresh-contents))
    (package-install package))
  (when req (require package)))

(defun try-want-package (package &optional req)
  "Try install given PACKAGE, require it when REQ."
  (condition-case err
      (want-package package req)
    (error
     (message "Couldn't install optional package `%s': %S" package err)
     nil)))

(setq package-pinned-packages '())
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

(want-package 'aproject)
(want-package 'diminish)
(want-package 'fullframe)

(global-set-key (kbd "C-x p") 'aproject-change-project)

(provide 'init-packages)
;; Local Variables:
;; byte-compile-warnings: (not free-vars)
;; End:
;;; init-packages.el ends here
