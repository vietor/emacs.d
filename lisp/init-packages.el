;;; init-packages.el --- -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'seq)
(require 'subr-x)

;;; package

(require 'package)

(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)
(setq package-user-dir
      (expand-file-name (format "elpa-%s.%s" emacs-major-version emacs-minor-version)
                        user-emacs-directory))

(defvar used-packages nil)

(defun use-package (package &optional do-require no-refresh)
  "Install given PACKAGE, If DO-REQUIRE is non-nil `require` it, If NO-REFRESH is non-nil, not refresh package lists."
  (if (package-installed-p package)
      (when do-require (require package))
    (if (or (assoc package package-archive-contents) no-refresh)
        (progn
          (package-install package)
          (when do-require (require package))
          (add-to-list 'used-packages package))
      (progn
        (package-refresh-contents)
        (use-package package do-require t)))))

(defun try-use-package (package &optional do-require no-refresh)
  "Try install given PACKAGE, If DO-REQUIRE is non-nil `require` it, If NO-REFRESH is non-nil, not refresh package lists."
  (condition-case err
      (progn (use-package package do-require no-refresh) t)
    (error
     (message "Couldn't install optional package `%s': %S" package err)
     nil)))

(setq package-enable-at-startup nil)
(package-initialize)

(when (fboundp 'package--save-selected-packages)
  (add-hook 'after-init-hook
            (lambda () (package--save-selected-packages
                   (seq-uniq (append used-packages package-selected-packages))))))

;;; necessary functons

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
