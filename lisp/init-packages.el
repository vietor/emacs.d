;;; init-packages.el --- -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'seq)
(require 'subr-x)
(require 'package)

(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)
(setq package-user-dir
      (expand-file-name (format "elpa-%s.%s" emacs-major-version emacs-minor-version)
                        user-emacs-directory))

(when (fboundp 'package--save-selected-packages)
  (defun fix-save-selected-packages (&optional value)
    (when value
      (setq package-selected-packages value)))
  (advice-add 'package--save-selected-packages :override #'fix-save-selected-packages))

(defun require-package (package &optional do-require no-refresh)
  "Install given PACKAGE, If DO-REQUIRE is non-nil `require` it, If NO-REFRESH is non-nil, not refresh package lists."
  (if (package-installed-p package)
      (when do-require (require package))
    (if (or (assoc package package-archive-contents) no-refresh)
        (progn
          (package-install package)
          (when do-require (require package)))
      (progn
        (package-refresh-contents)
        (require-package package do-require t)))))

;; Fire up

(package-initialize)

;;; necessary packages

(require-package 'diminish)
(require-package 'fullframe)

(require-package 'aproject)
(with-eval-after-load 'aproject
  (setq aproject-plugin-environ t)
  (global-set-key (kbd "C-x p") 'aproject-change-project))

(provide 'init-packages)
;; Local Variables:
;; byte-compile-warnings: (not free-vars)
;; End:
;;; init-packages.el ends here
