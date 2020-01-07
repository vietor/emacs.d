;;; init-packages.el --- -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

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

;; Fire up

(package-initialize)

(when (fboundp 'package--save-selected-packages)
  (defun fix-package-selected-packages()
    (package--save-selected-packages
     (seq-uniq (append used-packages
                       package-selected-packages))))
  (add-hook 'after-init-hook 'fix-package-selected-packages))

;;; necessary packages

(use-package 'diminish)
(use-package 'fullframe)

(use-package 'aproject)
(after-load 'aproject
  (setq aproject-plugin-environ t)
  (global-set-key (kbd "C-x p") 'aproject-change-project))

(provide 'init-packages)
;; Local Variables:
;; byte-compile-warnings: (not free-vars)
;; End:
;;; init-packages.el ends here
