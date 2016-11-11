(let ((parent-dir (expand-file-name "site-lisp/" user-emacs-directory)))
  (dolist (dir (directory-files parent-dir))
    (unless (string-match "^\\." dir)
      (add-to-list 'load-path (expand-file-name dir parent-dir)))))

(require 'package)
(add-to-list 'package-archives
  `("melpa" . "http://melpa.org/packages/"))
(setq package-enable-at-startup nil)
(package-initialize)

(defun require-package (package &optional min-version no-refresh)
  (if (package-installed-p package min-version)
      t
    (if (or (assoc package package-archive-contents) no-refresh)
        (if (boundp 'package-selected-packages)
            (package-install package nil)
          (package-install package))
      (progn
        (package-refresh-contents)
        (require-package package min-version t)))))

(defun maybe-require-package (package &optional min-version no-refresh)
  (condition-case err
      (require-package package min-version no-refresh)
    (error
     (message "Couldn't install package `%s': %S" package err)
     nil)))

(provide 'init-package)
