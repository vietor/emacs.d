;;; a-environ.el --- -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(defun a-environ--shell ()
  "Return the shell to use."
  (or shell-file-name
      (getenv "SHELL")
      (error "SHELL environment variable is unset")))

(defun a-environ--process-lines (command)
  "Return list from execute COMMAND by shell; Return nil if an error occured."
  (let* ((shell (a-environ--shell))
         (shell-args (append
                      (cond
                       ((string-match-p "t?csh$" shell)
                        (list "-d"))
                       ((string-match-p "fish" shell)
                        (list "-l"))
                       (t (list "-l" "-i")))
                      (list "-c" command))))
    (condition-case nil
        (apply #'process-lines shell shell-args)
      (error nil))))

(defun a-environ--copy-envs (command names)
  "Return the environment variables alist with NAMES from execute COMMAND by shell."
  (let ((name) (value) (target))
    (dolist (line (a-environ--process-lines command))
      (string-match "=" line)
      (setq name (substring line 0 (match-beginning 0)))
      (setq value (substring line (match-end 0)))
      (when (member name names)
        (push (cons name value) target)))
    target))

(defun a-environ--setenv (name value)
  "Set the value of environment var NAME to VALUE.
Additionally, if NAME is \"PATH\" then also set corresponding
variables such as `exec-path'."
  (setenv name value)
  (when (string-equal "PATH" name)
    (setq eshell-path-env value
          exec-path (append (parse-colon-path value) (list exec-directory)))))

(defun a-environ-initialize (names)
  "Initialize environment variables alist with NAMES from the user's shell."
  (dolist (pair (a-environ--copy-envs "env" names))
    (a-environ--setenv (car pair) (cdr pair))))

(defun a-environ-append-PATH (value)
  "Append VALUE to $PATH."
  (let ((news (parse-colon-path value))
        (olds (parse-colon-path (getenv "PATH"))))
    (a-environ--setenv "PATH" (string-join (delete-dups (append news olds)) path-separator))))

(provide 'a-environ)
;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; flycheck-disabled-checkers: (emacs-lisp-checkdoc)
;; End:
;;; a-environ.el ends here
