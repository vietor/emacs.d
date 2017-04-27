(unless (fboundp 'defvar-local)
  (defmacro defvar-local (var val &optional docstring)
    (declare (debug defvar) (doc-string 3))
    (list 'progn (list 'defvar var val docstring)
          (list 'make-variable-buffer-local (list 'quote var)))))

(unless (fboundp 'buffer-major-mode)
  (defun buffer-major-mode (buffer)
    (buffer-local-value 'major-mode buffer)))

(if (fboundp 'with-eval-after-load)
    (defalias 'after-load 'with-eval-after-load)
  (defmacro after-load (feature &rest body)
    (declare (indent defun))
    `(eval-after-load ,feature
       '(progn ,@body))))

(provide 'init-compat)
