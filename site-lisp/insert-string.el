;;; insert-string.el --- -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(defconst insert-string--alphabet
  "abcdefghijklmnopqrstuvwxyz")

(defconst insert-string--characters
  (concat "0123456789" insert-string--alphabet))

(defun insert-string--backend(text-function)
  (insert
   (with-temp-buffer
     (funcall text-function)
     (copy-region-as-kill (point-min) (point-max))
     (buffer-substring-no-properties (point-min) (point-max)))))

(defun insert-random-string-custom (total)
  (interactive "nTotal: ")
  (insert-string--backend
   (lambda ()
     (let ((alphabet-size (length insert-string--alphabet))
           (characters-size (length insert-string--characters)))
       (insert (elt insert-string--alphabet (random alphabet-size)))
       (dotimes (_ (- total 1))
         (insert (elt insert-string--characters (random characters-size))))))))

(defun insert-random-string ()
  (interactive)
  (insert-random-string-custom 10))

(defun insert-string--encode-base (value base)
  (let ((index)
        (result ""))
    (while (> value 0)
      (setq index (% value base))
      (setq value (/ value base))
      (setq result (format "%c%s" (elt insert-string--characters index) result)))
    result))

(defun insert-ordered-string ()
  (interactive)
  (let ((ts (truncate (* (float-time) 1000000))))
    (insert-string--backend
     (lambda ()
       (let ((base (length insert-string--characters)))
         (insert (insert-string--encode-base ts base)))))))

(provide 'insert-string)
;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; flycheck-disabled-checkers: (emacs-lisp-checkdoc)
;; End:
;;; insert-string.el ends here
