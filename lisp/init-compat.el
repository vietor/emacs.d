(unless (fboundp 'buffer-major-mode)
  (defun buffer-major-mode (buffer)
    (buffer-local-value 'major-mode buffer)))

(if (fboundp 'with-eval-after-load)
    (defalias 'after-load 'with-eval-after-load)
  (defmacro after-load (feature &rest body)
    (declare (indent defun))
    `(eval-after-load ,feature
       '(progn ,@body))))

(unless (fboundp 'string-trim-left)
  (defun string-trim-left (s)
    (if (string-match "\\`[ \t\n\r]+" s)
        (replace-match "" t t s)
      s)))

(unless (fboundp 'string-trim-right)
  (defun string-trim-right (s)
    (if (string-match "[ \t\n\r]+\\'" s)
        (replace-match "" t t s)
      s)))

(unless (fboundp 'string-trim)
  (defun string-trim (s)
    (string-trim-left (string-trim-right s))))

(unless (fboundp 'string-prefix-p)
  (defun string-prefix-p (prefix string &optional ignore-case)
    (eq t (compare-strings prefix nil nil
                           string 0 (length prefix) ignore-case))))

(unless (fboundp 'string-suffix-p)
  (defun string-suffix-p (suffix string  &optional ignore-case)
    (let ((start-pos (- (length string) (length suffix))))
      (and (>= start-pos 0)
           (eq t (compare-strings suffix nil nil
                                  string start-pos nil ignore-case))))))

(provide 'init-compat)
