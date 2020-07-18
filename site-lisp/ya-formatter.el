;;; ya-formatter.el --- -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(defvar ya-formatter-enabled-modes
  '(nxml-mode))

(defvar ya-formatter-disabled-modes
  '(sql-mode text-mode shell-mode eshell-mode term-mode))

(defvar ya-formatter-beautify-alist nil)

(defun ya-formatter-indent ()
  "Reformat current buffer by indent."
  (interactive)
  (when (or (apply 'derived-mode-p ya-formatter-enabled-modes)
            (not (apply 'derived-mode-p ya-formatter-disabled-modes)))
    (delete-trailing-whitespace)
    (indent-region (point-min) (point-max))))

(defun ya-formatter-beautify ()
  "Reformat current buffer by customize or indent."
  (interactive)
  (let ((beautify (cdr (assoc major-mode ya-formatter-beautify-alist))))
    (if (not beautify)
        (ya-formatter-indent)
      (let ((c-point (point))
            (w-start (window-start)))
        (funcall beautify)
        (goto-char c-point)
        (goto-char (line-beginning-position))
        (set-window-start (selected-window) w-start)))))

(provide 'ya-formatter)
;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; flycheck-disabled-checkers: (emacs-lisp-checkdoc)
;; End:
;;; ya-formatter.el ends here
