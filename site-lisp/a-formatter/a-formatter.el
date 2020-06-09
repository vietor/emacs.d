;;; a-formatter.el --- -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(defvar a-formatter-enabled-modes
  '(nxml-mode))

(defvar a-formatter-disabled-modes
  '(sql-mode text-mode shell-mode eshell-mode term-mode))

(defvar a-formatter-beautify-alist nil)

(defun a-formatter-indent ()
  "Reformat current buffer by indent."
  (interactive)
  (when (or (apply 'derived-mode-p a-formatter-enabled-modes)
            (not (apply 'derived-mode-p a-formatter-disabled-modes)))
    (delete-trailing-whitespace)
    (indent-region (point-min) (point-max))))

(defun a-formatter-beautify ()
  "Reformat current buffer by customize or indent."
  (interactive)
  (let ((beautify (cdr (assoc major-mode a-formatter-beautify-alist))))
    (if (not beautify)
        (a-formatter-indent)
      (let ((c-point (point))
            (w-start (window-start)))
        (funcall beautify)
        (goto-char c-point)
        (goto-char (line-beginning-position))
        (set-window-start (selected-window) w-start)))))

(provide 'a-formatter)
;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; flycheck-disabled-checkers: (emacs-lisp-checkdoc)
;; End:
;;; a-formatter.el ends here
