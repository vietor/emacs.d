;;; aformatter.el --- -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(defvar aformatter-enabled-modes
  '(nxml-mode))

(defvar aformatter-disabled-modes
  '(sql-mode text-mode shell-mode eshell-mode term-mode))

(defvar aformatter-beautify-alist nil)

(defun aformatter-indent ()
  "Reformat current buffer by indent."
  (interactive)
  (when (or (apply 'derived-mode-p aformatter-enabled-modes)
            (not (apply 'derived-mode-p aformatter-disabled-modes)))
    (delete-trailing-whitespace)
    (indent-region (point-min) (point-max))))

(defun aformatter-beautify ()
  "Reformat current buffer by customize or indent."
  (interactive)
  (let ((beautify (cdr (assoc major-mode aformatter-beautify-alist))))
    (if (not beautify)
        (aformatter-indent)
      (let ((c-point (point))
            (w-start (window-start)))
        (funcall beautify)
        (goto-char c-point)
        (goto-char (line-beginning-position))
        (set-window-start (selected-window) w-start)))))

(provide 'aformatter)
;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; flycheck-disabled-checkers: (emacs-lisp-checkdoc)
;; End:
;;; aformatter.el ends here
