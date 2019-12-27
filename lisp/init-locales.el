;;; init-locales.el --- -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(setq system-time-locale "C")

(defun utf8-locale-p (v)
  "Test string V match UTF-8."
  (and v (or (string-match-p "UTF8" v) (string-match-p "UTF-8" v))))

(dolist (varname '("LC_ALL" "LANG" "LC_CTYPE"))
  (unless (utf8-locale-p (getenv varname))
    (message "Warning: non-UTF8 encoding in environment variable %s." varname)))

(when (fboundp 'set-charset-priority)
  (set-charset-priority 'unicode))
(prefer-coding-system 'utf-8)
(setq locale-coding-system 'utf-8)
(unless (eq system-type 'windows-nt)
  (set-selection-coding-system 'utf-8))

(provide 'init-locales)
;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
;;; init-locales.el ends here
