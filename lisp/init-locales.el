;;; init-locales.el --- -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(defun utf8-locale-p (v)
  "Test string V match UTF-8."
  (and v (string-match-p "UTF-8" v)))

(defun locale-is-utf8-p ()
  "Test local strings match UTF-8."
  (or (utf8-locale-p (and (executable-find "locale") (shell-command-to-string "locale")))
      (utf8-locale-p (getenv "LC_ALL"))
      (utf8-locale-p (getenv "LC_CTYPE"))
      (utf8-locale-p (getenv "LANG"))))

(setq system-time-locale "C")
(when (or window-system (locale-is-utf8-p))
  (set-language-environment 'utf-8)
  (setq locale-coding-system 'utf-8)
  (set-selection-coding-system (if system-is-win 'utf-16-le 'utf-8))
  (prefer-coding-system 'utf-8))

(provide 'init-locales)
;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
;;; init-locales.el ends here
