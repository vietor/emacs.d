(defun utf8-locale-p (v)
  (and v (string-match "UTF-8" v)))

(defun locale-is-utf8-p ()
  (or (utf8-locale-p (and (executable-find "locale") (shell-command-to-string "locale")))
      (utf8-locale-p (getenv "LC_ALL"))
      (utf8-locale-p (getenv "LC_CTYPE"))
      (utf8-locale-p (getenv "LANG"))))

(when (or window-system (locale-is-utf8-p))
  (set-language-environment 'utf-8)
  (setq locale-coding-system 'utf-8)
  (set-selection-coding-system (if *is-os-win* 'utf-16-le 'utf-8))
  (prefer-coding-system 'utf-8))

(provide 'init-locales)
