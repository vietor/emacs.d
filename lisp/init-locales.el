(defun locale-is-utf8-p ()
  (or (string-match-p "UTF-8" (and (executable-find "locale") (shell-command-to-string "locale")))
      (string-match-p "UTF-8" (getenv "LC_ALL"))
      (string-match-p "UTF-8" (getenv "LC_CTYPE"))
      (string-match-p "UTF-8" (getenv "LANG"))))

(when (or window-system (locale-is-utf8-p))
  (setq utf-translate-cjk-mode nil)
  (set-language-environment 'utf-8)
  (setq locale-coding-system 'utf-8)
  (set-default-coding-systems 'utf-8)
  (set-terminal-coding-system 'utf-8)
  (unless os-windows
   (set-selection-coding-system 'utf-8))
  (prefer-coding-system 'utf-8))

(provide 'init-locales)
