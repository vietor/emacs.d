;;; init-org.el --- -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(setq org-directory "~/Documents/org/"
      org-log-done 'time
      org-edit-timestamp-down-means-later t
      org-hide-emphasis-markers t
      org-catch-invisible-edits 'show
      org-export-coding-system 'utf-8
      org-fast-tag-selection-single-key 'expert
      org-html-validation-link nil
      org-export-kill-product-buffer-when-displayed t
      org-tags-column 80
      org-deadline-warning-days 7)

;; Capture

(define-key global-map (kbd "M-g o c") 'org-capture)

(setq org-default-notes-file (concat org-directory "gtd.org")
      org-capture-templates
      '(("t" "Todo" entry (file "")
         "* TODO %?\n%U\n")
        ("j" "Journal" entry (file+datetree "journal.org")
         "* %U - %^{heading}\n  %?")))

(setq org-todo-keywords
      '((sequence "TODO(t)" "START(s!)" "WAIT(w@)" "|" "DONE(d!)" "CANCELED(c@)"))
      org-todo-repeat-to-state "START"
      org-todo-keyword-faces
      '(("TODO" :inherit warning)
        ("START" :inherit font-lock-string-face)
        ("WAIT" :inherit error))
      org-enforce-todo-dependencies t
      org-enforce-todo-checkbox-dependencies t)

;; Agenda View

(define-key global-map (kbd "M-g o a") 'org-agenda)

(setq org-agenda-files (list org-directory)
      org-agenda-window-setup 'current-window
      org-agenda-compact-blocks t
      org-agenda-sticky t
      org-agenda-start-on-weekday nil
      org-agenda-span 'day
      org-agenda-include-diary nil
      org-agenda-sorting-strategy
      '((agenda habit-down time-up user-defined-up effort-up category-keep)
        (todo category-up effort-up)
        (tags category-up effort-up)
        (search category-up)))
(with-eval-after-load 'org-agenda
  (add-to-list 'org-agenda-after-show-hook 'org-show-entry)
  (add-hook 'org-agenda-mode-hook
            (lambda () (add-hook 'window-configuration-change-hook 'org-agenda-align-tags nil t))))

;; Archiving

(setq org-archive-mark-done nil
      org-archive-location "%s_archive::* Archive")

(provide 'init-org)
;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
;;; init-org.el ends here
