;;; init-org.el --- -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(setq org-directory "~/org/")

(setq org-log-done t
      org-edit-timestamp-down-means-later t
      org-hide-emphasis-markers t
      org-catch-invisible-edits 'show
      org-export-coding-system 'utf-8
      org-fast-tag-selection-single-key 'expert
      org-html-validation-link nil
      org-export-kill-product-buffer-when-displayed t
      org-tags-column 80)

;; Capture

(define-key global-map (kbd "M-g o c") 'org-capture)

(setq org-default-notes-file (concat org-directory "gtd.org")
      org-capture-templates
      `(("t" "Todo" entry (file "")
         "* TODO %?\n%U\n" :clock-resume t)
        ("j" "Journal" entry (file+datetree "journal.org")
         "* %U - %^{heading}\n  %?")))

(setq org-todo-keywords
      `((sequence "TODO(t)" "NEXT(n)" "HOLD(h)" "|" "DONE(d)" "CANCELLED(c)"))
      org-todo-keyword-faces
      `(("NEXT" :inherit warning)))

;; Agenda

(define-key global-map (kbd "M-g o a") 'org-agenda)

(setq org-agenda-files (list org-directory)
      org-agenda-window-setup 'current-window
      org-agenda-compact-blocks t
      org-agenda-sticky t
      org-agenda-start-on-weekday nil
      org-agenda-span 'day
      org-agenda-include-diary nil)
(after-load 'org-agenda
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
