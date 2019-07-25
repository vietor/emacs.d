;;; init-org.el --- -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(setq org-directory "~/org/")

;; Capture

(define-key global-map (kbd "M-g o c") 'org-capture)

(setq org-default-notes-file (concat org-directory "gtd.org")
      org-capture-templates
      '(("t" "Todo" entry (file+headline "" "Tasks")
         "* TODO %?\n%U\n")
        ("j" "Journal" entry (file+datetree "journal.org")
         "* %U - %^{heading}\n  %?")))

;; Agenda

(define-key global-map (kbd "M-g o a") 'org-agenda)

(setq org-agenda-files (list org-directory)
      org-agenda-window-setup 'current-window)

;; Archiving

(setq org-archive-mark-done nil
      org-archive-location "%s_archive::* Archive")

(provide 'init-org)
;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
;;; init-org.el ends here
