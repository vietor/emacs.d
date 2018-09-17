(setq-default grep-highlight-matches t
              grep-scroll-output t)

(when *is-os-mac*
  (setq-default locate-command "mdfind"))

;; wgrep
(require-package 'wgrep)
(setq wgrep-enable-key "e")
(setq wgrep-auto-save-buffer t)

;; ag
(when (executable-find "ag")
  (require-package 'ag)
  (require-package 'wgrep-ag)
  (setq-default ag-reuse-window t)
  (setq-default ag-reuse-buffers t)
  (setq-default ag-highlight-search t)
  (setq-default ag-project-root-function
    (lambda (filepath) (if aproject-project aproject-rootdir filepath)))
  (global-set-key (kbd "M-?") 'ag-project))

(provide 'init-grep)
