(setq-default grep-highlight-matches t
              grep-scroll-output t)

(when os-mac
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
  (defun ag-project-root-at-aproject (filepath)
    (if aproject-project aproject-rootdir filepath))
  (setq-default ag-project-root-function 'ag-project-root-at-aproject))
