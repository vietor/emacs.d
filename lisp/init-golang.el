(when (executable-find "go")
  (require-package 'go-mode)

  (add-hook 'before-save-hook 'gofmt-before-save)
  (after-aproject-change
   (setenv "GOPATH" aproject-rootdir)))

(provide 'init-golang)
