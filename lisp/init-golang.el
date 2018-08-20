(when (executable-find "go")
  (require-package 'go-mode)

  (after-aproject-change
   (setenv "GOPATH" aproject-rootdir))
  (add-hook 'before-save-hook 'gofmt-before-save))

(provide 'init-golang)
