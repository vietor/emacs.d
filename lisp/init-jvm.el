(when (executable-find "scala")
  (require-package 'scala-mode))

(when (executable-find "sbt")
  (require-package 'sbt-mode))

(when (executable-find "gradle")
  (require-package 'groovy-mode)
  (add-to-list 'auto-mode-alist '("\\.gradle\\'" . groovy-mode)))

(provide 'init-jvm)
