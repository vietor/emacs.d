(when (executable-find "scala")
  (require-package 'scala-mode))

(when (executable-find "sbt")
  (require-package 'sbt-mode))

(provide 'init-scala)
