;;; init-java.el --- -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package scala-mode)

(use-package groovy-mode
  :mode "\\.gradle\\'")

(use-package java
  :ensure nil
  :when (executable-find "java")
  :hook (java-mode . eglot-ensure)
  :init
  (defvar eclipse-jdt-vmargs
    '("-XX:+UseParallelGC" "-XX:GCTimeRatio=4" "-XX:AdaptiveSizePolicyWeight=90" "-Dsun.zip.disableMemoryMapping=true" "-Xmx1G" "-Xms100m"))

  (defun eclipse-jdt-contact (interactive)
    (let* ((install-dir
            (expand-file-name "eclipse.jdt.ls" user-emacs-space-directory))
           (plugin-dir
            (expand-file-name "plugins" install-dir))
           (config-dir
            (expand-file-name (cond ((string= system-type "darwin") "config_mac")
                                    ((string= system-type "windows-nt") "config_win")
                                    (t "config_linux"))
                              install-dir))
           (workspace-dir
            (expand-file-name (md5 (aproject-project-root))
                              (concat user-emacs-space-directory "eclipse.workspaces")))
           (launcher-jar nil))

      (unless (file-directory-p install-dir)
        (error "Not found 'eclipse.jdt.ls' directory in '%s'" user-emacs-space-directory))

      (setq launcher-jar
            (ignore-errors
              (car (directory-files plugin-dir t "org\\.eclipse\\.equinox\\.launcher_.*\\.jar$"))))
      (unless (and launcher-jar (file-exists-p launcher-jar))
        (error "Not found 'eclipse.jdt.ls' launcher jar"))

      (unless (file-directory-p workspace-dir)
        (make-directory workspace-dir t))

      (cons 'eglot-eclipse-jdt `(,(executable-find "java")
                                 "-Declipse.application=org.eclipse.jdt.ls.core.id1"
                                 "-Dosgi.bundles.defaultStartLevel=4"
                                 "-Declipse.product=org.eclipse.jdt.ls.core.product"
                                 ,@eclipse-jdt-vmargs
                                 "-jar" ,launcher-jar
                                 "-configuration" ,config-dir
                                 "-data" ,workspace-dir))))

  (add-to-list 'eglot-server-programs '(java-mode . eclipse-jdt-contact)))

(provide 'init-java)
;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
;;; init-java.el ends here
