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
  (defconst eclipse-jdt-code-style-file-url (concat "file:///"
                                                    (expand-file-name "etc/eclipse-java-google-style.xml" user-emacs-directory)))

  (defvar eclipse-jdt-vmargs
    '("-XX:+UseParallelGC" "-XX:GCTimeRatio=4" "-XX:AdaptiveSizePolicyWeight=90" "-Dsun.zip.disableMemoryMapping=true" "-Xmx1G" "-Xms100m"))

  (defclass eglot-eclipse-jdt (eglot-lsp-server) ()
    :documentation "Eclipse's Java Development Tools Language Server.")

  (cl-defmethod eglot-initialization-options ((server eglot-eclipse-jdt))
    "Passes through required jdt initialization options."
    `(:settings
      (:java
       (:format
        (:settings
         (:profile "GoogleStyle" :url ,eclipse-jdt-code-style-file-url))))))

  (cl-defmethod eglot-execute-command ((_server eglot-eclipse-jdt)
                                       (_cmd (eql java.apply.workspaceEdit))
                                       arguments)
    (mapc #'eglot--apply-workspace-edit arguments))

  (defun eclipse-jdt--found (directory match-regexp &optional default-file)
    (or (ignore-errors
          (car (last (directory-files directory t match-regexp))))
        (if (not default-file)
            nil
          (expand-file-name default-file directory))))

  (defun eclipse-jdt-contact (interactive)
    (let* ((install-dir
            (eclipse-jdt--found user-emacs-space-directory "jdt-language-server-*" "eclipse.jdt.ls"))
           (config-dir
            (expand-file-name (cond ((string= system-type "darwin") "config_mac")
                                    ((string= system-type "windows-nt") "config_win")
                                    (t "config_linux"))
                              install-dir))
           (workspace-dir
            (expand-file-name (md5 (project-root (eglot--current-project)))
                              (concat user-emacs-space-directory "eclipse.workspaces")))
           (launcher-jar nil)
           (lombok-jar nil))

      (unless (file-directory-p install-dir)
        (error "Not found 'eclipse.jdt.ls' directory in '%s'" user-emacs-space-directory))

      (setq launcher-jar
            (eclipse-jdt--found (expand-file-name "plugins" install-dir)
                                "org\\.eclipse\\.equinox\\.launcher_.*\\.jar$"))
      (unless (and launcher-jar (file-exists-p launcher-jar))
        (error "Not found 'eclipse.jdt.ls' launcher jar"))

      (setq lombok-jar
            (eclipse-jdt--found (concat user-emacs-space-directory "eclipse.assists")
                                "lombok-.*\\.jar$"))
      (when (and lombok-jar (file-exists-p lombok-jar))
        (add-to-list 'eclipse-jdt-vmargs (concat "-javaagent:" lombok-jar)))

      (unless (file-directory-p workspace-dir)
        (make-directory workspace-dir t))

      (cons 'eglot-eclipse-jdt `(,(executable-find "java")
                                 "--add-modules=ALL-SYSTEM"
                                 "--add-opens" "java.base/java.util=ALL-UNNAMED"
                                 "--add-opens" "java.base/java.lang=ALL-UNNAMED"
                                 "-Declipse.application=org.eclipse.jdt.ls.core.id1"
                                 "-Dosgi.bundles.defaultStartLevel=4"
                                 "-Declipse.product=org.eclipse.jdt.ls.core.product"
                                 ,@eclipse-jdt-vmargs
                                 "-jar" ,launcher-jar
                                 "-configuration" ,config-dir
                                 "-data" ,workspace-dir))))

  (add-to-list 'eglot-server-programs '(java-mode . eclipse-jdt-contact))

  (defun java-workspace-configuration()
    (setq eglot-workspace-configuration
          `(("java.format.settings.url" . ,eclipse-jdt-code-style-file-url))))
  (add-to-list 'eglot-language-configuration-alist '("java" . java-workspace-configuration)))

(provide 'init-java)
;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
;;; init-java.el ends here
