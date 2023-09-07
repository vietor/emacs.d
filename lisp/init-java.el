;;; init-java.el --- -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package scala-mode
  :ensure t)

(use-package groovy-mode
  :ensure t
  :mode "\\.gradle\\'")

(use-package java
  :when (executable-find "java")
  :hook ((java-mode . eglot-ensure)
         (java-ts-mode . eglot-ensure))
  :init
  (defconst eclipse-jdt-vmargs '("-Xmx1G" "-Xms100m"))

  (defconst eclipse-jdt-code-style-file-url (concat "file://" (if (eq system-type 'windows-nt) "/")
                                                    (expand-file-name "etc/eclipse-java-google-style.xml" user-emacs-directory)))

  (defclass eglot-eclipse-jdt (eglot-lsp-server) ()
    :documentation "Eclipse's Java Development Tools Language Server.")

  (cl-defmethod eglot-initialization-options ((_server eglot-eclipse-jdt))
    "Passes through required jdt initialization options."
    `(:settings
      (:java
       (:format
        (:settings
         (:url ,eclipse-jdt-code-style-file-url))))))

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
           (lombok-jar nil)
           (runtime-jdt-vmargs (append '() eclipse-jdt-vmargs)))

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
        (push (concat "-javaagent:" lombok-jar) runtime-jdt-vmargs))

      (unless (file-directory-p workspace-dir)
        (make-directory workspace-dir t))

      (cons 'eglot-eclipse-jdt `(,(executable-find "java")
                                 "-Declipse.application=org.eclipse.jdt.ls.core.id1"
                                 "-Dosgi.bundles.defaultStartLevel=4"
                                 "-Declipse.product=org.eclipse.jdt.ls.core.product"
                                 "-Dosgi.checkConfiguration=true"
			                     ,(concat "-Dosgi.sharedConfiguration.area=" config-dir)
			                     "-Dosgi.sharedConfiguration.area.readOnly=true"
			                     "-Dosgi.configuration.cascaded=true"
                                 "--add-modules=ALL-SYSTEM"
                                 "--add-opens" "java.base/java.util=ALL-UNNAMED"
                                 "--add-opens" "java.base/java.lang=ALL-UNNAMED"
                                 ,@runtime-jdt-vmargs
                                 "-jar" ,launcher-jar
                                 "-data" ,workspace-dir))))

  (add-to-list 'eglot-server-programs '((java-mode java-ts-mode) . eclipse-jdt-contact)))

(provide 'init-java)
;;; init-java.el ends here
