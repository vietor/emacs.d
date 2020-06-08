;;; init-environs.el --- -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; from shell

(require-package 'exec-path-from-shell)

(with-eval-after-load 'exec-path-from-shell
  (dolist (var '("SSH_AUTH_SOCK" "SSH_AGENT_PID" "GPG_AGENT_INFO" "LANG" "LC_CTYPE"))
    (add-to-list 'exec-path-from-shell-variables var)))

(when (or (memq window-system '(mac ns x))
          (unless (memq system-type '(windows-nt))
            (daemonp)))
  (exec-path-from-shell-initialize))

;; from file

(defun append-exec-path(value)
  "Append VALUE to `'exec-path`' and $PATH."
  (let ((new-path (split-string value path-separator))
        (env-path (split-string (getenv "PATH") path-separator)))
    (setq exec-path (delete-dups (append new-path exec-path)))
    (setenv "PATH" (string-join (delete-dups (append new-path env-path)) path-separator))))

(defvar auto-append-exec-path nil)

(after-aproject-change
 (dolist (value auto-append-exec-path)
   (let ((target (aproject-root-file value)))
     (when (file-directory-p target)
       (append-exec-path target)))))

(defun try-load-environ-from-file (file)
  "Try load environment from FILE."
  (when (file-exists-p file)
    (dolist (line (with-temp-buffer (insert-file-contents file) (split-string (buffer-string) "\n" t)))
      (setq line (string-trim line))
      (cond
       ((string= "" line) nil)
       ((string-prefix-p "#" line) nil)
       ((string-match-p "=" line 1)
        (string-match "=" line 1)
        (let ((name (substring line 0 (match-beginning 0)))
              (value (substring line (match-end 0))))
          (if (not (string-equal "PATH" name))
              (setenv name value)
            (append-exec-path value))))
       (t nil)))))

(try-load-environ-from-file (expand-file-name "setenv" user-emacs-directory))

;; open emacs

(when window-system
  (defun open-new-emacs()
    "Open a new Emacs process."
    (interactive)
    (let ((program (car command-line-args)))
      (cond
       (system-is-mac
        (shell-command (concat "open -n -a " program)))
       (system-is-win
        (w32-shell-execute "open" (concat (file-name-directory program) "runemacs.exe")))
       (t (message "Couldn't support to start a new Emacs")))))

  (global-set-key (kbd "M-g z") 'open-new-emacs))

(provide 'init-environs)
;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
;;; init-environs.el ends here
