(require-package 'exec-path-from-shell)
(after-load 'exec-path-from-shell
  (dolist (var '("SSH_AUTH_SOCK" "SSH_AGENT_PID" "GPG_AGENT_INFO" "LANG" "LC_CTYPE"))
    (add-to-list 'exec-path-from-shell-variables var)))
(when (or (memq window-system '(mac ns x))
          (unless (memq system-type '(ms-dos windows-nt))
            (daemonp)))
  (exec-path-from-shell-initialize))

(defun smart-setenv (name value)
  (if (not (string-equal "PATH" name))
      (setenv name value)
    (let ((this-path) (paths (remove "" (split-string value path-separator))))
      (setq exec-path (delete-dups (append paths exec-path)))
      (setq this-path (concat (mapconcat 'identity paths path-separator) path-separator (getenv "PATH")))
      (setenv "PATH" (mapconcat 'identity (delete-dups (split-string this-path path-separator)) path-separator)))))

(defun startup-environ (file)
  (when (file-exists-p file)
    (dolist (line (with-temp-buffer
                    (insert-file-contents file)
                    (split-string (buffer-string) "\n" t)))
      (setq line (string-trim line))
      (cond
       ((string= "" line) nil)
       ((string-prefix-p "#" line) nil)
       ((string-match-p "=" line 1)
        (string-match "=" line 1)
        (let ((left (match-beginning 0)) (right (match-end 0)))
          (smart-setenv
           (substring line 0 left)
           (substring line right))))
       (t nil)))))

(startup-environ (expand-file-name "setenv" user-emacs-directory))

(provide 'init-environ)
