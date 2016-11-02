(custom-set-variables
  '(warning-suppress-types (quote ((alloc)))))

(defconst os-mac (eq system-type 'darwin))
(defconst os-mac-x (memq window-system '(mac ns x)))
(defconst os-windows (eq system-type 'windows-nt))

(require-package 'exec-path-from-shell)
(after-load 'exec-path-from-shell
  (dolist (var '("SSH_AUTH_SOCK" "SSH_AGENT_PID" "GPG_AGENT_INFO" "LANG" "LC_CTYPE"))
    (add-to-list 'exec-path-from-shell-variables var)))
(when os-mac-x
  (exec-path-from-shell-initialize))

(defun smart-setenv (name value)
  (if (not (string-equal "PATH" name))
      (setenv name value)
    (let ((paths (list value)))
      (setq exec-path (append paths exec-path))
      (setenv "PATH" (concat (mapconcat 'identity paths path-separator)
                             path-separator
                             (getenv "PATH"))))))

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
