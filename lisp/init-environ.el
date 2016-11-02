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

(provide 'init-system)
