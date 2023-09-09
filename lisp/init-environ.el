;;; init-environ.el --- -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'ya-environ)

(when (or (memq window-system '(mac ns x))
          (unless (memq system-type '(windows-nt))
            (daemonp)))
  (let ((names '("PATH" "MANPATH"
                 "LANG" "LC_CTYPE"
                 "SSH_AUTH_SOCK" "SSH_AGENT_PID"
                 "GPG_AGENT_INFO")))
    (ya-environ-initialize names)))

(defvar auto-append-environ-path nil)

(after-aproject-change
 (dolist (value auto-append-environ-path)
   (let ((target (aproject-root-file value)))
     (when (file-directory-p target)
       (ya-environ-append-PATH target)))))

;; open emacs

(when window-system
  (defun open-new-emacs()
    "Open a new Emacs process."
    (interactive)
    (cond
     ((eq system-type 'darwin)
      (shell-command "open -n -a Emacs.app"))
     ((eq system-type 'windows-nt)
      (w32-shell-execute "open" (concat (file-name-directory (car command-line-args)) "runemacs.exe")))
     (t (call-process-shell-command (concat (car command-line-args) " & disown")))))

  (bind-key "M-g z" 'open-new-emacs))

(provide 'init-environ)
;;; init-environ.el ends here
