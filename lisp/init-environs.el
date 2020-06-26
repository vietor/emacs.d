;;; init-environs.el --- -*- lexical-binding: t -*-
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
