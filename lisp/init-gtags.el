(require 'gtags)

(defconst gtags-key-prefix "C-x g")
(setq gtags-select-buffer-single t)

(fmakunbound 'gtags-visit-rootdir)
(fmakunbound 'gtags-find-with-idutils)
(fmakunbound 'gtags-display-browser)
(defadvice gtags-get-rootpath (around from-env activate)
  (let ((rootpath-from-env (getenv "GTAGSROOT")))
    (if rootpath-from-env
        (setq ad-return-value rootpath-from-env)
      ad-do-it)))

(defun gtags-build-tags ()
  (interactive)
  (let ((rootpath (gtags-get-rootpath)))
    (dolist (file (list "GRTAGS" "GPATH" "GTAGS"))
      (ignore-errors
        (delete-file (expand-file-name file rootpath))))
    (with-temp-buffer
      (cd rootpath)
      (when (zerop (call-process (executable-find "gtags") nil t nil "-i"))
        (message "Tags create or update by GTAGS")))))

(unless (fboundp 'gtags-find-rtag-from-here)
  (defun gtags-find-rtag-from-here ()
    (interactive)
    (let (tagname)
      (setq tagname (gtags-current-token))
      (if (not tagname)
          nil
        (gtags-push-context)
        (gtags-goto-tag tagname "r")))))

(unless (fboundp 'gtags-find-symbol-from-here)
  (defun gtags-find-symbol-from-here ()
    (interactive)
    (let (tagname)
      (setq tagname (gtags-current-token))
      (if (not tagname)
          nil
        (gtags-push-context)
        (gtags-goto-tag tagname "s")))))

(unless (fboundp 'gtags-find-with-grep-from-here)
  (defun gtags-find-with-grep-from-here ()
    (interactive)
    (let (tagname)
      (setq tagname (gtags-current-token))
      (if (not tagname)
          nil
        (gtags-push-context)
        (gtags-goto-tag tagname (if gtags-grep-all-text-files "go" "g"))))))

(defun gtags-keybind-rebuild ()
  (setq gtags-select-mode-map (make-keymap))
  (suppress-keymap gtags-select-mode-map)
  (dolist (pair '(("p" . previous-line)
                  ("n" . next-line)
                  ("q" . gtags-pop-stack)
                  ("RET" . gtags-select-tag)))
    (define-key gtags-select-mode-map (kbd (car pair)) (cdr pair)))
  (setq gtags-mode-map (make-keymap))
  (dolist (pair '(("q" . gtags-pop-stack)
                  ("f" . gtags-find-file)
                  ("p" . gtags-find-with-grep)
                  ("P" . gtags-find-with-grep-from-here)
                  ("t" . gtags-find-tag)
                  ("T" . gtags-find-tag-from-here)
                  ("r" . gtags-find-rtag)
                  ("R" . gtags-find-rtag-from-here)
                  ("s" . gtags-find-symbol)
                  ("S" . gtags-find-symbol-from-here)
                  ("b" . gtags-build-tags)))
    (global-set-key (kbd (concat gtags-key-prefix " " (car pair))) (cdr pair))))

(when (executable-find "global")
  (after-load 'gtags
    (gtags-keybind-rebuild)
    (setq gtags-auto-update t)
    (add-hook 'prog-mode-hook
              (lambda ()
                (gtags-mode 1)
                (diminish 'gtags-mode)))))

(defadvice gtags-pop-stack (after empty-to-kill activate)
  (unless gtags-buffer-stack
    (gtags-select-kill-buffers)))

(after-aproject-change
 (setenv "GTAGSROOT" aproject-rootdir))

(defun gtags-label-parser ()
  (cond
   ((and (executable-find "pygmentize") (executable-find "ctags"))
    (setenv "GTAGSLABEL" "pygments"))
   ((executable-find "ctags")
    (setenv "GTAGSLABEL" "ctags"))
   (t
    (setenv "GTAGSLABEL" "native"))))

(gtags-label-parser)
(add-hook 'aproject-environ-change-hook 'gtags-label-parser)

(provide 'init-gtags)
