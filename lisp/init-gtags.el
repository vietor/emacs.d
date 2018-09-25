(require 'agtags)
(require 'agtags-xref)

(defun executable-universal-ctags()
  (with-temp-buffer
    (call-process "ctags" nil t nil "--version")
    (goto-char (point-min))
    (looking-at "Universal Ctags")))

(defun gtags-label-parser ()
  (cond
   ((and (executable-find "ctags") (executable-universal-ctags))
    (setenv "GTAGSLABEL" "new-ctags"))
   ((and (executable-find "ctags") (executable-find "pygmentize"))
    (setenv "GTAGSLABEL" "pygments"))
   ((executable-find "ctags")
    (setenv "GTAGSLABEL" "ctags"))))

(when (executable-find "global")
  (agtags-bind-keys)
  (setq agtags-auto-dwim t)
  (setq agtags-global-treat-text t)
  (add-hook 'prog-mode-hook
            (lambda ()
              (agtags-mode 1)
              (diminish 'agtags-mode)))
  (add-to-list 'xref-backend-functions 'agtags-xref-backend)

  (after-aproject-change
   (setenv "GTAGSROOT" aproject-rootdir))

  (gtags-label-parser)
  (add-hook 'aproject-environ-change-hook 'gtags-label-parser))

(provide 'init-gtags)
