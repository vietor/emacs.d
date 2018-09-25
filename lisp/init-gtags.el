(require 'agtags)

(when (executable-find "global")
  (agtags-bind-keys)
  (setq agtags-auto-dwim t)
  (setq agtags-global-treat-text t)
  (add-hook 'prog-mode-hook
            (lambda ()
              (agtags-mode 1)
              (diminish 'agtags-mode))))

(after-aproject-change
 (setenv "GTAGSROOT" aproject-rootdir))

(defun gtags-label-parser ()
  (cond
   ((and (executable-find "pygmentize") (executable-find "ctags"))
    (setenv "GTAGSLABEL" "pygments"))
   ((executable-find "ctags")
    (setenv "GTAGSLABEL" "ctags"))))

(gtags-label-parser)
(add-hook 'aproject-environ-change-hook 'gtags-label-parser)

(require 'agtags-xref)
(add-to-list 'xref-backend-functions 'agtags-xref-backend)

(provide 'init-gtags)
