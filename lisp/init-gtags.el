(require 'agtags)
(require 'agtags-xref)

(defun gtags-update-label ()
  (let* ((exe-ctags (executable-find "ctags"))
         (exe-uctags (and ctags (with-temp-buffer
                                  (call-process "ctags" nil t nil "--version")
                                  (goto-char (point-min))
                                  (looking-at "Universal Ctags"))))
         (label (cond (exe-uctags "new-ctags")
                      ((and exe-ctags (executable-find "pygmentize")) "pygments")
                      (exe-ctags "ctags")
                      (t "default"))))
    (setenv "GTAGSLABEL" label)))

(when (executable-find "global")
  (defun agtags-mode-on()
    (agtags-mode 1)
    (diminish 'agtags-mode))

  (setq agtags-global-treat-text t)

  (agtags-bind-keys)
  (add-hook 'text-mode-hook 'agtags-mode-on)
  (add-hook 'prog-mode-hook 'agtags-mode-on)
  (add-to-list 'xref-backend-functions 'agtags-xref-backend)

  (agtags-update-parser)
  (add-hook 'aproject-environ-change-hook 'agtags-update-parser)
  (after-aproject-change (agtags-update-root aproject-rootdir)))

(provide 'init-gtags)
