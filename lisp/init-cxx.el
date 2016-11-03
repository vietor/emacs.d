(require 'kernel-c-style)
(require 'google-c-style)

(c-add-style "kernel" kernel-c-style)
(c-add-style "google" google-c-style)

(defadvice c-set-style (after tab-width activate)
  (when indent-tabs-mode
    (setq tab-width c-basic-offset)))

(defvar cxx-mode nil)
(make-variable-buffer-local 'cxx-mode)
(defvar cxx-mode-hook '()
  "Hooks to run after c/c++'s mode")

(defvar cxx-style "kernel")

(defun cxx-mode-setup ()
  (unless cxx-mode
    (setq cxx-mode t)
    (c-set-style cxx-style)
    (run-hooks 'cxx-mode-hook)))

(add-hook 'c-mode-hook
          (lambda ()
            (let ((file-name (downcase buffer-file-name)))
              (when (or (string-suffix-p ".h" file-name)
                        (string-suffix-p ".c" file-name))
                (cxx-mode-setup)))))

(add-hook 'c++-mode-hook 'cxx-mode-setup)

(provide 'init-cxx)
