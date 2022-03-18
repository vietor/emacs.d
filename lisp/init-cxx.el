;;; init-cxx.el --- -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'google-c-style)

;; linux kernel style

(defun c-lineup-arglist-tabs-only (ignored)
  "Line up argument lists by tabs, not spaces, IGNORED."
  (let* ((anchor (c-langelem-pos c-syntactic-element))
         (column (c-langelem-2nd-pos c-syntactic-element))
         (offset (- (1+ column) anchor))
         (steps (floor offset c-basic-offset)))
    (* (max steps 1)
       c-basic-offset)))

(defconst kernel-c-style
  '("linux"
    (indent-tabs-mode . t)
    (c-offsets-alist
     (arglist-cont-nonempty
      c-lineup-gcc-asm-reg
      c-lineup-arglist-tabs-only)))
  "Linux Kernel C/C++ Programming Style.")

(defun kernel-set-c-style ()
  "Set the current buffer's c-style to Linux Kernel C/C++ Programming Style. Meant to be added to `c-mode-common-hook'."
  (interactive)
  (setq indent-tabs-mode t
        show-trailing-whitespace t)
  (c-add-style "Kernel" kernel-c-style t))

(add-hook 'c-mode-hook 'google-set-c-style)
(add-hook 'c++-mode-hook 'google-set-c-style)

(when (executable-find "clangd")
  (add-hook 'c-mode-hook 'eglot-ensure)
  (add-hook 'c++-mode-hook 'eglot-ensure))

(provide 'init-cxx)
;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
;;; init-cxx.el ends here
