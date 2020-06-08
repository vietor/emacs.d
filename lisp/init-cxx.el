;;; init-cxx.el --- -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

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
  (setq indent-tabs-mode t)
  (setq show-trailing-whitespace t)
  (c-add-style "Kernel" kernel-c-style t))

;; cxx mode

(defun cxx-mode-on ()
  "Set current buffer to cxx mode."
  (when (or (eq major-mode 'c-mode)
            (eq major-mode 'c++-mode))
    (kernel-set-c-style))
  (when indent-tabs-mode
    (setq tab-width c-basic-offset)))

(add-hook 'c-mode-common-hook 'cxx-mode-on)

(with-eval-after-load 'flycheck
  (setq-default flycheck-gcc-language-standard "c++11")
  (setq-default flycheck-clang-language-standard "c++11"))

(provide 'init-cxx)
;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
;;; init-cxx.el ends here
