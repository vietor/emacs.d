(defun c-lineup-arglist-tabs-only (ignored)
  "Line up argument lists by tabs, not spaces"
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

;;;###autoload
(defun kernel-set-c-style ()
  "Set the current buffer's c-style to Linux Kernel C/C++ Programming
  Style. Meant to be added to `c-mode-common-hook'."
  (interactive)
  (setq indent-tabs-mode t)
  (setq show-trailing-whitespace t)
  (c-add-style "Kernel" kernel-c-style t))

(provide 'kernel-c-style)
