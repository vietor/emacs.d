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
      c-lineup-arglist-tabs-only))))

(provide 'kernel-c-style)
