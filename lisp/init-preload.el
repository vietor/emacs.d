;;; init-preload.el --- -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'seq)
(require 'subr-x)

(defalias 'after-load 'with-eval-after-load)

(unless (fboundp 'buffer-major-mode)
  (defun buffer-major-mode (buffer)
    (buffer-local-value 'major-mode buffer)))

(provide 'init-preload)
;; Local Variables:
;; byte-compile-warnings: (not free-vars)
;; End:
;;; init-preload.el ends here
