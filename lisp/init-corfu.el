;;; init-corfu.el --- -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(setq tab-always-indent 'complete)
(add-to-list 'completion-styles 'initials t)

(use-package corfu
  :hook (after-init . global-corfu-mode)
  :config
  (setq-default corfu-auto t
                corfu-preview-current nil
                corfu-quit-no-match 'separator))

(provide 'init-corfu)
;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
;;; init-corfu.el ends here
