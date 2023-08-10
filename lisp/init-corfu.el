;;; init-corfu.el --- -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(setq tab-always-indent 'complete)
(setq completion-category-defaults nil
      completion-category-overrides nil)
(setq completion-cycle-threshold 4)

(use-package orderless
  :config
  (setq completion-styles '(orderless basic)))

(use-package corfu
  :hook (after-init . global-corfu-mode)
  :config
  (setq-default corfu-auto t
                corfu-preview-current nil
                corfu-quit-no-match 'separator)

  (with-eval-after-load 'corfu
    (corfu-popupinfo-mode)))

(use-package cape
  :init
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-keyword))

(provide 'init-corfu)
;;; init-corfu.el ends here
