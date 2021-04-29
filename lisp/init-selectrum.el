;;; init-selectrum.el --- -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package selectrum
  :hook (after-init . selectrum-mode)
  :config
  (setq-default selectrum-fix-vertical-window-height t))

(use-package selectrum-prescient
  :after selectrum
  :config
  (require 'prescient)
  (prescient-persist-mode 1)
  (selectrum-prescient-mode 1))

(use-package embark
  :after selectrum)

(use-package consult
  :after selectrum
  :bind (([remap switch-to-buffer] . consult-buffer)
         ([remap switch-to-buffer-other-window] . consult-buffer-other-window)
         ([remap switch-to-buffer-other-frame] . consult-buffer-other-frame))
  :config
  (setq-default consult-project-root-function 'aproject-current))

(use-package embark-consult
  :after (embark consult)
  :hook (embark-collect-mode . embark-consult-preview-minor-mode))

(use-package marginalia
  :hook (after-init . marginalia-mode)
  :config
  (setq-default marginalia-annotators '(marginalia-annotators-heavy)))

(provide 'init-selectrum)
;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
;;; init-selectrum.el ends here
