;;; init-docker.el --- -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(when (executable-find "docker")
  (require-package 'docker)

  (fullframe docker-images tablist-quit)
  (fullframe docker-machines tablist-quit)
  (fullframe docker-volumes tablist-quit)
  (fullframe docker-networks tablist-quit)
  (fullframe docker-containers tablist-quit))

(require-package 'dockerfile-mode)
(add-to-list 'auto-mode-alist '("Dockerfile\\'" . dockerfile-mode))

(require-package 'docker-compose-mode)

(provide 'init-docker)
;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
;;; init-docker.el ends here
