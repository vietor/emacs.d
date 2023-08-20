;;; init-docker.el --- -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package docker
  :ensure t
  :when (executable-find "docker")
  :config
  (fullframe docker-images tablist-quit)
  (fullframe docker-machines tablist-quit)
  (fullframe docker-volumes tablist-quit)
  (fullframe docker-networks tablist-quit)
  (fullframe docker-containers tablist-quit))

(use-package dockerfile-mode
  :ensure t
  :mode "Dockerfile\\'")

(use-package docker-compose-mode
  :ensure t)

(provide 'init-docker)
;;; init-docker.el ends here
