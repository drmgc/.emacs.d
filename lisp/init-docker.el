;;; init-docker.el --- Support for the Docker -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package docker
  :bind ("C-c d" . docker))

(use-package dockerfile-mode
  :mode "Dockerfile\\'")

(provide 'init-docker)
;;; init-docker.el ends here
