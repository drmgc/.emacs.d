;;; init-cmake.el --- CMakeLists.txt editing -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require-package 'cmake-mode)

(add-hook 'cmake-mode-hook
          (lambda ()
            (drmgc/enable-tabs)
            (setq indent-tabs-mode t)
            (setq tab-width drmgc/tab-width)))

(provide 'init-cmake)
;;; init-cmake.el ends here
