;;; init-cmake.el --- CMakeLists.txt editing -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require-package 'cmake-mode)

(add-hook 'cmake-mode-hook
          (lambda ()
            (drmgc/enable-tabs)
            (setq indent-tabs-mode nil)
            (setq tab-width 2)
            (setq cmake-tab-width 2)))

(provide 'init-cmake)
;;; init-cmake.el ends here
