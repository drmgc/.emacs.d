;;; init-csharp.el --- Support for working with C Sharp -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require-package 'omnisharp)

(eval-after-load
  'company
  '(add-to-list 'company-backends #'company-omnisharp))


(add-hook 'csharp-mode-hook #'company-mode)
(add-hook 'csharp-mode-hook #'flycheck-mode)

(add-hook 'csharp-mode-hook
          (lambda ()
            (omnisharp-mode)

            (setq indent-tabs-mode t)
            (setq c-basic-offset 4)
            (setq truncate-lines t)
            (setq tab-width 4)))

(require-package 'csharp-mode)
(add-to-list 'auto-mode-alist '("\\.cs$" . csharp-mode))

(provide 'init-csharp)
;;; init-csharp.el ends here
