;;; init-csharp.el --- Support for working with C Sharp -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require-package 'omnisharp)

(eval-after-load
  'company
  '(add-to-list 'company-backends #'company-omnisharp))

(require-package 'csharp-mode)

(add-hook 'csharp-mode-hook #'company-mode)
(add-hook 'csharp-mode-hook #'flycheck-mode)

(add-hook 'csharp-mode-hook
          (lambda ()
            (omnisharp-mode)

            (setq indent-tabs-mode t)
            (setq c-basic-offset 4)
            (setq truncate-lines t)
            (setq tab-width 4)

            (define-key csharp-mode-map (kbd "C-c f") 'omnisharp-code-format-region)
            (define-key csharp-mode-map (kbd "C-c C-f") 'omnisharp-code-format-entire-file)
            (define-key csharp-mode-map (kbd "C-c r") 'omnisharp-rename)
            (define-key csharp-mode-map (kbd "C-c u") 'omnisharp-find-usages)
            (define-key csharp-mode-map (kbd "C-c i") 'omnisharp-find-implementations)))

(add-to-list 'auto-mode-alist '("\\.cs$" . csharp-mode))

(provide 'init-csharp)
;;; init-csharp.el ends here
