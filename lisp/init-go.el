;;; init-go.el --- Support for working with Go -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package go-mode
  :ensure t
  :bind (("C-c C-f C-f" . gofmt))
  :config
  (defun my-go-mode-hook ()
    (setq tab-width 2))

  (add-hook 'go-mode-hook 'my-go-mode-hook))

;; (use-package go-rename
;;   :after go-mode)

(use-package company-go
  :after company
  :hook (go-mode . (lambda ()
                      (set (make-local-variable 'company-backends) '(company-go))
                      (company-mode))))

(provide 'init-go)
;;; init-go.el ends here
