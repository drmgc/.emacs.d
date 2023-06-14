;;; init-go.el --- Support for working with Go -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package go-mode
  :after lsp
  :ensure t
  :bind (("C-c C-f C-f" . gofmt))
  :config
  ;; (add-hook 'go-mode-hook #'lsp-deferred)

  (defun lsp-go-install-save-hooks ()
    (add-hook 'before-save-hook #'lsp-format-buffer t t)
    (add-hook 'before-save-hook #'lsp-organize-imports t t))
  (add-hook 'go-mode-hook #'lsp-go-install-save-hooks))

;; (use-package go-rename
;;   :after go-mode)

;; (use-package company-go
;;   :after company
;;   :hook (go-mode . (lambda ()
;;                       (set (make-local-variable 'company-backends) '(company-go))
;;                       (company-mode))))

(provide 'init-go)
;;; init-go.el ends here
