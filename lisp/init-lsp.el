;;; init-lsp.el --- LSP support -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package lsp-mode
  :init
  (setq lsp-keymap-prefix "C-l")
  :hook ((lsp-mode . lsp-enable-which-key-integration)
         (csharp-mode . lsp-deferred)
         (c-mode . lsp-deferred)
         (typescript-mode . lsp-deferred)
         ;; (web-mode . lsp-deferred)
         (haskell-mode . lsp-deferred)
         ))


(use-package lsp-ui :commands lsp-ui-mode)
(use-package lsp-ivy :commands lsp-ivy-workspace-symbol)

(provide 'init-lsp)
;;; init-lsp.el ends here
