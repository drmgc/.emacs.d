;;; init-lsp.el --- LSP support -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package lsp-mode
  :init
  (setq lsp-keymap-prefix "C-c l")
  :hook
  (lsp-mode . lsp-enable-which-key-integration))


(use-package lsp-ui :commands lsp-ui-mode)
(use-package lsp-ivy :commands lsp-ivy-workspace-symbol)

(provide 'init-lsp)
;;; init-lsp.el ends here
