;;; init-lsp.el --- LSP support -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package lsp-mode
  :init
  (setq lsp-keymap-prefix "C-l")
  ;; (setq lsp-disabled-clients '(eslint))
  (setq lsp-prefer-capf t)
  :config
  (setq lsp-print-io nil)
  (setq lsp-log-io nil)
  (advice-add 'json-parse-string :around
              (lambda (orig string &rest rest)
                (apply orig (s-replace "\\u0000" "" string)
                       rest)))
  (advice-add 'json-parse-buffer :around
              (lambda (orig &rest rest)
                (while (re-search-forward "\\u0000" nil t)
                  (replace-match ""))
                (apply orig rest)))
  :hook ((lsp-mode . lsp-enable-which-key-integration)
         (csharp-mode . lsp-deferred)
         (c-mode . lsp-deferred)
         (js-mode . lsp-deferred)
         (typescript-mode . lsp-deferred)
         (go-mode . lsp-deferred)
         ;; (web-mode . lsp-deferred)
         (haskell-mode . lsp-deferred)
         ))


(use-package lsp-ui :commands lsp-ui-mode)
(use-package lsp-ivy :commands lsp-ivy-workspace-symbol)

(provide 'init-lsp)
;;; init-lsp.el ends here
