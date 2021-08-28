;;; init-rust.el --- Rust support -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package rustic
  :after (lsp-mode)
  :hook (rust-mode . (lambda ()
                       (setq indent-tabs-mode nil) ; Rust Style Guidelines 2.1
                       (setq rustic-format-on-save t)
                       (message "rust lambda"))))

(provide 'init-rust)
;;; init-rust.el ends here
