;;; init-el.el --- Support for Nim -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package nim-mode
  :hook
  (nim-mode . (lambda ()
                (setq-local drmgc/eol-symbols-ring '(":" ","))
                (lsp-deferred)
                )))

(provide 'init-nim)
;;; init-nim.el ends here
