;;; init-haskell.el --- Haskell stuff -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package haskell-mode
  :hook ((haskell-mode . flycheck-mode)))

(use-package dante
  :after haskell-mode
  :commands 'dante-mode
  :hook (haskell-mode . dante-mode)
  :config
  (flycheck-add-next-checker 'haskell-dante '(info . haskell-hlint)))

(provide 'init-haskell)
;;; init-haskell.el ends here
