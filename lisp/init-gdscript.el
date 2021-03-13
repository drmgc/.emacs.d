;;; init-gdscript.el --- GDScript support -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


(use-package gdscript-mode
  :after (lsp-mode)
  :config
  (add-hook 'gdscript-mode-hook
            '(lambda () (set (make-local-variable 'yas-indent-line) 'fixed)))
  (setq gdscript-use-tab-indents t)
  (setq gdscript-indent-offset 4)

  :hook (gdscript-mode . lsp))

(provide 'init-gdscript)
;;; init-gdscript.el ends here
