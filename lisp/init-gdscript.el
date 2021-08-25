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

(defun lsp--gdscript-ignore-errors (original-function &rest args)
  "Ignore the error message resulting from Godot not replying to the `JSONRPC' request."
  (if (string-equal major-mode "gdscript-mode")
      (let ((json-data (nth 0 args)))
        (if (and (string= (gethash "jsonrpc" json-data "") "2.0")
                 (not (gethash "id" json-data nil))
                 (not (gethash "method" json-data nil)))
            nil ; (message "Method not found")
          (apply original-function args)))
    (apply original-function args)))
;; Runs the function `lsp--gdscript-ignore-errors` around `lsp--get-message-type` to suppress unknown notification errors.
(advice-add #'lsp--get-message-type :around #'lsp--gdscript-ignore-errors)

(provide 'init-gdscript)
;;; init-gdscript.el ends here
