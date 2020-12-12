;;; init-avy.el --- avy navigation -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require-package 'avy)
(global-set-key (kbd "C-c j") 'avy-goto-word-or-subword-1)
(global-set-key (kbd "M-g f") 'avy-goto-line)


(provide 'init-avy)
;;; init-avy.el ends here
