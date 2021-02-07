;;; init-php.el --- Support for working with PHP -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(when (maybe-require-package 'php-mode)
  (maybe-require-package 'smarty-mode)

  (add-hook 'php-mode-hook '(lambda ()
                              (setq tab-width 4
                                    indent-tabs-mode t)))
  (add-hook 'php-mode-hook 'smart-tabs-mode-enable)

  (when (maybe-require-package 'company-php)
    (with-eval-after-load 'company
      (add-to-list 'company-backends 'company-ac-php-backend))))

(provide 'init-php)
;;; init-php.el ends here
