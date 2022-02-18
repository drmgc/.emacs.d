;;; init-html.el --- Editing HTML -*- lexical-binding: t -*-
;;; Commentary:

;; ERB is configured separately in init-ruby

;;; Code:

;; (require-package 'tagedit)
;; (with-eval-after-load 'sgml-mode
;;   (tagedit-add-paredit-like-keybindings)
;;   (define-key tagedit-mode-map (kbd "M-?") nil)
;;   (define-key tagedit-mode-map (kbd "M-s") nil)
;;   (add-hook 'sgml-mode-hook (lambda () (tagedit-mode 1))))

(add-auto-mode 'html-mode "\\.\\(jsp\\|tmpl\\)\\'")

(require-package 'web-mode)

(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.css?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.js\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.cjs\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.j2\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.twig\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.vue\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.jsx\\'" . web-mode))

(defvar drmgc/web-tab-width 2)

(require 'swagger-jsdoc-edit)

(add-hook 'web-mode-hook
          (lambda ()
            (set (make-local-variable 'company-backends)
                 '(company-css
                   company-web-html
                   company-files
                   company-yasnippet))
            (setq tab-width drmgc/web-tab-width
                  indent-tabs-mode t
                  web-mode-markup-indent-offset drmgc/web-tab-width
                  web-mode-code-indent-offset drmgc/web-tab-width
                  web-mode-css-indent-offset drmgc/web-tab-width)
            (web-mode-use-tabs)
            (setq web-mode-enable-current-column-hightlight t
                  web-mode-enable-current-element-hightlight t)
            (setq web-mode-enable-auto-pairing t)
            (add-to-list 'web-mode-content-types-alist '("javascript" . "\\.cjs\\'"))

            (setq web-mode-engine-alist
                  '(("django" . "\\.j2\\'")
                    ("django" . "\\.twig\\'")))))

(require-package 'emmet-mode)
(add-hook 'web-mode-hook 'emmet-mode)
(add-hook 'emmet-mode-hook
          (lambda ()
            (setq emmet-indentation drmgc/web-tab-width)
            (setq tab-width drmgc/web-tab-width)))

;; (add-hook 'html-mode ())
;; (when (maybe-require-package 'multi-web-mode)
;;   (setq mweb-default-major-mode 'html-mode)
;;   (setq mweb-tags
;;	'((js-mode "<script[^>]*>" "</script>")
;;	  (css-mode "<style[^>]*>" "</style>")))
;;   (setq 'mweb-filename-extensions '("html" "htm"))
;;   )

(provide 'init-html)
;;; init-html.el ends here
