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
(add-to-list 'auto-mode-alist '("\\.j2\\'" . web-mode))

(add-hook 'web-mode-hook
	  (lambda ()
	    (set (make-local-variable 'company-backends)
		 '(company-css
		   company-web-html
		   company-files
		   company-yasnippet))
	    (setq tab-width 4
		  indent-tabs-mode t
		  web-mode-markup-indent-offset 4
		  web-mode-code-indent-offset 4
		  web-mode-css-indent-offset 4)
            (web-mode-use-tabs)
	    (setq web-mode-enable-current-column-hightlight t
		  web-mode-enable-current-element-hightlight t)
            (setq web-mode-enable-auto-pairing nil)
            (setq web-mode-engine-alist
                  '(("django" . "\\.j2\\'")))))

(require-package 'emmet-mode)
(add-hook 'web-mode-hook 'emmet-mode)
(add-hook 'emmet-mode-hook
          (lambda ()
            (setq emmet-indentation 4)))

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
