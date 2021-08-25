;;; init.el --- Load the full configuration -*- lexical-binding: t -*-


;;(setq debug-on-error t)

;; Make frame transparency overridable
(defvar drmgc/frame-transparency '(90 . 90))

(let ((minver "24.5"))
  (when (version< emacs-version minver)
    (error "Your Emacs is too old -- this config requires v%s or higher" minver)))
(when (version< emacs-version "25.1")
  (message "Your Emacs is old, and some functionality in this config will be disabled. Please upgrade if possible."))

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(require 'init-benchmarking) ;; Measure startup time

(defconst *spell-check-support-enabled* nil) ;; Enable with t if you prefer
(defconst *is-a-mac* (eq system-type 'darwin))

;;----------------------------------------------------------------------------
;; Adjust garbage collection thresholds during startup, and thereafter
;;----------------------------------------------------------------------------
(let ((normal-gc-cons-threshold (* 20 1024 1024))
      (init-gc-cons-threshold (* 128 1024 1024)))
  (setq gc-cons-threshold init-gc-cons-threshold)
  (add-hook 'emacs-startup-hook
            (lambda () (setq gc-cons-threshold normal-gc-cons-threshold))))

;;----------------------------------------------------------------------------
;; Bootstrap config
;;----------------------------------------------------------------------------
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(require 'init-utils)
(require 'init-site-lisp) ;; Must come before elpa, as it may provide package.el
;; Calls (package-initialize)
(require 'init-elpa)      ;; Machinery for installing required packages
(require 'init-exec-path) ;; Set up $PATH

;;----------------------------------------------------------------------------
;; use-package
;;----------------------------------------------------------------------------

(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

;;----------------------------------------------------------------------------
;; Allow users to provide an optional "init-preload-local.el"
;;----------------------------------------------------------------------------
(require 'init-preload-local nil t)

;;----------------------------------------------------------------------------
;; Load configs for specific features and modes
;;----------------------------------------------------------------------------

(require-package 'diminish)
(maybe-require-package 'scratch)
(require-package 'command-log-mode)


;;----------------------------------------------------------------------------
;; Indentation
;;----------------------------------------------------------------------------

(defvar drmgc/tab-width 4)
(setq-default c-basic-offset drmgc/tab-width)
(defun drmgc/disable-tabs ()
  "Disable TABS."
  (setq indent-tabs-mode nil))

(defun drmgc/enable-tabs ()
  "Enable TABs."
  (setq indent-tabs-mode t))

(defun drmgc/set-tab-width ()
  "Enable TABs."
  (setq tab-width drmgc/tab-width))

(add-hook 'prog-mode-hook 'drmgc/set-tab-width)
(add-hook 'c-mode-common-hook 'drmgc/enable-tabs)

(add-hook 'lisp-mode-hook 'drmgc/disable-tabs)
(add-hook 'emacs-lisp-mode-hook 'drmgc/disable-tabs)

(setq-default electric-indent-inhabit t)
(setq backward-delete-char-untabify-method 'hungry)


;;----------------------------------------------------------------------------
;; whitespace-mode
;;----------------------------------------------------------------------------

(setq whitespace-style '(face tabs tab-mark trailing))
(custom-set-faces
 '(whitespace-tab ((t (:foreground "#353540"))))) ; TODO: Убрать в init-themes?
(setq whitespace-display-mappings
  '((tab-mark 9 [124 9] [92 9]))) ; 124 is the ascii ID for '\|'
(global-whitespace-mode)


;;----------------------------------------------------------------------------
;; Keys-rebindings
;;----------------------------------------------------------------------------

(define-key key-translation-map (kbd "C-h") (kbd "DEL"))
(define-key key-translation-map (kbd "C-M-h") (kbd "M-DEL"))
(define-key key-translation-map (kbd "C-?") (kbd "C-h"))

(require 'no-easy-keys)
(no-easy-keys 1)


(require 'init-frame-hooks) ; хуки для фреймов
(require 'init-xterm) ; интеграция с терминалом
(require 'init-themes) ; темки
; (require 'init-osx-keys)
(require 'init-gui-frames) ; чо касаема фреймов
(require 'init-dired)
(require 'init-isearch)
(require 'init-grep)
(require 'init-uniquify) ; uniquify именовать буферков
(require 'init-ibuffer)
(require 'init-flycheck)
(require 'init-avy)

(require 'init-recentf) ; недавние файлы
(require 'init-smex)
(require 'init-ivy) ; minibuffer-completition
(require 'init-hippie-expand)
(require 'init-company)
(require 'init-windows) ; окошечки
(require 'init-sessions) ; сессии
(require 'init-mmm)

(use-package editorconfig
  :ensure t
  :config
  (editorconfig-mode 1))

(require 'init-lsp)


;;----------------------------------------------------------------------------
;; shell-mode
;;----------------------------------------------------------------------------
(add-hook 'shell-mode-hook
          (lambda ()
            (face-remap-set-base 'comint-highlight-prompt :inherit nil)))

(require 'init-editing-utils)
(require 'init-whitespace)

(require 'init-vc)
(require 'init-git)
(require 'init-github)

(require 'init-projectile)

(use-package yasnippet
  :hook (prog-mode . yas-minor-mode)
  :config
  (yas-reload-all)
  (yas-global-mode))

(require 'init-compile)
(require 'init-cmake)
; (require 'init-crontab)
; (require 'init-textile)
; (require 'init-markdown)
; (require 'init-csv)
; (require 'init-erlang)
(require 'init-javascript)
(require 'init-lua)
(require 'init-php)
(require 'init-org)
(require 'init-latex)
(require 'init-go)
; (require 'init-nxml)
(require 'init-html)
(require 'init-css)
; (require 'init-haml)
; (require 'init-http)
(require 'init-python)
; (require 'init-haskell)
; (require 'init-elm)
; (require 'init-purescript)
; (require 'init-ruby)
; (require 'init-rails)
(require 'init-sql)
; (require 'init-nim)
; (require 'init-rust)
; (require 'init-toml)
(require 'init-yaml)
; (require 'init-docker)
; (require 'init-terraform)
; (require 'init-nix)
(maybe-require-package 'nginx-mode)

(require 'init-csharp)

; (require 'init-paredit)
; (require 'init-lisp)
; (require 'init-slime)
; (require 'init-clojure)
; (require 'init-clojure-cider)
; (require 'init-common-lisp)

(use-package cpputils-cmake
  :config
  (add-hook 'c-mode-common-hook
            (lambda ()
              (if (derived-mode-p 'c-mode 'c++-mode)
                  (cppcm-reload-all)
                ))))

(use-package glsl-mode
  :mode (("\\.glsl\\'" . glsl-mode)
         ("\\.vert\\'" . glsl-mode)
         ("\\.frag\\'" . glsl-mode)
         ("\\.geom\\'" . glsl-mode)
         ("\\.vs\\'" . glsl-mode)
         ("\\.fs\\'" . glsl-mode)
         ("\\.gs\\'" . glsl-mode)))

(require 'init-gdscript)

(use-package smart-tabs-mode
  :config
  (add-hook 'c-mode-common-hook 'smart-tabs-mode-enable)
  (add-hook 'csharp-mode-hook 'smart-tabs-mode-enable)
  (smart-tabs-advice c-indent-line c-basic-offset)
  (smart-tabs-advice c-indent-region c-basic-offset))

(when *spell-check-support-enabled*
  (require 'init-spelling))

(require 'init-misc)

(require 'init-folding) ; сворачивание
; (require 'init-dash)

;;(require 'init-twitter)
;; (require 'init-mu)
(require 'init-ledger)

;; Extra packages which don't require any configuration

(require-package 'sudo-edit)
; (require-package 'gnuplot)
; (require-package 'htmlize)
(when *is-a-mac*
  (require-package 'osx-location))
(unless (eq system-type 'windows-nt)
  (maybe-require-package 'daemons))
; (maybe-require-package 'dotenv-mode)
(maybe-require-package 'shfmt)

(when (maybe-require-package 'uptimes)
  (setq-default uptimes-keep-count 200)
  (add-hook 'after-init-hook (lambda () (require 'uptimes))))

(when (fboundp 'global-eldoc-mode)
  (add-hook 'after-init-hook 'global-eldoc-mode))

(require 'init-direnv)

;;----------------------------------------------------------------------------
;; Allow access from emacsclient
;;----------------------------------------------------------------------------
(add-hook 'after-init-hook
          (lambda ()
            (require 'server)
            (unless (server-running-p)
              (server-start))))

;;----------------------------------------------------------------------------
;; Variables configured via the interactive 'customize' interface
;;----------------------------------------------------------------------------
(when (file-exists-p custom-file)
  (load custom-file))


;;----------------------------------------------------------------------------
;; Locales (setting them earlier in this file doesn't work in X)
;;----------------------------------------------------------------------------
(require 'init-locales)


;;----------------------------------------------------------------------------
;; Allow users to provide an optional "init-local" containing personal settings
;;----------------------------------------------------------------------------
(require 'init-local nil t)



(provide 'init)

;; Local Variables:
;; coding: utf-8
;; no-byte-compile: t
;; End:
;;; init.el ends here
