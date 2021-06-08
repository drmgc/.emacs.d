;;; init-themes.el --- Defaults for themes -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;;------------------------------------------------------------------------------
;; Fonts
;;------------------------------------------------------------------------------
(set-frame-font "Hack" nil t)


(require-package 'atom-one-dark-theme)
(require-package 'one-themes)
(require-package 'vscode-dark-plus-theme)
(require-package 'plan9-theme)
(require-package 'leuven-theme)
(require-package 'twilight-bright-theme)

;; Don't prompt to confirm theme safety. This avoids problems with
;; first-time startup on Emacs > 26.3.
(setq custom-safe-themes t)

; ;; If you don't customize it, this is the theme you get.
(setq-default custom-enabled-themes '(vscode-dark-plus))

;; Ensure that themes will be applied even if they have not been customized
(defun reapply-themes ()
  "Forcibly load the themes listed in `custom-enabled-themes'."
  (dolist (theme custom-enabled-themes)
    (unless (custom-theme-p theme)
      (load-theme theme)))
  (custom-set-variables `(custom-enabled-themes (quote ,custom-enabled-themes))))

(defun disable-all-themes ()
  "disable all active themes."
  (dolist (i custom-enabled-themes)
    (disable-theme i)))

(defadvice load-theme (before disable-themes-first activate)
  (disable-all-themes))


;;------------------------------------------------------------------------------
;; Toggle between light and dark
;;------------------------------------------------------------------------------
(defun light ()
  "Activate a light color theme."
  (interactive)
  (disable-all-themes)
  (setq custom-enabled-themes '(one-light))
  (custom-set-faces
   '(whitespace-tab ((t (:foreground "#ccc")))))
  (reapply-themes))

(defun dark ()
  "Activate a dark color theme."
  (interactive)
  (disable-all-themes)
  (setq custom-enabled-themes '(vscode-dark-plus))
  (custom-set-faces
   '(whitespace-tab ((t (:foreground "#353540")))))
  (reapply-themes))

(add-hook 'after-init-hook 'dark)


(when (maybe-require-package 'dimmer)
  (setq-default dimmer-fraction 0.15)
  (add-hook 'after-init-hook 'dimmer-mode)
  (with-eval-after-load 'dimmer
    ;; TODO: file upstream as a PR
    (advice-add 'frame-set-background-mode :after (lambda (&rest args) (dimmer-process-all))))
  (with-eval-after-load 'dimmer
    ;; Don't dim in terminal windows. Even with 256 colours it can
    ;; lead to poor contrast.  Better would be to vary dimmer-fraction
    ;; according to frame type.
    (defun sanityinc/display-non-graphic-p ()
      (not (display-graphic-p)))
    (add-to-list 'dimmer-exclusion-predicates 'sanityinc/display-non-graphic-p)))


(provide 'init-themes)
;;; init-themes.el ends here
