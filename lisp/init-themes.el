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
(setq-default custom-enabled-themes '(atom-one-dark))

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
  (setq custom-enabled-themes '(atom-one-dark))
  (custom-set-faces
   '(whitespace-tab ((t (:foreground "#353540")))))
  (reapply-themes))

;; (add-hook 'after-init-hook 'dark)



;;------------------------------------------------------------------------------
;; Setting theme depending of time and month
;;------------------------------------------------------------------------------

(defvar daylight-hours-by-month (make-list 12 '(9 . 18)))
(setq daylight-hours-by-month '((8 . 19) ; январь
                                (8 . 19) ; февраль
                                (8 . 19) ; март
                                (8 . 19) ; апрель
                                (8 . 19) ; май
                                (8 . 19) ; июнь
                                (8 . 19) ; июль
                                (8 . 18) ; август
                                (8 . 18) ; сентябрь
                                (8 . 19) ; октябрь
                                (8 . 19) ; ноябрь
                                (8 . 19))) ; декабрь


(defun update-color-theme-by-time ()
  "Setting color theme by timem and month."
  (interactive)
  (let* ((now (current-time))
         (decoded-time (decode-time now))
         (hours (nth 2 decoded-time))
         (month (nth 4 decoded-time))
         (daylight-hours (nth (- month 1) daylight-hours-by-month))
         (daylight-start (car daylight-hours))
         (daylight-end (cdr daylight-hours))
         (daylight-now (and (>= hours daylight-start)
                            (< hours daylight-end))))
    (progn (if daylight-now (light) (dark))
           (message "Using %s theme because of daylight time from %d:00 to %d:00 in %s"
                    (if daylight-now "light" "dark")
                    daylight-start daylight-end
                    (format-time-string "%d %B" now)))))

(add-hook 'emacs-startup-hook 'update-color-theme-by-time)


;; Dimmer

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
