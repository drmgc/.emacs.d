;;; init-latex.el --- LaTeX editing -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(add-hook 'latex-mode-hook
          (lambda ()
            (drmgc/disable-tabs)
            (display-line-numbers-mode)
            (setq tex-indent-basic 2)))

(provide 'init-latex)
;;; init-latex.el ends here
