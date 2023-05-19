;;; init-github.el --- Github integration -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'init-git)

(maybe-require-package 'yagist)
(require-package 'bug-reference-github)
(add-hook 'prog-mode-hook 'bug-reference-prog-mode)

(maybe-require-package 'github-clone)
(maybe-require-package 'forge)
(maybe-require-package 'github-review)

(with-eval-after-load 'forge
  (add-to-list 'forge-alist
               '("fbet-gitlab.ex2b.co"
                 "fbet-gitlab.ex2b.co/api/v4"
                 "fbgl"
                 forge-gitlab-repository)))

(provide 'init-github)
;;; init-github.el ends here
