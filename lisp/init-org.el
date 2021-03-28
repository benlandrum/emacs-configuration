;;; init-org.el --- Customize org and related modes.

;;; Commentary:
;; Only a few settings currently.

;;; Code:

(use-package org
  :config
  (setq org-startup-indented t
	org-agenda-files '(my-org-agenda-files))
  :hook (visual-line-mode))

(use-package org-noter)

(provide 'init-org)

;;; init-org.el ends here
