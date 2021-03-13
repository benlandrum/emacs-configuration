(use-package org
  :config
  (setq org-startup-indented t
	org-agenda-files '(my-org-agenda-files))
  :hook (visual-line-mode))

(use-package org-noter)

(provide 'init-org)
