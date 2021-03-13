(use-package org
  :config
  (setq org-startup-indented t
	org-agenda-files '(my-org-agenda-files))
  :hook (visual-line-mode))

(provide 'init-org)
