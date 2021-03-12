(with-eval-after-load 'org
  (setq org-startup-indented t
	org-agenda-files '(my-org-agenda-files))
  (add-hook 'org-mode-hook #'visual-line-mode))

(provide 'init-org)
