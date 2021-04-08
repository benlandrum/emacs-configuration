;;; init-org.el --- Customize org and related modes.

;;; Commentary:
;; The biggest customizations are indented startup and org-noter bindings.

;;; Code:

(use-package org
  :config
  (setq org-startup-indented t
	org-agenda-files (directory-files-recursively
			  my-org-directory "\.org$" nil t t))

  ;; Sync LaTeX fragment scaling with text scaling.
  ;; https://emacs.stackexchange.com/questions/3387/how-to-enlarge-latex-fragments-in-org-mode-at-the-same-time-as-the-buffer-text
  (defun update-org-latex-fragments ()
    (org-latex-preview '(64))
    (plist-put org-format-latex-options :scale text-scale-mode-amount)
    (org-latex-preview '(16)))
  (add-hook 'text-scale-mode-hook 'update-org-latex-fragments)
  (add-hook 'org-mode-hook #'visual-line-mode))

(use-package org-noter
  :bind ("M-i" . org-noter-insert-precise-note))

(use-package org-roam
  :hook
  (after-init . org-roam-mode)
  :custom
  (org-roam-directory my-org-roam-directory)
  :bind (:map org-roam-mode-map
	      (("C-c n l" . org-roam)
	       ("C-c n f" . org-roam-find-file)
	       ("C-c n g" . org-roam-graph))
	      :map org-mode-map
	      (("C-c n i" . org-roam-insert))
	      (("C-c n I" . org-roam-insert-immediate))))

(provide 'init-org)

;;; init-org.el ends here
