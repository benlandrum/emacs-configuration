;;; init-org.el --- Customize org and related modes.

;;; Commentary:
;; The biggest customizations are indented startup and org-noter bindings.
;; In the future, I'd like to add a natural way to link between noter documents.

;;; Code:

(use-package org
  :config
  (setq org-cycle-emulate-tab nil
	org-startup-indented t
	org-id-link-to-org-use-id 'create-if-interactive-and-no-custom-id)
  (if my-org-directory
      (setq org-agenda-files (directory-files-recursively
			      my-org-directory "\.org$" nil t t)))
  (plist-put org-format-latex-options
	     :scale my-org-format-latex-text-ratio)

  ;; This requres dvipng.
  ;; I installed this with tlmgr.

  ;; Sync LaTeX fragment scaling with text scaling.
  ;; https://emacs.stackexchange.com/questions/3387/how-to-enlarge-latex-fragments-in-org-mode-at-the-same-time-as-the-buffer-text
  (defun update-org-latex-fragments ()
    (org-latex-preview '(64))
    (plist-put org-format-latex-options
	       :scale (* my-org-format-latex-text-ratio
			 text-scale-mode-amount))
    (org-latex-preview '(16)))
  (add-hook 'text-scale-mode-hook 'update-org-latex-fragments)
  (add-hook 'org-mode-hook #'visual-line-mode)

  ;; https://writequit.org/articles/emacs-org-mode-generate-ids.html
  (defun my-org-custom-id-get (&optional pom create prefix)
    "Get the CUSTOM_ID property of the entry at point-or-marker POM.
     If POM is nil, refer to the entry at point. If the entry does
     not have an CUSTOM_ID, the function returns nil. However, when
     CREATE is non nil, create a CUSTOM_ID if none is present
     already. PREFIX will be passed through to `org-id-new'. In any
     case, the CUSTOM_ID of the entry is returned."
    (interactive)
    (org-with-point-at pom
      (let ((id (org-entry-get nil "CUSTOM_ID")))
	(cond
	 ((and id (stringp id) (string-match "\\S-" id))
          id)
	 (create
          (setq id (org-id-new (concat prefix "h")))
          (org-entry-put pom "CUSTOM_ID" id)
          (org-id-add-location id (buffer-file-name (buffer-base-buffer)))
          id)))))

  (defun my-org-add-ids-to-headlines-in-file ()
    "Add CUSTOM_ID properties to all headlines in the
     current file which do not already have one."
    (interactive)
    (org-map-entries (lambda () (my-org-custom-id-get (point) 'create))))

  ;; Automatically add ids to captured headlines.
  (add-hook 'org-capture-prepare-finalize-hook
            (lambda () (my-org-custom-id-get (point) 'create))))

;; Not technically org, but I always use this through org.
;; "C-c C-e h o" renders to my browser with syntax highlighting with this.
(use-package htmlize)

(use-package ox-gfm)

;; For reading *.epub files in org-noter.
(use-package nov)
(add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode))

;; This does not seem to play well with visual line mode.
;; Consider disabling it here.
(use-package org-noter
  :config
  (setq org-noter-auto-save-last-location t
	org-noter-doc-split-fraction '(0.6 . 0.5))
  :bind ("M-i" . org-noter-insert-precise-note))

(setq org-roam-v2-ack t)
(use-package org-roam
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
