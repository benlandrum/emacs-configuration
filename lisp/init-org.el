;;; init-org.el --- Customize org and related modes.

;;; Commentary:
;; The biggest customizations are indented startup and org-noter bindings.
;; In the future, I'd like to add a natural way to link between noter documents.

;;; Code:

;; Currently using the org-mode below for faster LaTeX previews.
;; https://abode.karthinks.com/org-latex-preview/
;; https://git.tecosaur.net/tec/org-mode
;; (package-vc-install '(org-mode :url "https://code.tecosaur.net/tec/org-mode" :branch "dev"))

(use-package org
  :load-path "~/.emacs.d/elpa/org-mode/lisp/"
  :config
  (setq org-cycle-emulate-tab nil
	org-startup-indented t
	org-src-preserve-indentation t
	org-id-link-to-org-use-id 'create-if-interactive-and-no-custom-id)
  (if my-org-directory
      (setq org-agenda-files (directory-files-recursively
			      my-org-directory "\.org$" nil t t)))
  (plist-put org-latex-preview-appearance-options :page-width 0.8)
  (plist-put org-latex-preview-appearance-options :zoom my-org-latex-preview-appearance-zoom)

  ;; This requires dvipng.
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

  ;; Turn on org-latex-preview-mode.
  (add-hook 'org-mode-hook 'org-latex-preview-mode)

  ;; Turn on live previews.
  (setq org-latex-preview-mode-display-live t)

  ;; More immediate live previews.
  ;; Default is one second.
  (setq org-latex-preview-mode-update-delay 0.25)

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

;; Trying this out for now.
(use-package xenops)
(add-hook 'latex-mode-hook #'xenops-mode)

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

(defun my-org-roam-directory-set-relative (path)
  "Used by .dir-locals.el files to set org-roam-directory to a PATH relative to the .dir-locals.el file."
  (setq-local org-roam-directory
	      (concat (file-name-as-directory
		       (locate-dominating-file default-directory ".dir-locals.el"))
		      (file-name-as-directory path))))

(defun my-org-file-to-cite-property ()
  "Interpret the file name as a citation key, and add it as a :ROAM_REFS: citation."
  (interactive)
  (beginning-of-buffer)
  (org-set-property "ROAM_REFS"
		    (concat "cite:"
			    (string-remove-suffix ".org" (buffer-name)))))

(use-package org-roam
  :after org
  :init
  (setq org-roam-v2-ack t)
  (setq org-roam-capture-templates
	'(("d" "default" plain
	   "%?"
	   :target
	   (file+head
	    "zettel/%<%Y%m%d%H%M%S>-${slug}.org"
	    "#+TITLE: ${title}\n")
	   :unnarrowed t)))
  :custom
  (org-roam-directory my-org-roam-directory)
  (org-roam-dailies-directory "daily")
  (org-roam-dailies-capture-templates
   '(("d" "default" entry
         "* %?"
         :target (file+head "%<%Y-%m-%d>.org"
                            "#+title: %<%Y-%m-%d>\n"))))
  :bind (("C-c n f" . org-roam-node-find)
	 ("C-c n r" . org-roam-node-random)
         ("C-c n g" . org-roam-graph)
         ("C-c n c" . org-roam-capture)
         ("C-c n j" . org-roam-dailies-capture-today)
	 (:map org-mode-map
	       (("C-c n i" . org-roam-node-insert)
		("C-c n o" . org-id-get-create)
		("C-c n t" . org-roam-tag-add)
		("C-c n a" . org-roam-alias-add)
		("C-c n l" . org-roam-buffer-toggle))))
  :config
  (org-roam-setup)
  (org-roam-db-autosync-mode))

(use-package ox-reveal)

;; Copied from orgmode.org version 9.7 changes.
(defun org-compat-adjust-tab-width-in-buffer (old-width)
  "Adjust visual indentation from `tab-width' equal OLD-WIDTH to 8."
  (interactive "nOld `tab-width': ")
  (cl-assert (derived-mode-p 'org-mode))
  (unless (= old-width 8)
    (org-with-wide-buffer
     (goto-char (point-min))
     (let (bound
           (repl (if (< old-width 8)
                     (make-string old-width ?\s)
                   (concat "\t" (make-string (- old-width 8) ?\s)))))
       (while (re-search-forward "^ *\t" nil t)
         (skip-chars-forward " \t")
         (setq bound (point-marker))
         (forward-line 0)
         (while (search-forward "\t" bound t)
           (replace-match repl)))))))

(provide 'init-org)

;; Always permit Babel fragment evaluation for files checked into my repos.
;; This must be loaded after Magit.
(defun my-is-safe-file-predicate ()
  "Custom predicate to control when to prompt about Babel evaluation."
  (let ((remote (magit-get "remote" (magit-get-remote) "url")))
    (if (and (member remote my-file-local-repos)
	     (vc-backend (buffer-file-name)))
	t nil)))

(defun my-org-confirm-babel-evaluate-predicate (lang body)
  (not (my-is-safe-file-predicate)))
(setq org-confirm-babel-evaluate #'my-org-confirm-babel-evaluate-predicate)

;; Prevent org-ctags from changing desired org-open-link-functions.
;; It makes internal links function in unexpected ways.
;; https://emacs.stackexchange.com/a/76352
(with-eval-after-load 'org-ctags (setq org-open-link-functions nil))

;;; init-org.el ends here
