;;; init-bib.el --- Customize bibliographic packages.  -*- lexical-binding: t; -*-

;;; Commentary:
;; Customize bibliographic import and paths.
;; Provide import functionality for working with PDFs/EPUBs on Dropbox.
;; To complete a citation, type `C-M-i` for completion-at-point.
;; To get a context menu on one, use embark with `C-.`.

;;; Code:

;; Derive paths.
(setq org-cite-global-bibliography (list my-bib-path))

(use-package biblio
  :bind (:map biblio-selection-mode-map
	      ("e" . ebib-biblio-selection-import))
  :custom
  (biblio-bibtex-use-autokey t))

;; For field extraction.
(use-package parsebib)

;; TODO: Fix automatic entries with name conflicts (happens with year-only keys).
;; TODO: Always use the title for the autokeys.
;;       See http://www.jonathanleroux.org/bibtex-mode.html.
(use-package ebib
  :bind (("C-c e" . ebib)
	 (:map ebib-index-mode-map
	       ("B" . ebib-biblio-import-doi)))
  :custom
  (ebib-autogenerate-keys nil)
  (ebib-bib-search-dirs (list my-research-dir))
  (ebib-preload-bib-files (list my-bib-file))
  (ebib-file-search-dirs (list my-bib-library-dir))
  ;; Open files within Emacs rather than calling xpdf or gv.
  (ebib-file-associations nil)
  (ebib-notes-directory my-bib-notes-dir))

;;;; Ebib commands

(defun my--ebib-import-file (file)
  "Import FILE into the current Ebib entry, keeping the original.
`ebib-import-file' always prompts for its file, so answer the prompt
with FILE."
  (cl-letf (((symbol-function 'read-file-name) (lambda (&rest _) file)))
    (ebib-import-file t)))

(defun my--files-matching-key-recursive (root key)
  "Return the PDF and EPUB files under ROOT named after the BibTeX KEY."
  (directory-files-recursively
   root (concat "\\`" (regexp-quote key) "\\.\\(pdf\\|epub\\)\\'") nil t t))

;; TODO: Restrict to ebib-mode.
(defun my-ebib-import-file-from-dropbox-hierarchy ()
  "Import the unique Dropbox reading file named after the current entry's key.
The file is copied into the first of `ebib-file-search-dirs'."
  (interactive)
  (let* ((key (ebib--get-key-at-point))
	 (files (my--files-matching-key-recursive
		 my-dropbox-reading-directory key)))
    (pcase files
      ('() (user-error "[my] Import failed: no files matching %s" key))
      (`(,file) (my--ebib-import-file file))
      (_ (user-error "[my] Import failed: multiple files matching %s: %s"
		     key files)))))

;; TODO: Restrict to ebib-mode.
;; TODO: Handle empty .bib file.
(defun my-ebib-iterate-entries ()
  "Iterate through the ebib entries."
  (interactive)
  (ebib-goto-first-entry)
  ;; `ebib-next-entry' stays put on the last entry, so stop once the key
  ;; no longer changes.
  (let ((key (ebib--get-key-at-point))
	(last-key nil))
    (while (not (equal key last-key))
      (ebib-next-entry)
      (setq last-key key
	    key (ebib--get-key-at-point)))))

;;;; Bibliography consistency checks

(defun my--bib-contents ()
  "Parse the global bib file into a hash table from key to its file field."
  (parsebib-parse my-bib-path :fields '("file")))

(defun my--bib-entry-files (entry)
  "Return the list of files in the file field of a parsed bib ENTRY."
  ;; This is just `split-string-default-separators' with a semicolon.
  (split-string (or (cdr (assoc-string "file" entry)) "")
		"[ \f\t\n\r\v;]+" t))

(defun my--all-file-kvs ()
  "Return a list of (KEY FILE) pairs for every file in the bib file."
  (let ((pairs nil))
    (maphash (lambda (key entry)
	       (dolist (file (my--bib-entry-files entry))
		 (push (list key file) pairs)))
	     (my--bib-contents))
    (nreverse pairs)))

(defun my-bib-missing-files ()
  "Return the (KEY FILE) pairs whose FILE is missing from the library."
  (seq-remove (lambda (kv)
		(file-exists-p (file-name-concat my-bib-library-dir (cadr kv))))
	      (my--all-file-kvs)))

(defun my-reading-files-missing-entries ()
  "Return the files in the library not attached to a bib entry."
  (seq-difference (directory-files my-bib-library-dir nil "\\`[^.]")
		  (mapcar #'cadr (my--all-file-kvs))))

(defun my-bib-unregistered-notes ()
  "Return the note files whose names do not match a bib entry's key."
  (let ((bib (my--bib-contents)))
    (seq-remove (lambda (file)
		  (gethash (file-name-sans-extension file) bib))
		(directory-files my-bib-notes-dir nil "\\.org\\'"))))

;; TODO: Check all attached file names against patterns, including key.
;; TODO: Check all attached files in the expected directory.
;; TODO: Check primary attached file is textual.
;; TODO: Check file name legality according to allowed strings in bib keys.
;; TODO: Check strings in bib keys.

(defvar my--bib-file-text-extensions '("pdf" "epub" "mobi")
  "List of possible extensions for text files associated with bib entries.")

(defvar my--bib-file-supplemental-extensions '("zip")
  "List of possible extensions for supplemental files associated with bib entries.")

(defvar my--bib-file-movie-extensions '("mp4")
  "List of possible extensions for movies associated with bib entries.")

(defvar my--bib-file-pattern-alist '(("supplemental" . "_supplemental$")
				     ("movie" . "-movie.*$")
				     ("corrigendum" . "-corrigendum$")
				     ("text" . ".+"))
  "Ordered alist of (TYPE . PATTERN) for files attached to bib entries.
TYPE is the attached file type and PATTERN is its allowed file name pattern.")

;; We might want to group this with the alist above.
(defvar my--bib-file-extension-alist '(("supplemental" . my--bib-file-supplemental-extensions)
				       ("movie" . my--bib-file-movie-extensions)
				       ("corrigendum" . my--bib-file-text-extensions)
				       ("text" . my--bib-file-text-extensions))
  "Alist of (TYPE . LIST) for files attached to bib entries.
TYPE is the attached file type and LIST is its list of allowed extensions.")

;;;; Citar and Org-roam

(use-package citar
  :custom
  (org-cite-insert-processor 'citar)
  (org-cite-follow-processor 'citar)
  (org-cite-activate-processor 'citar)
  (citar-bibliography org-cite-global-bibliography)
  (citar-library-paths (list my-bib-library-dir))
  (citar-notes-paths (list my-bib-notes-dir))
  :bind
  ;; This allows me to select from all references, and it brings up non-roam preexisting notes, but it's a capture.
  ;; An alternative is citar-open-note, but this doesn't show me all references.
  (("C-c n n" . citar-open-notes)
   ;; Override `org-cite-insert` key binding.
   ;; citar is better at differentiating between sources with the same authors and titles.
   ;; Multi-volume works are an example.
   (:map org-mode-map :package org ("C-c b" . org-cite-insert)))
  :hook
  (LaTeX-mode . citar-capf-setup)
  (org-mode . citar-capf-setup))

(use-package citar-embark
  :after citar embark
  :no-require
  :config (citar-embark-mode))

(use-package org-roam-bibtex
  :after org-roam
  :custom
  (orb-roam-ref-format 'org-cite)
  :config
  (org-roam-bibtex-mode 1))

;; citar-org-roam requires org-roam itself, so load it with citar alone
;; rather than waiting on org-roam, which may never load first.
;; https://github.com/emacs-citar/citar-org-roam/issues/26#issuecomment-1474938504
(use-package citar-org-roam
  :after citar
  :custom
  (citar-org-roam-capture-template-key "n")
  :config
  ;; Can access this in Org mode, but citar-org-roam adds an extra citation key with unexpected formatting: @key.
  ;; Should I consider :if-new, :info, :node, :props?
  ;; Should I use citekey instead of citar-citekey?
  ;; https://kristofferbalintona.me/posts/202206141852/
  (add-to-list 'org-roam-capture-templates
	       '("n" "org-noter" plain
		 "%?"
		 :target
		 (file+head
		  "ref/${citar-citekey}.org"
		  ":PROPERTIES:
:ROAM_REFS: [cite:@${citar-citekey}]
:END:
#+TITLE: ${citar-title}

* Notes                                                               :noter:
")
		 :unnarrowed t))
  (citar-org-roam-mode))

(provide 'init-bib)

;;; init-bib.el ends here
