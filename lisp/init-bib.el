;;; init-bib.el --- Customize bibliographic packages.

;;; Commentary:
;; Customize bibliographic import and paths.
;; Provide import functionality for working with PDFs/EPUBs on Dropbox.

;;; Code:

(use-package biblio
  :config
  (setq biblio-bibtex-use-autokey t))

;; For list filtering, etc.
(use-package dash)

;; For field extraction.
(use-package parsebib)

;; TODO: Fix automatic entries with name conflicts (happens with year-only keys).
;; TODO: Always use the title for the autokeys.
;;       See http://www.jonathanleroux.org/bibtex-mode.html.
(use-package ebib
  :bind (("C-c e" . ebib)
	 (:map ebib-index-mode-map
	       ("B" . ebib-biblio-import-doi))
	 (:map biblio-selection-mode-map
	       ("e" . ebib-biblio-selection-import)))
  :config
  (setq ebib-autogenerate-keys nil)
  (setq ebib-bib-search-dirs my-ebib-bib-search-dirs)
  (setq ebib-preload-bib-files my-ebib-preload-bib-files)
  (setq ebib-file-search-dirs my-ebib-file-search-dirs)
  ;; Open files within Emacs rather than calling xpdf or gv.
  (setq ebib-file-associations nil))

;; TODO: Request a way to supply a file name argument without a command prompt.
;;       For now just copying the function from ebib.el.
;;       Original code Copyright (c) 2003-2022, Joost Kremers.
;;       All rights reserved.
(defun my--ebib-import-file (arg fname)
  "A hack to get around ebib-import-file having to read from a prompt."
  (let* ((key (ebib--get-key-at-point))
         (file-path (expand-file-name fname))
         (ext (file-name-extension file-path))
         (new-name (ebib--create-file-name-from-key key ext))
         (dest-dir (file-name-as-directory (car ebib-file-search-dirs)))
         (dest-path (concat dest-dir new-name))
         (overwrite nil))
    (if (not (file-writable-p dest-path))
        (error "[Ebib] Cannot write file %s" dest-path))
    (while (and (file-exists-p dest-path)
                (not overwrite))
      (let ((choice (read-char-choice (format "File %s already exists; (o)verwrite / (r)ename / (c)ancel? " new-name) '(?o ?r ?c ?q))))
        (cl-case choice
          ((?c ?q) (error "[Ebib] Cancelled importing file"))
          (?r (setq new-name (read-string (format "Change `%s' to: " new-name) new-name))
              (setq dest-path (concat (file-name-as-directory (car ebib-file-search-dirs)) new-name)))
          (?o (setq overwrite t)))))
    (copy-file file-path dest-path t)
    (unless arg
      (delete-file file-path t))
    (let ((files (ebib-get-field-value "file" key ebib--cur-db 'noerror 'unbraced)))
      (when (or (null files)
                (not (string-match-p (regexp-quote new-name) files)))
        (ebib-set-field-value "file" (ebib--transform-file-name-for-storing (expand-file-name dest-path)) key ebib--cur-db ebib-filename-separator)
        (ebib--set-modified t ebib--cur-db t (seq-filter (lambda (dependent)
                                                           (ebib-db-has-key key dependent))
                                                         (ebib--list-dependents ebib--cur-db)))
        (ebib--update-entry-buffer)))))

(defun my--files-matching-key-recursive (root key)
  "Get all PDF and EPUB files with names matching a BibTeX KEY within a ROOT directory."
  (append (directory-files-recursively root (concat (regexp-quote key) "\\.\\(pdf\\|epub\\)") nil t t)))

;; TODO: Restrict to ebib-mode.
(defun my-ebib-import-file-from-dropbox-hierarchy ()
  "Called from ebib, searches Dropbox for a unique file matching this entry's key and imports it into (car my-ebib-file-search-dirs)."
  (interactive)
  (ebib-copy-key-as-kill)
  (let* ((key (pop kill-ring))
	 (matching-files (my--files-matching-key-recursive
			  my-dropbox-reading-directory key)))
    (cl-case (length matching-files)
      (0 (error "[my] Import failed: no files matching %s" key))
      (2 (error "[my] Import failed: multiple files matching %s: %s" key matching-files))
      (1 (my--ebib-import-file t (car matching-files))))))

(defun my--bib-contents ()
  "Parse the global bib file into a hash table mapping bib key to properties...here just the file."
  (parsebib-parse my-bib-file :fields '("file")))

(defun my--bib-entry-file-string-to-list (string)
  "Split a semicolon-and-space-delimited string into a list of strings."
  ;; This is just split-string-default-separators with a semicolon.
  (split-string string "[ \f\t\n\r\v;]+" t))

(defun my--bib-entry-file-exists (file)
  "Search in the default flat bibliography file paths for the given file, returning t for found and nil for not."
  (file-exists-p (concat (file-name-as-directory my-bib-file-dir) file)))

(defun my--bib-entry-file-string (key table)
  "Return the (first) file string for a given bib entry, otherwise empty."
  (let ((values (-filter (lambda (kv) (string= (car kv) "file"))
			 (gethash key table))))
    (cl-case (length values)
      (0 "")
      (1 (cdr (car values)))
      (t (error "[my] Multiple file strings found")))))

;; TODO: Consider returning cons cells.
(defun my--bib-entry-to-pair-list (key table)
  "Assume that the car of the value is file. Return flattened key-file pairs."
  (let* ((files-string (my--bib-entry-file-string key table))
	 (files (my--bib-entry-file-string-to-list files-string)))
    (mapcar (lambda (file) (list key file)) files)))

(defun my--all-file-kvs ()
  "Return all key-file pairs."
  (let ((bib (my--bib-contents)))
    (apply #'append (mapcar (lambda (key) (my--bib-entry-to-pair-list key bib))
			    (hash-table-keys bib)))))

(defun my--all-files ()
  "Return all files in the bib file."
  (mapcar (lambda (kv) (car (cdr kv))) (my--all-file-kvs)))

(defun my-bib-missing-files ()
  "Return a list of pairs of key-file pairs to indicate files in .bib file that cannot be found."
  (-filter (lambda (f)
	     (not (my--bib-entry-file-exists (car (cdr f)))))
	   (my--all-file-kvs)))

(defun my-reading-files-missing-entries ()
  "Return a list of files in the reading location not connected to a bib entry."
  (let ((files (directory-files my-bib-file-dir nil "^[^.]")))
    (-difference files (my--all-files))))

;; TODO: Restrict to ebib-mode.
;; TODO: Handle empty .bib file.
;; TODO: Eliminate beep on last entry.
;; TODO: Eliminate kill ring pollution.
(defun my-ebib-iterate-entries ()
  "Iterate through the ebib entries."
  (interactive)
  (ebib-goto-first-entry)
  ;; Iterate by comparing keys after advancing.
  ;; Stop when keys are equal.
  (let ((last-entry nil)
	(entry))
    (ebib-copy-key-as-kill)
    (setq entry (car kill-ring))
    (while (not (eq last-entry entry))
      (ebib-next-entry)
      (setq last-entry entry)
      (ebib-copy-key-as-kill)
      (setq entry (car kill-ring)))))

(use-package helm-bibtex
  :config
  (setq bibtex-completion-bibliography (list my-bibtex-completion-bibliography))
  (setq bibtex-completion-library-path (list my-bibtex-completion-library-path))
  (setq bibtex-completion-notes-path my-bibtex-completion-notes-path)
  ;; Prefer the file field in the BibLaTeX file to the file in the directory with the same name.
  (setq bibtex-completion-pdf-field "file")
  :bind (("C-c n B" . helm-bibtex)))

;; Watch directories in case we add new files.
(setq my-helm-bibtex-library-watch
      (file-notify-add-watch my-bibtex-completion-library-path
                             '(change)
                             (lambda (event) (bibtex-completion-candidates))))
(setq my-helm-bibtex-notes-watch
      (file-notify-add-watch my-bibtex-completion-notes-path
                             '(change)
                             (lambda (event) (bibtex-completion-candidates))))

(provide 'init-bib)
