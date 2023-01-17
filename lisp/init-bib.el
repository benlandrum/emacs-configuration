;;; init-bib.el --- Customize bibliographic packages.

;;; Commentary:
;; Customize bibliographic import and paths.

;;; Code:

(use-package biblio
  :config
  (setq biblio-bibtex-use-autokey t))

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
  (setq ebib-file-search-dirs my-ebib-file-search-dirs))

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

;; TODO: Enable epub and other formats.
(defun my--files-matching-key-recursive (root key)
  "Get all files with names matching a BibTeX KEY within a ROOT directory."
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

(use-package helm-bibtex
  :config
  (setq bibtex-completion-bibliography (list my-bibtex-completion-bibliography))
  (setq bibtex-completion-library-path (list my-bibtex-completion-library-path))
  (setq bibtex-completion-notes-path my-bibtex-completion-notes-path)
  ;; Prefer the file field in the BibLaTeX file to the file in the directory with the same name.
  (setq bibtex-completion-pdf-field "file")
  :bind (("C-c n B" . helm-bibtex)))

(provide 'init-bib)
