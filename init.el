;;; init.el --- Ben Landrum's Emacs .init file

;; System-specific variables needing customization.
; my-epdfinfo-program "/Users/blandrum/.local/bin/epdfinfo"

;; Global variables
(setq
 debug-on-error nil
 my-asymptote-dir "/usr/local/share/asymptote"
 my-bib-file "bibliography.bib"
 my-custom-file "custom.el"
 my-dropbox-reading-directory "~/Dropbox/reading"
 my-file-local-repos '("git@bitbucket.org:blandrum/research.git")
 my-init-file "~/.emacs.d/init.el"
 my-lsp-clients-clangd-executable "/ssh:privatecloudvm:/usr/bin/clangd"
 my-lsp-debug nil
 my-minimum-emacs-version "27.1"
 my-org-directory nil
 my-org-format-latex-text-ratio 1.8
 my-quotes-path "~/.emacs.d/quotes.txt"
 my-research-dir "~/research"
 my-tramp-default-method "scp"
 my-tramp-verbose 6
 my-workon-home-dir "~/venv"
 )

;; Derived global variables.
(setq my-org-dir (concat (file-name-as-directory my-research-dir) "org"))
(setq my-bib-path (concat (file-name-as-directory my-research-dir) my-bib-file))
(setq my-bib-library-dir (concat (file-name-as-directory my-research-dir) "ref"))
(setq my-bib-notes-dir (concat (file-name-as-directory my-org-dir) "ref"))
(setq my-org-roam-directory (file-name-as-directory my-org-dir))

;;; Commentary:
;; This is a modular setup, where different modes have their own
;; elisp files.  It enables me to program C++ and Python in a VM, as well as
;; reading *.pdf files and authoring LaTeX.

;;; Code:
(when (version< emacs-version my-minimum-emacs-version)
  (error "Emacs version %s is older than required version %s"
	 emacs-version my-minimum-emacs-version))

(add-to-list 'load-path "~/.emacs.d/lisp/")
(let ((file-name-handler-alist nil)  ; anti-regex speed loading hack
      (gc-cons-threshold 100000000)  ; delay garbage collection
      (debug-on-error t)
      (debug-on-quit t))
  (require 'init-global)
  (require 'init-embark)
  (require 'init-platform)
  (require 'init-appearance)
  (require 'init-dired)
  (require 'init-completion)
  (require 'init-tramp)
  (require 'init-treemacs)
  (require 'init-lsp)
  (require 'init-pdf)
  (require 'init-magit)
  (require 'init-org)
  (require 'init-python)
  (require 'init-filetypes)
  (require 'init-bib)
  (require 'init-dashboard))

;; Remove this!
(setq gc-cons-threshold (* 100 1024 1024)
      read-process-output-max (* 1024 1024)
      )

(provide 'init)

;;; init.el ends here
