;;; init.el --- Ben Landrum's Emacs .init file

;; Global variables
(setq
 my-custom-file "custom.el"
 my-init-file "~/.emacs.d/init.el"
 my-epdfinfo-program "/Users/blandrum/.local/bin/epdfinfo"
 my-lsp-clients-clangd-executable "/ssh:cloudvm:/usr/bin/clangd"
 my-lsp-debug nil
 my-org-directory "~/org"
 my-org-format-latex-text-ratio 1.8
 my-org-roam-directory "~/org"
 my-minimum-emacs-version "27.1"
 my-tramp-default-method "scp"
 my-tramp-verbose 6
 my-vagrant-vm-address "cloudvm"
 my-vagrant-vm-path "/Users/blandrum/beeswax/vms/cloudvm"
 debug-on-error nil
 )

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
  (require 'init-platform)
  (require 'init-appearance)
  (require 'init-dired)
  (require 'init-helm)
  (require 'init-tramp)
  (require 'init-treemacs)
  (require 'init-lsp)
  (require 'init-vagrant)
  (require 'init-pdf)
  (require 'init-magit)
  (require 'init-org)
  (require 'init-filetypes))

;; Remove this!
(setq gc-cons-threshold (* 100 1024 1024)
      read-process-output-max (* 1024 1024)
      )

(provide 'init)

;;; init.el ends here
