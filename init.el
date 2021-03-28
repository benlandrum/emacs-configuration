;;; init.el --- Ben Landrum's Emacs .init file

;; Global variables
(setq
 my-init-file "~/.emacs.d/init.el"
 my-epdfinfo-program "/Users/blandrum/.local/bin/epdfinfo"
 my-lsp-clients-clangd-executable "/ssh:vagrant@10.211.55.4#22:/usr/bin/clangd"
 my-lsp-debug t
 my-org-agenda-files "~/org/"
 my-minimum-emacs-version "27.1"
 my-vagrant-vm-address "10.211.55.4#22"
 my-vagrant-vm-path "/Users/blandrum/beeswax/vms/serving"
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

;; Remove this!
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "PragmataPro Mono Liga" :foundry "nil" :slant normal :weight normal :height 181 :width normal)))))

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
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(flycheck clojure-mode org-noter yaml-mode xterm-color which-key use-package treemacs-projectile speed-type solarized-theme protobuf-mode pdf-tools magit lsp-pyright helm-xref form-feed dockerfile-mode dired-du dap-mode counsel bazel-mode)))

(provide 'init)

;;; init.el ends here
