;; Uncomment this to enable error debug on start-up.
;; (setq debug-on-error t)

;; Global variables
(setq
 my-init-file "~/.emacs.d/init.el"
 my-epdfinfo-program "/Users/blandrum/.local/bin/epdfinfo"
 my-lsp-clients-clangd-executable "/ssh:vagrant@10.211.55.4#22:/usr/bin/clangd"
 my-lsp-debug t
 my-org-agenda-files "~/org/"
 )

;; Remove this!
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "PragmataPro Mono Liga" :foundry "nil" :slant normal :weight normal :height 181 :width normal)))))

(add-to-list 'load-path "~/.emacs.d/lisp/")
(let ((file-name-handler-alist nil)  ; anti-regex speed loading hack
      (gc-cons-threshold 100000000)) ; delay garbage collection
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
