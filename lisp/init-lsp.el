;;; init-lsp.el --- Configure LSP mode and clients here.

;;; Commentary:
;; This is a work in progress.  LSP sometimes crashes and stalls.

;;; Code:

;(use-package flycheck :init (global-flycheck-mode))

;; Important: stderr gets sent to /tmp/<process-name>-<id>~stderr
;; https://emacs-lsp.github.io/lsp-mode/page/remote/
;; I needed to see these and find out that pyls was on python2.
;; I installed the python3 version with pip3 install python-language-server.
(use-package lsp-mode
  :init
  (setq lsp-keymap-prefix "C-c l"
	lsp-clients-clangd-args '("--j=1" "--background-index=true" "--log=verbose")
	lsp-clients-clangd-executable my-lsp-clients-clangd-executable
	lsp-idle-delay 0.1                  ; clangd is fast
	lsp-headerline-breadcrumb-enable t) ; be more ide-ish
  (when my-lsp-debug
    (setq lsp-log-io t
	  lsp-print-performance t))
  :config
  ;; (lsp-register-client
  ;;  (make-lsp-client :new-connection (lsp-tramp-connection "pyls")
  ;; 		    :major-modes '(python-mode)
  ;; 		    :remote? t
  ;; 		    :server-id 'pyls-remote))
  (lsp-register-client
   (make-lsp-client :new-connection (lsp-tramp-connection "clangd")
		    :major-modes '(c-mode c++-mode)
		    :remote? t
		    :server-id 'clangd-remote))
  :hook ((c-mode . lsp)
	 (cpp-mode . lsp)
	 ;; Running into library problems.
	 ;(python-mode . lsp)
	 (lsp-mode . lsp-enable-which-key-integration))
  :commands lsp
  :config
  ;; Following the advice below, after finding eldoc causing slowness.
  ;; https://ianyepan.github.io/posts/emacs-ide/
  ;; It suggests other lsp-* features to disable.
  (setq lsp-eldoc-hook nil))

;; Running into library problems.
;(use-package lsp-pyright)

(use-package lsp-treemacs)

(use-package dap-mode)
;(use-package dap-cpptools)

(provide 'init-lsp)

;;; init-lsp.el ends here
