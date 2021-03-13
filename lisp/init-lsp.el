;; Important: stderr gets sent to /tmp/<process-name>-<id>~stderr
;; https://emacs-lsp.github.io/lsp-mode/page/remote/
;; I needed to see these and find out that pyls was on python2.
;; I installed the python3 version with pip3 install python-language-server.
(use-package lsp-mode
  :init
  (setq lsp-keymap-prefix "C-c l"
	lsp-clients-clangd-executable my-lsp-clients-clangd-executable
	lsp-idle-delay 0.1                  ; clangd is fast
	lsp-headerline-breadcrumb-enable t) ; be more ide-ish
  (when my-lsp-debug
    (setq lsp-log-io t
	  lsp-print-performance t))
  :config (lsp-register-client
	   (make-lsp-client :new-connection (lsp-tramp-connection "pyls")
			    :major-modes '(python-mode)
			    :remote? t
			    :server-id 'pyls-remote))
  :hook ((c-mode . lsp)
	 (cpp-mode . lsp)
	 (python-mode . lsp)
	 (lsp-mode . lsp-enable-which-key-integration))
  :commands lsp)

(use-package lsp-pyright)
(use-package lsp-treemacs)

(use-package dap-mode)
(use-package dap-cpptools)

(provide 'init-lsp)
