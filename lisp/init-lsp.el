(use-package lsp-mode)

;; Important: stderr gets sent to /tmp/<process-name>-<id>~stderr
;; https://emacs-lsp.github.io/lsp-mode/page/remote/
;; I needed to see these and find out that pyls was on python2.
;; I installed the python3 version with pip3 install python-language-server.
(with-eval-after-load 'lsp-mode
  (add-hook 'lsp-mode-hook #'lsp-enable-which-key-integration)
  (require 'dap-cpptools)

  (lsp-register-client
   (make-lsp-client :new-connection (lsp-tramp-connection "pyls")
                    :major-modes '(python-mode)
                    :remote? t
                    :server-id 'pyls-remote)))

(when my-lsp-debug
  (setq lsp-log-io t
	lsp-print-performance t))
(setq lsp-clients-clangd-executable my-lsp-clients-clangd-executable
      lsp-idle-delay 0.1                  ; clangd is fast
      lsp-headerline-breadcrumb-enable t) ; be more ide-ish

(add-hook 'c-mode-hook 'lsp)
(add-hook 'cpp-mode-hook 'lsp)
(add-hook 'python-mode-hook 'lsp)

(use-package lsp-pyright)
(use-package lsp-treemacs)

(use-package dap-mode)

(provide 'init-lsp)
