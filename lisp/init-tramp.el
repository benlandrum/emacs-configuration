;;; init-tramp.el --- Configure tramp.

;;; Commentary:
;; This is a work in progress.

;;; Code:

(use-package tramp
  :config
  ;; Need the require and the 'tramp-own-remote path line.
  ;; Without that line, TRAMP overwrites the variable.
  ;; Note: I had to pipx install pyls.
  (add-to-list 'tramp-remote-path "~/bin")
  (add-to-list 'tramp-remote-path "~/.local/bin")
  (add-to-list 'tramp-remote-path 'tramp-own-remote-path)
  (when my-tramp-verbose (setq tramp-verbose my-tramp-verbose))
  (setq tramp-default-method my-tramp-default-method))

;; Example multi-hop paths.
;; /ssh:vm-user@vm.example.net|docker:user@container:/path/to/file
;; /sudo:root@localhost|docker:user@container:/path/to/file
;; Supposedly no longer needed as of Emacs 29.
;; (use-package docker-tramp)

;; flycheck is causing freezes.  I disabled it.  I also removed flymake.
;; Also, when TRAMP freezes, try tramp-cleanup-this-connection.
;; https://stackoverflow.com/questions/23582421/emacs-tramp-hangs-on-saving-opening-a-current-buffer-after-suspend-resume

;; Go to definition is not working well either.

(provide 'init-tramp)

;;; init-tramp.el ends here
