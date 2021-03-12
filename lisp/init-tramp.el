;; Need the require and the 'tramp-own-remote path line.
;; Without that line, TRAMP overwrites the variable.
;; Note: I had to pipx install pyls.
(require 'tramp)
(add-to-list 'tramp-remote-path "~/bin")
(add-to-list 'tramp-remote-path "~/.local/bin")
(add-to-list 'tramp-remote-path 'tramp-own-remote-path)

;; flycheck is causing freezes.  I disabled it.  I also removed flymake.
;; Also, when TRAMP freezes, try tramp-cleanup-this-connection.
;; https://stackoverflow.com/questions/23582421/emacs-tramp-hangs-on-saving-opening-a-current-buffer-after-suspend-resume

;; Go to definition is not working well either.

(provide 'init-tramp)
