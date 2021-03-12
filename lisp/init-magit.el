(use-package magit)

;; The vc branch line doesn't stay current without (setq auto-revert-check-vc-info t)
;; Supposedly this has bad performance, and I don't need it with Magit.
(setq-default mode-line-format
              (delete '(vc-mode vc-mode) mode-line-format))

;; Magit customizations.  This one could be slow.
(add-hook 'after-save-hook 'magit-after-save-refresh-status t)

(provide 'init-magit)
