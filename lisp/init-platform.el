(when (string= system-type "darwin")
  (setq dired-use-ls-dired nil
	ns-alternate-modifier '(:ordinary super :function super :mouse super)
	ns-command-modifier 'meta))

(provide 'init-platform)
