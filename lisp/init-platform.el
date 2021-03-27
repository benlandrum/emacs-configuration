(when (string= system-type "darwin")
  (setq dired-use-ls-dired nil
	ns-alternate-modifier '(:ordinary super
				:function super
				:mouse super)
	ns-command-modifier 'meta)
  (let ((texbin-path "/Library/TeX/texbin"))
    (setenv "PATH" (concat (getenv "PATH") ":" texbin-path))
    (setq exec-path (append exec-path (list texbin-path)))))

(provide 'init-platform)
