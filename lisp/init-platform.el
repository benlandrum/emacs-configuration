;;; init-platform.el --- OS-specific settings.

;;; Commentary:
;; Just adding paths and support for Apple operating systems here.

;;; Code:

(let ((usr-local-bin-path "/usr/local/bin"))
  (setenv "PATH" (concat (getenv "PATH") ":" usr-local-bin-path))
  (setq exec-path (append exec-path (list usr-local-bin-path))))

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

;;; init-platform.el ends here
