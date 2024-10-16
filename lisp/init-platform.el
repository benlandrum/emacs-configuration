;;; init-platform.el --- OS-specific settings.

;;; Commentary:
;; Just adding paths and support for Apple operating systems here.
;; I might not need a lot of this code after using exec-path-from-shell.

;;; Code:

(use-package exec-path-from-shell)
(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))

(defun my-add-to-env-path (p)
  (setenv "PATH" (concat (getenv "PATH") ":" p))
  (push p exec-path))

(my-add-to-env-path "/usr/local/bin")

(when (string= system-type "darwin")
  (setq dired-use-ls-dired nil
	ns-alternate-modifier '(:ordinary super
				:function super
				:mouse super)
	ns-command-modifier 'meta)
  (my-add-to-env-path "/Library/TeX/texbin"))

(provide 'init-platform)

;;; init-platform.el ends here
