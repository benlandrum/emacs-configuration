;;; init-dashboard.el --- Set the start-up dashboard.  -*- lexical-binding: t; -*-

;;; Commentary:
;; Right now, just uses the light Solarized theme.

;;; Code:

;; Read file at path as list of lines.
;; https://www.reddit.com/r/emacs/comments/kkujqe/emacs_dashboard_configuration/
(defun read-lines (path)
  "Return a list of lines of a file at the given path."
  (with-temp-buffer
    (insert-file-contents path)
    (split-string (buffer-string) "\n" t)))

(use-package dashboard
  :config
  (if (file-readable-p my-quotes-path)
      (let ((lines (read-lines my-quotes-path)))
	(if lines
	    (setq dashboard-footer-messages lines)
	  (message "Skipping empty quotes file \"%s\"." my-quotes-path)))
    (message "No quotation file readable at \"%s\"." my-quotes-path))
  (dashboard-setup-startup-hook))

(provide 'init-dashboard)

;;; init-dashboard.el ends here
