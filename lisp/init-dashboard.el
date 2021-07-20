;;; init-dashboard.el --- Set the start-up dashboard.

;;; Commentary:
;; Right now, just uses the light Solarized theme.

;;; Code:

(use-package dashboard
  :config
  (dashboard-setup-startup-hook))

(provide 'init-dashboard)

;;; init-dashboard.el ends here
