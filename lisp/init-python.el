;;; init-python.el --- Customize elpy and virtual environments.

;;; Commentary:
;; Nothing yet.

;;; Code:

;; This is where pyvenv will put virtual environments.
(setenv "WORKON_HOME" my-workon-home-dir)

(use-package elpy
  :init
  (elpy-enable)
  :config
  (defalias 'workon 'pyvenv-workon))

(provide 'init-python)

;;; init-python.el ends here
