;;; init-helm.el --- Configure helm.

;;; Commentary:
;; Only a little bit of customization here so far.  Not really used yet.

;;; Code:

(use-package helm
  :config
  (helm-mode)
  (define-key global-map [remap find-file] #'helm-find-files)
  (define-key global-map [remap execute-extended-command] #'helm-M-x)
  (define-key global-map [remap switch-to-buffer] #'helm-mini))

(use-package helm-xref)

(provide 'init-helm)

;;; init-helm.el ends here
