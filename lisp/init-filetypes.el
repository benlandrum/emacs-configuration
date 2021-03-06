;;; init-filetypes.el --- Add modes for different file types.

;;; Commentary:
;; Put all file types that don't need much customization here.

;;; Code:

(use-package yaml-mode)
(use-package dockerfile-mode)
(use-package bazel-mode)
(use-package protobuf-mode)

(add-to-list 'auto-mode-alist '("\\.txx\\'" . c++-mode))

(defun my-js-mode-hook ()
  "Custom `js-mode' behavior."
  (setq indent-tabs-mode nil))
(add-hook 'js-mode-hook 'my-js-mode-hook)

(provide 'init-filetypes)

;;; init-filetypes.el ends here
