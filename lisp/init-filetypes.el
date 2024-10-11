;;; init-filetypes.el --- Add modes for different file types.

;;; Commentary:
;; Put all file types that don't need much customization here.

;;; Code:

(use-package yaml-mode)
(use-package dockerfile-mode)
(use-package bazel-mode)
(use-package protobuf-mode)
(use-package haskell-mode)
(use-package julia-mode)
(use-package terraform-mode)
(use-package vterm)
(use-package gnuplot)
(use-package gnuplot-mode)
(use-package julia-snail
  :requires vterm
  :hook (julia-mode . julia-snail-mode)
  :config
  (add-to-list 'display-buffer-alist
	       '("\\*julia" (display-buffer-reuse-window display-buffer-same-window)))
  (set-variable 'split-height-threshold 15))

(add-to-list 'auto-mode-alist '("\\.txx\\'" . c++-mode))

(add-hook 'text-mode-hook
          '(lambda ()
             (setq indent-tabs-mode nil)
             (setq tab-width 4)))

(defun my-js-mode-hook ()
  "Custom `js-mode' behavior."
  (setq indent-tabs-mode nil))
(add-hook 'js-mode-hook 'my-js-mode-hook)

;; ob-asymptote, org-babel, and asymptote.
;; ob-asymptote will migrate to org.
;; https://github.com/emacsmirror/org-contrib/commit/fff6c888065588527b1c1d7dd7e41c29ef767e17
(use-package org-contrib)
(org-babel-do-load-languages
 'org-babel-load-languages
 '((asymptote . t)
   (gnuplot . t)))

(add-to-list 'load-path my-asymptote-dir)
(autoload 'asy-mode "asy-mode.el" "Asymptote major mode." t)
(autoload 'lasy-mode "asy-mode.el" "hybrid Asymptote/Latex major mode." t)
(autoload 'asy-insinuate-latex "asy-mode.el" "Asymptote insinuate LaTeX." t)
(add-to-list 'auto-mode-alist '("\\.asy$" . asy-mode))

(provide 'init-filetypes)

;;; init-filetypes.el ends here
