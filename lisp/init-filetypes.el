;;; init-filetypes.el --- Add modes for different file types.

;;; Commentary:
;; Put all file types that don't need much customization here.

;;; Code:

(use-package yaml-mode)
(use-package dockerfile-mode)
(use-package bazel)
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

;; Org-mode updates force a tab-width of 8.
;; (add-hook 'text-mode-hook
;;           '(lambda ()
;;              (setq indent-tabs-mode nil)
;;              (setq tab-width 4)))

;; https://www.reddit.com/r/emacs/comments/15y7j41/suggestions_on_my_configuration_of_auctex/
(defun my-tex-save-and-compile ()
  (interactive)
  (let (TeX-save-query) (TeX-save-document (TeX-master-file)))
  (TeX-command-run-all nil))

(use-package tex
  :pin gnu
  :ensure auctex
  :mode ("\\.tex\\$" . latex-mode)
  :bind (("<f5>" . 'my-tex-save-and-compile)))

(defun my-js-mode-hook ()
  "Custom `js-mode' behavior."
  (setq indent-tabs-mode nil))
(add-hook 'js-mode-hook 'my-js-mode-hook)

(use-package ob-asymptote)

;; ob-asymptote, org-babel, and asymptote.
;; ob-asymptote will migrate to org.
;; https://github.com/emacsmirror/org-contrib/commit/fff6c888065588527b1c1d7dd7e41c29ef767e17
(use-package org-contrib)
(org-babel-do-load-languages
 'org-babel-load-languages
 '((asymptote . t)
   (gnuplot . t)
   (python . t)))

(add-to-list 'load-path my-asymptote-dir)
(autoload 'asy-mode "asy-mode.el" "Asymptote major mode." t)
(autoload 'lasy-mode "asy-mode.el" "hybrid Asymptote/Latex major mode." t)
(autoload 'asy-insinuate-latex "asy-mode.el" "Asymptote insinuate LaTeX." t)
(add-to-list 'auto-mode-alist '("\\.asy$" . asy-mode))

(provide 'init-filetypes)

;;; init-filetypes.el ends here
