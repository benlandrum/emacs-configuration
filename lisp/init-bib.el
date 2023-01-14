;;; init-bib.el --- Customize bibliographic packages.

;;; Commentary:
;; Only a single package so far.

;;; Code:

(use-package biblio :ensure t)

(use-package ebib
  :ensure t
  :bind (("C-c e" . ebib)
	 (:map ebib-index-mode-map
	       ("B" . ebib-biblio-import-doi))
	 (:map biblio-selection-mode-map
	       ("e" . ebib-biblio-selection-import)))
  :config
  (setq ebib-bib-search-dirs my-ebib-bib-search-dirs)
  (setq ebib-preload-bib-files my-ebib-preload-bib-files))

(provide 'init-bib)
