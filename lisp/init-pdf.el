;;; init-pdf.el --- Customize PDF interaction.

;;; Commentary:
;; Really just customizing pdf-tools here.

;;; Code:

;; Keep this as a conditional to allow booting before epdfinfo gets set up.
;; Need to customize this variable later.
(when (boundp 'pdf-info-epdfinfo-program)
  (setq pdf-info-epdfinfo-program my-epdfinfo-program))

(use-package pdf-tools
	     :pin manual ;; manually update
	     :config

	     ;; Initialize.
	     (pdf-tools-install)

	     (setq-default pdf-view-display-size 'fit-width)

	     ;; More fine-grained zooming
	     (setq pdf-view-resize-factor 1.1)

	     ;; Use normal isearch
	     (define-key pdf-view-mode-map (kbd "C-s") 'isearch-forward)

	     ;; Annotation shortcuts
	     (define-key pdf-view-mode-map (kbd "h")
	       'pdf-annot-add-highlight-markup-annotation)
	     (define-key pdf-view-mode-map (kbd "t") 'pdf-annot-add-text-annotation)
	     (define-key pdf-view-mode-map (kbd "D") 'pdf-annot-delete))

(add-to-list 'auto-mode-alist '("\\.pdf\\'" . pdf-view-mode))

(provide 'init-pdf)

;;; init-pdf.el ends here
