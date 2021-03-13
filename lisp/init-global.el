;; Quickly access the predefined master init file with C-x r j e
(set-register ?e (cons 'file my-init-file))

(setq ring-bell-function 'ignore)
(setq column-number-mode t)

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

(eval-when-compile
  ;; Following line is not needed if use-package.el is in ~/.emacs.d
  ;; (add-to-list 'load-path "<path where use-package is installed>")
  (require 'use-package))

;; Textual rendering
(use-package form-feed
  :hook (emacs-lisp-mode emacs-compilation-mode))

(use-package which-key
  :config
  (which-key-mode))

(defun revert-buffer-no-confirm ()
  "Revert buffer without confirmation."
  (interactive) (revert-buffer t t))

(global-set-key (kbd "C-x C-b") 'ibuffer)

(provide 'init-global)
