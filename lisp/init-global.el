;;; init-global.el --- Configure low-level Emacs options.

;;; Commentary:
;; Remove some annoyances (e.g., bells), and require use-package.

;;; Code:

;; Quickly access the predefined master init file with C-x r j e
(set-register ?e (cons 'file my-init-file))

(setq ring-bell-function 'ignore)
(setq column-number-mode t)
(setq vc-follow-symlinks t)
(setq large-file-warning-threshold 100000000)

;; Prevent automatic customization of init.el.
(setq custom-file (concat user-emacs-directory my-custom-file))
(when (file-exists-p custom-file)
  (load custom-file))

;; Ignore Emacs 2.7 deprecated package warning messages.
(setq byte-compile-warnings '(cl-functions))

;; Type 'y' instead of 'yes' and Enter.
(defalias 'yes-or-no-p 'y-or-n-p)

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

(eval-when-compile
  ;; Following line is not needed if use-package.el is in ~/.emacs.d
  ;; (add-to-list 'load-path "<path where use-package is installed>")
  (require 'use-package))

(require 'use-package-ensure)
(setq use-package-always-ensure t)

(use-package form-feed
  :hook ((emacs-lisp-mode . form-feed-mode)
	 (emacs-compilation-mode . form-feed-mode)))

(use-package which-key
  :config
  (which-key-mode))

;; Useful for switching between full-screen Org-noter frames.
(global-set-key (kbd "C-x 5 n") 'select-frame-by-name)

(defun revert-buffer-no-confirm ()
  "Revert buffer without confirmation."
  (interactive) (revert-buffer t t))

(global-set-key (kbd "C-x C-b") 'ibuffer)

(use-package google-this
  :config
  (google-this-mode 1))

(use-package google-translate
  :custom
  (google-translate-backend-method 'curl)
  :config
  ;; See https://github.com/atykhonov/google-translate/issues/52
  (defun google-translate--search-tkk () "Search TKK." (list 430675 2721866130))
  (bind-keys*
   ("\C-ct" . google-translate-at-point)
   ("\C-cT" . google-translate-query-translate)))

(provide 'init-global)

;;; init-global.el ends here
