;;; init-global.el --- Configure low-level Emacs options.

;;; Commentary:
;; Remove some annoyances (e.g., bells), and require use-package.

;;; Code:

;; Quickly access the predefined master init file with C-x r j e
(set-register ?e (cons 'file my-init-file))

(setq ring-bell-function 'ignore)
(setq column-number-mode t)

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

(defun revert-buffer-no-confirm ()
  "Revert buffer without confirmation."
  (interactive) (revert-buffer t t))

(global-set-key (kbd "C-x C-b") 'ibuffer)

(provide 'init-global)

;;; init-global.el ends here
