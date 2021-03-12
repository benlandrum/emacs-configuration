;; From Tikhon Jelvis's config...
;; https://www.quora.com/What-does-Tikhon-Jelviss-Emacs-setup-look-like
(defun new-shell (name) 
  "Opens a new shell buffer with the given name in 
asterisks (*name*) in the current directory and changes the 
prompt to 'name>'." 
  (interactive "sName: ") 
  (pop-to-buffer (concat "*" name "*")) 
  (unless (eq major-mode 'shell-mode)
    (shell (current-buffer)) 
    (sleep-for 0 200) 
    (delete-region (point-min) (point-max)) 
    (comint-simple-send (get-buffer-process (current-buffer))  
					;(concat "export PS1=\"\033[33m" name "\033[0m:\033[35m\\W\033[0m>\""))))
			(concat "export PS1=\"" name ": \""))
    (comint-simple-send (get-buffer-process (current-buffer))
			"alias ls=\"TERM=ansi ls --color=always\"")
    ))
(global-set-key (kbd "C-c s") 'new-shell)

(defun new-term (name) 
  "Opens a new term buffer with the given name in 
asterisks (*name*) in the current directory and changes the 
prompt to 'name>'." 
  (interactive "sName: ") 
  (pop-to-buffer (concat "*" name "*")) 
  (unless (eq major-mode 'term-mode) 
    (term "/bin/zsh")
    (sleep-for 0 200) 
    (delete-region (point-min) (point-max)) 
    (comint-simple-send (get-buffer-process (current-buffer))  
					;(concat "export PS1=\"\033[33m" name "\033[0m:\033[35m\\W\033[0m>\""))))
			(concat "export PS1=\"" name ": \""))
    (comint-simple-send (get-buffer-process (current-buffer))
			"alias ls=\"TERM=ansi ls --color=always\"")
    ))
(global-set-key (kbd "C-c t") 'new-term)

;; Compilation buffers: https://github.com/atomontage/xterm-color
(setq compilation-environment '("TERM=xterm-256color"))

(defun my/advice-compilation-filter (f proc string)
  (funcall f proc (xterm-color-filter string)))

(advice-add 'compilation-filter :around #'my/advice-compilation-filter)

;; Other: https://github.com/atomontage/xterm-color
(let ((buffer (generate-new-buffer "*xterm-color-test*")))
  (with-current-buffer buffer
    (insert (xterm-color-filter "\x1b[0;1;3;4"))
    (insert (xterm-color-filter ";35"))
    (insert (xterm-color-filter ";51mThis is only a test"))
    (insert (xterm-color-filter "\x1b[0m")))
  (switch-to-buffer buffer))

;; Comint: https://github.com/atomontage/xterm-color
(setq comint-output-filter-functions
      (remove 'ansi-color-process-output comint-output-filter-functions))

(add-hook 'shell-mode-hook
          (lambda ()
            ;; Disable font-locking in this buffer to improve performance
            (font-lock-mode -1)
            ;; Prevent font-locking from being re-enabled in this buffer
            (make-local-variable 'font-lock-function)
            (setq font-lock-function (lambda (_) nil))
            (add-hook 'comint-preoutput-filter-functions 'xterm-color-filter nil t)))

(use-package xterm-color)

(provide 'init-shell)
