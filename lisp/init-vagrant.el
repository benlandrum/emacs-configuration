;;; init-vagrant.el --- Customize Vagrant VM.

;;; Commentary:
;; For signing into the VM and going to the home directory.

;;; Code:

(setq vagrant-is-running nil)

(defun start-vagrant ()
  "Call vagrant up (prompting for a password and probably not working), and assume Vagrant is running from now."
  (interactive)
  (with-temp-buffer
    (shell-command (format "cd %s && vagrant up" my-vagrant-vm-path) t))
  (setq vagrant-is-running t))

;; Connect to Vagrant VM
(defun vagrant-home-dir ()
  "Navigate to the Vagrant VM home directory."
  (interactive)
  (unless vagrant-is-running
    (message "Starting Vagrant")
    (start-vagrant))
  ;; Important: Switching from ssh to scp here made helm-find-files actually
  ;; work for cloud VMs on AWS.
  (find-file (format "/scp:vagrant@%s:~" my-vagrant-vm-address)))

(provide 'init-vagrant)

;;; init-vagrant.el ends here
