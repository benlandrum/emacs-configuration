;;; init-vagrant.el --- Customize Vagrant VM.

;;; Commentary:
;; For signing into the VM and going to the home directory.

;;; Code:

(setq vagrant-is-running nil)

(defun start-vagrant ()
  "Call vagrant up, and assume Vagrant is running from now on."
  (interactive)
  (with-temp-buffer
    (shell-command "cd /Users/blandrum/beeswax/vms/serving && vagrant up" t))
  (setq vagrant-is-running t))

;; Connect to Vagrant VM
(defun vagrant-home-dir ()
  (interactive)
  (unless vagrant-is-running
    (message "Starting Vagrant")
    (start-vagrant))
  (find-file "/ssh:vagrant@10.211.55.4#22:~"))

(provide 'init-vagrant)

;;; init-vagrant.el ends here
