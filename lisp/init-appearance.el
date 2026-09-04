;;; init-appearance.el --- Set the theme and editor appearance.  -*- lexical-binding: t; -*-

;;; Commentary:
;; Interaction with themes, menu, tool, and scroll bars.

;;; Code:

;; Commenting out for Omarchy.
;(use-package solarized-theme)
;(load-theme 'solarized-light t)

(load (file-name-concat user-emacs-directory "omarchy.el"))

;; Hide the menu bar, tool bar, and scroll bars.
(when (fboundp 'menu-bar-mode)   (menu-bar-mode   -1))
(when (fboundp 'tool-bar-mode)   (tool-bar-mode   -1))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

(provide 'init-appearance)

;;; init-appearance.el ends here
