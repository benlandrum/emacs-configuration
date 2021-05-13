;;; init-appearance.el --- Set the theme and editor appearance.

;;; Commentary:
;; Right now, just uses the light Solarized theme.

;;; Code:

(use-package solarized-theme)
(load-theme 'solarized-light t)

;; Hide the menu bar, tool bar, and scroll bars.
(when (fboundp 'menu-bar-mode)   (menu-bar-mode   -1))
(when (fboundp 'tool-bar-mode)   (tool-bar-mode   -1))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

(provide 'init-appearance)

;;; init-appearance.el ends here
