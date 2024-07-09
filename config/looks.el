;; TODO config use-package here?
(use-package solarized-theme
  :ensure t)

;; Load main theme
(load-theme 'solarized-light t)

;; Set default font
(set-face-attribute 'default nil :family "Hack" :height 110)

(provide 'looks)
