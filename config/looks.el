;; TODO config use-package here?
(use-package solarized-theme
  :ensure t)

;; Load main theme
(load-theme 'solarized-light t)

;; Set default font
;; TODO this is not the best font
(set-face-attribute 'default nil
                    :family "Monoid"
                    :height  110
                    :weight 'normal
                    :width 'normal)

(provide 'looks)
