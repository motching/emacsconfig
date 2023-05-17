(use-package prettier
  :ensure t)


(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 1))

(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :init
  (setq lsp-keymap-prefix "C-c l")
  :config
  (lsp-enable-which-key-integration t))

(use-package typescript-mode
  :mode "\\.ts\\'"
  :hook (typescript-mode . lsp-deferred))

(use-package js2-mode
  :mode "\\.js\\'"
  :hook (js2-mode . lsp-deferred))

;;completion
(use-package company
  :after lsp-mode
  :hook (lsp-mode . company-mode)
  :bind (:map company-active-map
         ("<tab>" . company-complete-selection))
        (:map lsp-mode-map
         ("<tab>" . company-indent-or-complete-common))
  :custom
  (company-minimum-prefix-length 1)
  (company-dabbrev-downcase nil)
  (company-idle-delay 0))

(use-package dap-mode
  :after
  lsp-mode
  :config
  (dap-auto-configure-mode)

  :bind
  (("<f7>" . dap-step-in)
   ("<f8>" . dap-next)
   ("<f9>" . dap-continue)))


(use-package dap-mode
  :config
  (require 'dap-firefox)
  )

;; https://emacs-lsp.github.io/lsp-mode/page/performance/
(setq gc-cons-threshold 100000000) ;100MB
(setq read-process-output-max (* 1024 1024)) ;; 1Mb

;; (use-package company
;;   :ensure t)

;; (add-hook 'after-init-hook 'global-company-mode)
;; (setq company-idle-delay 0)
;; (setq company-dabbrev-downcase nil)


;; (use-package indium
;;   :ensure t)

;; for better jsx syntax-highlighting in web-mode
;; - courtesy of Patrick @halbtuerke
;; (defadvice web-mode-highlight-part (around tweak-jsx activate)
;;   (if (equal web-mode-content-type "jsx")
;;       (let ((web-mode-enable-part-face nil))
;;         ad-do-it)
;;     ad-do-it))

;; use eslint with rjsx-mode for jsx files
;; TODO more generic flycheck
;; (flycheck-add-mode 'javascript-eslint 'rjsx-mode)

;; disable jshint since we prefer eslint checking
;; (setq-default flycheck-disabled-checkers
;;               (append flycheck-disabled-checkers
;;                       '(javascript-jshint)))

;; (defun setup-tide-mode()
;;   "Setu1p function for tide."
;;   (interactive)
;;   (flycheck-mode +1)
;;   (tide-setup)
;;   (setq flycheck-check-syntax-automatically `(idle-change mode-enabled))
;;   (tide-hl-identifier-mode +1)
;;   (company-mode +1))


;; (use-package tide
;;   :ensure t
;;   :after (rjsx-mode company flycheck)
;;   :hook (rjsx-mode . setup-tide-mode))

;;flycheck's syntax checking should be superior to js2-mode
;; (setq js2-mode-show-parse-errors nil)
;; (setq js2-mode-show-strict-warnings nil)

;; (setq-default js2-global-externs '("define"
;;                                    "module"
;;                                    "require"
;;                                    "buster"
;;                                    "sinon"
;;                                    "assert"
;;                                    "refute"
;;                                    "setTimeout"
;;                                    "clearTimeout"
;;                                    "setInterval"
;;                                    "clearInterval"
;;                                    "location"
;;                                    "__dirname"
;;                                    "console"
;;                                    "JSON"
;;                                    "google"
;;                                    "Audio"))

;; (setq-default js2-idle-timer-delay 0.1)
;; (add-to-list 'auto-mode-alist '("\\.js$" . rjsx-mode))
;; (add-to-list 'auto-mode-alist '("\\.swift$" . swift-mode))

;; ;;javascript
;; (use-package rjsx-mode
;;    :ensure t
;;    :mode "\\js\\'")


;; (use-package web-mode
;;    :ensure t)

;; ;; use web-mode for .jsx files
;;  (add-to-list 'auto-mode-alist '("\\.jsx$" . web-mode))

;; ;and html
;; (add-to-list 'auto-mode-alist '("\\.html$" . web-mode))

(defun convert-to-es6 ()
  "Convert SOME ES5 style stuff to ES6. Definitely not comprehensive"
  (interactive)

  (goto-char (point-min))
  (while (search-forward-regexp ";$" nil t)
    (replace-match ""))

  (goto-char (point-min))
  (while (search-forward-regexp "^ *var " nil t)
    (replace-match "let "))

  (goto-char (point-min))
  (while (search-forward-regexp "{ " nil t)
    (replace-match "{"))

  (goto-char (point-min))
  (while (search-forward-regexp " }" nil t)
    (replace-match "}"))

  (goto-char (point-min))
  (while (search-forward-regexp "//[^ ]" nil t)
    (replace-match "// "))

  (goto-char (point-min))
  (while (search-forward-regexp "function(" nil t)
    (replace-match "function ("))

  (er-indent-buffer)
)

(provide 'web)
