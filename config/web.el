(use-package prettier
  :ensure t)

;;TODO does this belong here?
(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 1))

;; use our own eslint
(defun my/use-eslint-from-node-modules ()
  (let* ((root (locate-dominating-file
                (or (buffer-file-name) default-directory)
                "node_modules"))
         (eslint
          (and root
               (expand-file-name "node_modules/.bin/eslint"
                                 root))))
    (when (and eslint (file-executable-p eslint))
      (setq-local flycheck-javascript-eslint-executable eslint))))

(add-hook 'flycheck-mode-hook #'my/use-eslint-from-node-modules)

(use-package json-mode
  :ensure t
  :mode "\\.json\\'")

(add-hook 'json-mode-hook
          (lambda ()
            (setq-local js-indent-level 2)))

;; Disable underlining for early returns in js2-mode
(defun my-rjsx-mode-hook ()
  (setq-local js2-highlight-external-variables nil)
  (setq-local js2-highlight-level 1))

(use-package rjsx-mode
  :ensure t
  :mode "\\js\\'"
    :hook (rjsx-mode . lsp-deferred)
    :hook (rjsx-mode . my-rjsx-mode-hook))

(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :init
  (setq lsp-keymap-prefix "C-c l")
  :config
  (lsp-enable-which-key-integration t))

;; We don't want to see superfluous messages from LSP
;; (defvar unwanted-messages '("Connected to" "Disconnected"))

;; (defun disable-lsp-msg-advice (original-lsp--info &rest r)
;;   (unless (seq-some #'identity (mapcar (lambda (unwanted-message) (funcall #'string-prefix-p ((car r) unwanted-message) unwanted-messages)
;;     ;;(unless (string-prefix-p "Connected to" (car r))
;;     (apply original-lsp--info r)))

;; (advice-add 'lsp--info :around #'disable-lsp-msg-advice)

(use-package typescript-mode
  :mode "\\.ts\\'"
  :hook (typescript-mode . lsp-deferred))

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

;; (use-package dap-mode
;;   :after
;;   lsp-mode
;;   :config
;;   (dap-auto-configure-mode)

;;   :bind
;;   (("<f7>" . dap-step-in)
;;    ("<f8>" . dap-next)
;;    ("<f9>" . dap-continue)))


;; (use-package dap-mode
;;   :config
;;   (require 'dap-firefox)
;;   )

;; https://emacs-lsp.github.io/lsp-mode/page/performance/
(setq gc-cons-threshold 100000000) ;100MB
(setq read-process-output-max (* 1024 1024)) ;; 1Mb


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

(use-package web-mode
  :ensure t
  :mode "\\html\\'")

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
