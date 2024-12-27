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
      (setq-local flycheck-javascript-eslint-executable eslint)
      (setq-local eslint-fix-executable eslint))))

(add-hook 'flycheck-mode-hook #'my/use-eslint-from-node-modules)

;; TODO this needs to use the custom eslint as well
(use-package eslint-fix
  :ensure t)

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
  :ensure t
  :commands (lsp lsp-deferred)
  :init
  (setq lsp-keymap-prefix "C-c l")
  :config
  (lsp-enable-which-key-integration t)
  :hook (lsp-mode . setup-checker-chain))

;; https://github.com/emacs-lsp/lsp-mode/issues/2594
(defun setup-checker-chain ()
  (require 'lsp-diagnostics)
  (lsp-diagnostics-flycheck-enable)
  (flycheck-add-next-checker 'lsp 'javascript-eslint))

(use-package lsp-ui
  :ensure t
  :hook (lsp-mode . lsp-ui-mode)
  )
(use-package lsp-treemacs :ensure t)

(use-package typescript-mode
  :mode "\\.ts\\'"
  :hook (typescript-mode . lsp-deferred))

;;completion
(use-package company
  :ensure t
  :after lsp-mode
  :hook (lsp-mode . company-mode)
  :bind (:map company-active-map
         ("<tab>" . company-complete-selection))
        (:map lsp-mode-map
         ("<tab>" . company-indent-or-complete-common))
  :custom
  (company-minimum-prefix-length 1)
  (company-dabbrev-downcase nil)
  (company-idle-delay 0.0))

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
