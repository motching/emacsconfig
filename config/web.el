(use-package prettier
  :ensure t)

(use-package indium
  :ensure t)

;; for better jsx syntax-highlighting in web-mode
;; - courtesy of Patrick @halbtuerke
(defadvice web-mode-highlight-part (around tweak-jsx activate)
  (if (equal web-mode-content-type "jsx")
      (let ((web-mode-enable-part-face nil))
        ad-do-it)
    ad-do-it))

(use-package amd-mode
  :ensure t)

;; use eslint with rjsx-mode for jsx files
(flycheck-add-mode 'javascript-eslint 'rjsx-mode)

;; disable jshint since we prefer eslint checking
(setq-default flycheck-disabled-checkers
              (append flycheck-disabled-checkers
                      '(javascript-jshint)))

(defun setup-tide-mode()
  "Setu1p function for tide."
  (interactive)
  (flycheck-mode +1)
  (tide-setup)
  (setq flycheck-check-syntax-automatically `(idle-change mode-enabled))
  (tide-hl-identifier-mode +1)
  (company-mode +1))


(use-package tide
  :ensure t
  :after (rjsx-mode company flycheck)
  :hook (rjsx-mode . setup-tide-mode))

;;flycheck's syntax checking should be superior to js2-mode
(setq js2-mode-show-parse-errors nil)
(setq js2-mode-show-strict-warnings nil)

(setq-default js2-global-externs '("define"
                                   "module"
                                   "require"
                                   "buster"
                                   "sinon"
                                   "assert"
                                   "refute"
                                   "setTimeout"
                                   "clearTimeout"
                                   "setInterval"
                                   "clearInterval"
                                   "location"
                                   "__dirname"
                                   "console"
                                   "JSON"
                                   "google"
                                   "Audio"))

(setq-default js2-idle-timer-delay 0.1)
(add-to-list 'auto-mode-alist '("\\.js$" . rjsx-mode))
(add-to-list 'auto-mode-alist '("\\.swift$" . swift-mode))

;;javascript
(use-package rjsx-mode
  :ensure t
  :mode "\\js\\'")


(use-package web-mode
  :ensure t)

(use-package swift-mode
  :ensure t)

;; use web-mode for .jsx files
(add-to-list 'auto-mode-alist '("\\.jsx$" . web-mode))

;and html
(add-to-list 'auto-mode-alist '("\\.html$" . web-mode))

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
