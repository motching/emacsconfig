;;Emacs config

;;??
(setq byte-compile-warnings '(cl-functions))

;; Set load path
(add-to-list 'load-path (expand-file-name "config" user-emacs-directory))

;; Tell Flycheck where to find files
;; TODO warning here
(setq flycheck-emacs-lisp-load-path 'inherit)

;; Load "modules"
(require 'general)
(require 'looks)

;;put backup files into a temporary directory
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

(require 'package)
(setq
 package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                    ("org" . "http://orgmode.org/elpa/")
                    ;;                 ("marmalade" . "https://marmalade-repo.org/packages/")
                    ("melpa" . "http://melpa.org/packages/")
                    ("melpa-stable" . "http://stable.melpa.org/packages/"))
 package-archive-priorities '(("melpa-stable" . 1)))

(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)

(use-package ansi-color
  :ensure t)





(add-hook 'xref-backend-functions #'dumb-jump-xref-activate)

(defun comment-or-uncomment-region-or-line ()
    "Comments or uncomments the region or the current line if there's no active region."
    (interactive)
    (let (beg end)
        (if (region-active-p)
            (setq beg (region-beginning) end (region-end))
            (setq beg (line-beginning-position) end (line-end-position)))
        (comment-or-uncomment-region beg end)))

(defun markdown-html (buffer)
    (princ (with-current-buffer buffer
      (format "<!DOCTYPE html><html><title>Impatient Markdown</title><xmp theme=\"united\" style=\"display:none;\"> %s  </xmp><script src=\"http://ndossougbe.github.io/strapdown/dist/strapdown.js\"></script></html>" (buffer-substring-no-properties (point-min) (point-max))))
    (current-buffer)))

;; Live render markdown
;; Go to http://localhost:8080/imp/
(defun serve-current-markdown ()
    (interactive)
    (httpd-start)
    (impatient-mode)
    (imp-set-user-filter 'markdown-html))

(use-package exec-path-from-shell
  :ensure t)

;;Undo tree
(global-undo-tree-mode)

;; Projectile
(projectile-mode +1)

;;bookmark cycle in all buffers
(setq bm-cycle-all-buffers t)
(setq bm-highlight-style 'bm-highlight-line-and-fringe)

;;this will help remember TRAMP sessions
(setq desktop-files-not-to-save "^$")
(setq desktop-buffers-not-to-save "^$")

(use-package magit
  :ensure t)

;; https://tsdh.org/posts/2022-08-01-difftastic-diffing-with-magit.html

(defun th/magit--with-difftastic (buffer command)
  "Run COMMAND with GIT_EXTERNAL_DIFF=difft then show result in BUFFER."
  (let ((process-environment
         (cons (concat "GIT_EXTERNAL_DIFF=difft --width="
                       (number-to-string (frame-width)))
               process-environment)))
    ;; Clear the result buffer (we might regenerate a diff, e.g., for
    ;; the current changes in our working directory).
    (with-current-buffer buffer
      (setq buffer-read-only nil)
      (erase-buffer))
    ;; Now spawn a process calling the git COMMAND.
    (make-process
     :name (buffer-name buffer)
     :buffer buffer
     :command command
     ;; Don't query for running processes when emacs is quit.
     :noquery t
     ;; Show the result buffer once the process has finished.
     :sentinel (lambda (proc event)
                 (when (eq (process-status proc) 'exit)
                   (with-current-buffer (process-buffer proc)
                     (goto-char (point-min))
                     (ansi-color-apply-on-region (point-min) (point-max))
                     (setq buffer-read-only t)
                     (view-mode)
                     (end-of-line)
                     ;; difftastic diffs are usually 2-column side-by-side,
                     ;; so ensure our window is wide enough.
                     (let ((width (current-column)))
                       (while (zerop (forward-line 1))
                         (end-of-line)
                         (setq width (max (current-column) width)))
                       ;; Add column size of fringes
                       (setq width (+ width
                                      (fringe-columns 'left)
                                      (fringe-columns 'right)))
                       (goto-char (point-min))
                       (pop-to-buffer
                        (current-buffer)
                        `(;; If the buffer is that wide that splitting the frame in
                          ;; two side-by-side windows would result in less than
                          ;; 80 columns left, ensure it's shown at the bottom.
                          ,(when (> 80 (- (frame-width) width))
                             #'display-buffer-at-bottom)
                          (window-width
                           . ,(min width (frame-width))))))))))))

(defun th/magit-show-with-difftastic (rev)
  "Show the result of \"git show REV\" with GIT_EXTERNAL_DIFF=difft."
  (interactive
   (list (or
          ;; If REV is given, just use it.
          (when (boundp 'rev) rev)
          ;; If not invoked with prefix arg, try to guess the REV from
          ;; point's position.
          (and (not current-prefix-arg)
               (or (magit-thing-at-point 'git-revision t)
                   (magit-branch-or-commit-at-point)))
          ;; Otherwise, query the user.
          (magit-read-branch-or-commit "Revision"))))
  (if (not rev)
      (error "No revision specified")
    (th/magit--with-difftastic
     (get-buffer-create (concat "*git show difftastic " rev "*"))
     (list "git" "--no-pager" "show" "--ext-diff" rev))))

(defun th/magit-diff-with-difftastic (arg)
  "Show the result of \"git diff ARG\" with GIT_EXTERNAL_DIFF=difft."
  (interactive
   (list (or
          ;; If RANGE is given, just use it.
          (when (boundp 'range) range)
          ;; If prefix arg is given, query the user.
          (and current-prefix-arg
               (magit-diff-read-range-or-commit "Range"))
          ;; Otherwise, auto-guess based on position of point, e.g., based on
          ;; if we are in the Staged or Unstaged section.
          (pcase (magit-diff--dwim)
            ('unmerged (error "unmerged is not yet implemented"))
            ('unstaged nil)
            ('staged "--cached")
            (`(stash . ,value) (error "stash is not yet implemented"))
            (`(commit . ,value) (format "%s^..%s" value value))
            ((and range (pred stringp)) range)
            (_ (magit-diff-read-range-or-commit "Range/Commit"))))))
  (let ((name (concat "*git diff difftastic"
                      (if arg (concat " " arg) "")
                      "*")))
    (th/magit--with-difftastic
     (get-buffer-create name)
     `("git" "--no-pager" "diff" "--ext-diff" ,@(when arg (list arg))))))

(transient-define-prefix th/magit-aux-commands ()
  "My personal auxiliary magit commands."
  ["Auxiliary commands"
   ("d" "Difftastic Diff (dwim)" th/magit-diff-with-difftastic)
   ("s" "Difftastic Show" th/magit-show-with-difftastic)])

(use-package helm
  :ensure t)

(use-package helm-git-grep
  :ensure t)

(use-package php-mode
  :ensure t)

(use-package treemacs
  :ensure t)

;;my keybindings
(global-set-key (kbd "C-c a") 'ace-window)
(global-set-key (kbd "C-s") 'swiper-isearch)
(global-set-key (kbd "C-c b a") 'bm-show-all)
(global-set-key (kbd "C-c b c") 'bm-toggle-cycle-all-buffers)
(global-set-key (kbd "C-c b k") 'bm-remove-all-all-buffers)
(global-set-key (kbd "C-c b r") 'bm-remove-all-current-buffer)
(global-set-key (kbd "C-c b t") 'bm-toggle)
(global-set-key (kbd "C-c b n") 'bm-next)
(global-set-key (kbd "C-c b p") 'bm-previous)
(global-set-key (kbd "C-c c 6") 'convert-to-es6)
(global-set-key (kbd "C-c d s") 'desktop-save)
(global-set-key (kbd "C-c d c") 'desktop-change-dir)
(global-set-key (kbd "C-c e l") 'list-flycheck-errors)
(global-set-key (kbd "C-c e f") 'eslint-fix) ;; doesn't work in new emacs?
(global-set-key (kbd "C-c e i") 'er-indent-region-or-buffer)
(global-set-key (kbd "C-c e p") 'prettier-prettify)
(global-set-key (kbd "C-c f w") 'write-file)
(global-set-key (kbd "C-c f d") 'delete-file)
(global-set-key (kbd "C-c g b") 'magit-blame-addition)
(global-set-key (kbd "C-c g d") 'magit-diff-popup)
(global-set-key (kbd "C-c g f") 'magit-file-popup)
(global-set-key (kbd "C-c g s") 'magit-status)
(global-set-key (kbd "C-c i d") 'insert-debugger)
(global-set-key (kbd "C-c i e") 'insert-console-error)
(global-set-key (kbd "C-c i j") 'insert-jsdoc-header)
(global-set-key (kbd "C-c i k") 'insert-ko-context-log)
(global-set-key (kbd "C-c i l") 'insert-console-log)
(global-set-key (kbd "C-c i w") 'insert-console-warning)
(global-set-key (kbd "C-c k") 'kill-whole-line)
;; (global-set-key (kbd "C-c l s") 'slack-start)
;; (global-set-key (kbd "C-c l c") 'slack-channel-select)
;; (global-set-key (kbd "C-c l i") 'slack-im-select)
(global-set-key (kbd "C-c m") 'mvn-compile)
(global-set-key (kbd "C-c m") 'mvn-compile)
(global-set-key (kbd "C-c n") 'narrow-split)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
(global-set-key (kbd "C-c r") 'revert-buffer)
(global-set-key (kbd "C-c s s") 'helm-git-grep)
(global-set-key (kbd "C-c s a") 'helm-git-grep-at-point)
(global-set-key (kbd "C-c s p") 'swiper-thing-at-point)
;; not necessarily a good idea to put multiple things under C-c t
;; TODO think about keybindings
(global-set-key (kbd "C-c t r") 'treemacs)
(global-set-key (kbd "C-c t l") (lambda () (interactive) (load-theme 'solarized-light t)))
(global-set-key (kbd "C-c t d") (lambda () (interactive) (load-theme 'solarized-dark t)))
(global-set-key (kbd "C-c w") 'delete-trailing-whitespace)
(global-set-key (kbd "C-c x") 'replace-regexp)
(global-set-key (kbd "M-;") 'comment-or-uncomment-region-or-line)

(defun er-indent-buffer ()
  "Indent the currently visited buffer."
  (interactive)
  (indent-region (point-min) (point-max)))



(defun er-indent-region-or-buffer ()
  "Indent a region if selected, otherwise the whole buffer."
  (interactive)
  (save-excursion
    (if (region-active-p)
        (progn
          (indent-region (region-beginning) (region-end))
          (message "Indented selected region."))
      (progn
        (er-indent-buffer)
        (message "Indented buffer.")))))

;;window splitting
(defun narrow-split()
  (interactive)
  (split-window-below -9))

;; slack
(defun trim-final-newline (string)
  (let ((len (length string)))
    (cond
     ((and (> len 0) (eql (aref string (- len 1)) ?\n))
      (substring string 0 (- len 1)))
     (t string))))

(defun get-string-from-file (filePath)
  "Return filePath's file content."
  (with-temp-buffer
    (insert-file-contents filePath)
    (buffer-string)))

(use-package alert
  :ensure t
  :commands (alert)
  :init
  (setq alert-default-style 'notifier))


;;whitespace-mode
(use-package whitespace
  :ensure t)
(setq whitespace-style '(face empty tabs lines-tail trailing))
(setq-default indent-tabs-mode nil)
(setq tab-width 4)
(global-whitespace-mode t)

;;faster than the default scp
(setq tramp-default-method "ssh")

;;ttracker settings
(setq tramp-default-user "deploy")
(setq tramp-default-host "trucktracker.net")

;; http://www.flycheck.org/manual/latest/index.html
(use-package flycheck
  :ensure t)
(setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc))
(setq flycheck-pos-tip-timeout 1)
(setq flycheck-display-errors-function nil)
(add-hook 'after-init-hook #'global-flycheck-mode)

(use-package paren
  :ensure t)
(setq show-paren-style 'parenthesis)
(show-paren-mode +1)

;;colored parentheses
(use-package rainbow-delimiters
  :ensure t)
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)



;; UTF-8
(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

;;ediff characterwise
(setq-default ediff-forward-word-function 'forward-char)

;;IDO
(defvar ido-enable-flex-matching nil)
(defvar ido-enable-last-directory-history nil)
(defvar ido-record-commands nil)
(defvar ido-max-work-directory-list 0)
(defvar ido-max-work-file-list 0)
(ido-mode 'buffers)

;;open multiple files;
(declare-function dired-get-marked-files "dired")

(defun dired-find-marked-files ()
  (interactive)
  (dolist (f (dired-get-marked-files))
    (find-file f)))

;;ivy compilation
(use-package counsel
  :ensure t
  :bind (("C-c j" . counsel-imenu)
         ("M-x" . counsel-M-x)
         ("C-x C-f" . counsel-find-file)
         ("C-X 4 b" . ivy-switch-buffer-other-window))
  :config (ivy-mode 1))

;;set emacs path from $PATH
(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))

;;load theme
(use-package dash
  :ensure t)
(use-package s
  :ensure t)

(-each
    (-map
     (lambda (item)
       (format "~/.emacs.d/elpa/%s" item))
     (-filter
      (lambda (item) (s-contains? "theme" item))
      (directory-files "~/.emacs.d/elpa/")))
  (lambda (item)
    (add-to-list 'custom-theme-load-path item)))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(connection-local-criteria-alist
   '(((:application tramp)
      tramp-connection-local-default-system-profile tramp-connection-local-default-shell-profile)))
 '(connection-local-profile-alist
   '((tramp-connection-local-darwin-ps-profile
      (tramp-process-attributes-ps-args "-acxww" "-o" "pid,uid,user,gid,comm=abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ" "-o" "state=abcde" "-o" "ppid,pgid,sess,tty,tpgid,minflt,majflt,time,pri,nice,vsz,rss,etime,pcpu,pmem,args")
      (tramp-process-attributes-ps-format
       (pid . number)
       (euid . number)
       (user . string)
       (egid . number)
       (comm . 52)
       (state . 5)
       (ppid . number)
       (pgrp . number)
       (sess . number)
       (ttname . string)
       (tpgid . number)
       (minflt . number)
       (majflt . number)
       (time . tramp-ps-time)
       (pri . number)
       (nice . number)
       (vsize . number)
       (rss . number)
       (etime . tramp-ps-time)
       (pcpu . number)
       (pmem . number)
       (args)))
     (tramp-connection-local-busybox-ps-profile
      (tramp-process-attributes-ps-args "-o" "pid,user,group,comm=abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ" "-o" "stat=abcde" "-o" "ppid,pgid,tty,time,nice,etime,args")
      (tramp-process-attributes-ps-format
       (pid . number)
       (user . string)
       (group . string)
       (comm . 52)
       (state . 5)
       (ppid . number)
       (pgrp . number)
       (ttname . string)
       (time . tramp-ps-time)
       (nice . number)
       (etime . tramp-ps-time)
       (args)))
     (tramp-connection-local-bsd-ps-profile
      (tramp-process-attributes-ps-args "-acxww" "-o" "pid,euid,user,egid,egroup,comm=abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ" "-o" "state,ppid,pgid,sid,tty,tpgid,minflt,majflt,time,pri,nice,vsz,rss,etimes,pcpu,pmem,args")
      (tramp-process-attributes-ps-format
       (pid . number)
       (euid . number)
       (user . string)
       (egid . number)
       (group . string)
       (comm . 52)
       (state . string)
       (ppid . number)
       (pgrp . number)
       (sess . number)
       (ttname . string)
       (tpgid . number)
       (minflt . number)
       (majflt . number)
       (time . tramp-ps-time)
       (pri . number)
       (nice . number)
       (vsize . number)
       (rss . number)
       (etime . number)
       (pcpu . number)
       (pmem . number)
       (args)))
     (tramp-connection-local-default-shell-profile
      (shell-file-name . "/bin/sh")
      (shell-command-switch . "-c"))
     (tramp-connection-local-default-system-profile
      (path-separator . ":")
      (null-device . "/dev/null"))))
 '(custom-safe-themes
   '("285d1bf306091644fb49993341e0ad8bafe57130d9981b680c1dbd974475c5c7" "0dd2666921bd4c651c7f8a724b3416e95228a13fca1aa27dc0022f4e023bf197" "653574dd35a64b45030075c99bb9e73f26d8abc7f21e145321e64fa2659fb6f5" "c433c87bd4b64b8ba9890e8ed64597ea0f8eb0396f4c9a9e01bd20a04d15d358" "00445e6f15d31e9afaa23ed0d765850e9cd5e929be5e8e63b114a3346236c44c" "30289fa8d502f71a392f40a0941a83842152a68c54ad69e0638ef52f04777a4c" "a8245b7cc985a0610d71f9852e9f2767ad1b852c2bdea6f4aadc12cce9c4d6d0" "4cf3221feff536e2b3385209e9b9dc4c2e0818a69a1cdb4b522756bcdf4e00a4" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" default))
 '(helm-git-grep-candidate-number-limit nil)
 '(js-indent-level 4)
 '(js2-bounce-indent-p t)
 '(magit-diff-use-overlays nil)
 '(org-startup-folded 'showeverything)
 '(org-startup-truncated nil)
 '(package-selected-packages
   '(dap-mode web-mode rjsx-mode tide which-key lsp-mode 0x0 php-mode apache-mode geben w3m impatient-mode impatient-showdown erefactor fzf magit find-file-in-project prettier one-themes silkworm-theme plan9-theme xcscope counsel-etags yaml-mode flycheck-yamllint less-css-mode elm-mode bm undo-tree org-jira js-doc company-tern tern counsel eslint-fix ivy paredit buffer-move sass-mode json-mode flx-ido helm-projectile projectile live-py-mode flycheck-pycheckers helm-git-grep vimish-fold exec-path-from-shell mvn rainbow-delimiters hindent ghc ghc-imported-from ghci-completion scion treemacs solarized-theme js2-closure helm flycheck dockerfile-mode))
 '(safe-local-variable-values
   '((vc-prepare-patches-separately)
     (diff-add-log-use-relative-names . t)
     (vc-git-annotate-switches . "-w")))
 '(tab-width 4)
 '(treemacs-space-between-root-nodes nil)
 '(undo-tree-auto-save-history nil)
 '(undo-tree-visualizer-timestamps t)
 '(warning-suppress-types '((comp) (comp)))
 '(web-mode-enable-auto-indentation nil))

(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))
(add-to-list 'auto-mode-alist '("\\.yaml$" . yaml-mode))

;; customize flycheck temp file prefix
(setq-default flycheck-temp-prefix ".flycheck")

;; disable json-jsonlist checking for json files
(setq-default flycheck-disabled-checkers
              (append flycheck-disabled-checkers
                      '(json-jsonlist)))

;; this checker sucks too
(setq-default flycheck-disabled-checkers
              (append flycheck-disabled-checkers
                      '(haskell-stack-ghc)))

;;error buffer hack
(setq split-height-threshold 0)
(setq split-width-threshold nil)

;;fold
(use-package vimish-fold
  :ensure t)
(vimish-fold-global-mode 1)


(defun my-c-hook ()
  (setq c-basic-offset 2)
  (use-package xcscope
    :ensure t)
  (cscope-setup))


(require 'web)

(add-hook 'c-mode-hook #'my-c-hook)

(add-hook 'html-mode-hook
          (lambda ()
            ;; Default indentation is usually 2 spaces, changing to 4.
            (defvar sgml-basic-offset)
            (set (make-local-variable 'sgml-basic-offset) 4)))

;; my macros
(defalias 'insert-console-log
   (kmacro "C-a RET C-p <tab> c o n s o l e . l o g ( ) ; C-b C-b"))

(defalias 'insert-console-warning
   (kmacro "C-a RET C-p <tab> c o n s o l e . w a r n ( ) ; C-b C-b"))

(defalias 'insert-console-error
   (kmacro "C-a RET C-p <tab> c o n s o l e . e r r o r ( ) ; C-b C-b"))

(defalias 'insert-debugger
   (kmacro "C-a RET C-p <tab> i f SPC ( t r u e ) SPC { SPC d e b u g g e r ; SPC } C-b C-b C-b C-b C-b C-b C-b C-b C-b C-b C-b C-b C-b C-b C-b C-b C-b C-b C-b"))

(fset 'insert-ko-context-log
      (kmacro-lambda-form [up ?\C-e return ?< ?s ?p ?a ?n ?  ?d ?a ?t ?a ?- ?b ?i ?n ?d ?= ?t ?e ?x ?t ?: ?  ?f ?u ?n ?c ?t ?i ?o ?n ?  ?\( ?\) ?\S-  ?\{ ?c ?o ?n ?s ?o ?l ?e ?. ?l ?o ?g ?\( ?\) ?\; ?\} ?  ?\( ?\) ?\C-e ?> ?< ?/ left left left left left left left left] 0 "%d"))

(fset 'insert-jsdoc-header
   (kmacro-lambda-form [?\C-a return up tab ?/ ?* ?* return ?* tab return ?* ?/ tab up ? ] 0 "%d"))

;; End:
(put 'narrow-to-region 'disabled nil)
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
