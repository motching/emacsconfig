;; Disable splash screen.
(setq inhibit-startup-message t)

;; TODO does this mess with autoimports?
(setq default-directory "~/")

;; Show stack trace, etc. when a lisp error happens.
(setq debug-on-error 1)

;; Makes sure the last line is terminated. If it's not, several tools are confused.
(setq require-final-newline t)

;; Prevent accidental quitting.
(setq confirm-kill-emacs 'yes-or-no-p)

;; We don't need lockfiles.
(setq create-lockfiles nil)

;; Hide bars
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)

;; Sane handling of selected text
(delete-selection-mode 1)

;; Replace in rectangles
;; TODO documentation for this
(cua-selection-mode 1)

;; Smooth scrolling
;; TODO Emacs 29 can do this natively??
(setq mouse-wheel-scroll-amount '(3 ((shift) . 3))
      mouse-wheel-progressive-speed nil
      mouse-wheel-follow-mouse 't
      scroll-step 1)
(setq scroll-conservatively 101)

;; Auto-revert (e.g. changing git branches)
(global-auto-revert-mode t)

;; Automatically close parens
;; TODO not directly related but electric indent mode inserts unwanted spaces sometimes
(electric-pair-mode 1)

;; Display column number
(setq column-number-mode t)

;; Cycle through buffers (like Firefox)
;; TODO tabbing back? Showing the tabs in a thumbnail? A better tabbing solution?
(global-set-key (kbd "<C-tab>") 'bury-buffer)

;; Line numbering
(global-display-line-numbers-mode)

;; Set scratch message
(setq initial-scratch-message "Far and away the best prize that life has to offer is the chance to work hard at work worth doing.")

;; TODO this opens a new emacs instance from magit which is not nice
;; emacsclient SOMETIMES works
(setenv "EDITOR" "emacs")

(setq desktop-path '("~/.emacs.d"))
(desktop-save-mode 1)

(provide 'general)
