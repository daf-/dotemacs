;; Some simple global Settings
;; Daniel Friedman, Fall 2013


;; for comment func
(setq mark-even-if-inactive nil)

;; global keys
(global-set-key (kbd "s-<return>") 'ns-toggle-fullscreen)
;; C-left/right/up/down resize the window
(global-set-key (kbd "C-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "C-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "C-<down>") 'shrink-window)
(global-set-key (kbd "C-<up>") 'enlarge-window)

;; minor modes
(transient-mark-mode t)    ;; show regions as highlighted
(column-number-mode t)     ;; shows column number in modeline
(size-indication-mode t)   ;; show buffer size in modeline
(when (>= emacs-major-version 24) (electric-indent-mode t))
(show-paren-mode t)
(scroll-bar-mode 0)
(global-hl-line-mode t)
(iswitchb-mode t)
(ido-mode t)

;; global settings
(make-variable-buffer-local 'global-hl-line-mode)
(if window-system
    (tool-bar-mode 0))
;; (global-linum-mode 1)
;; (setq linum-format " ")
;; (setq scroll-conservatively 1)
(setq scroll-margin 5)
(if (not (window-system))
    (menu-bar-mode -1))

;; allows mouse in terminal
(unless window-system
  (require 'mouse)
  (xterm-mouse-mode t)
  (defun track-mouse(e))
  (setq mouse-sel-mode t))

;; Don't prompt for "really want to exit?" when I still have processes running
(defadvice save-buffers-kill-emacs (around no-query-kill-emacs activate)
  "Prevent annoying \"Active processes exist\" query when you quit Emacs."
  (let ((process-list ())) ad-do-it))

;; woman
(setq woman-use-topic-at-point t)


;; style
(setq c-default-style "k&r")
(setq-default indent-tabs-mode nil)

; no backup files
(setq make-backup-files nil) ; prevents creation of backup files on first save
(setq backup-inhibited t)    ; never make backups
(setq auto-save-default nil) ; disables auto save

(provide 'daf-global-settings)
