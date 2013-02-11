;; Some simple global Settings
;; Daniel Friedman, Fall 2013


;; colors -- only for emacs 24
;; (when (>= emacs-major-version 24)
;;     (load-theme 'twilight t))


;; global keys

;; C-left/right/up/down resize the window
(global-set-key (kbd "C-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "C-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "C-<down>") 'shrink-window)
(global-set-key (kbd "C-<up>") 'enlarge-window)

;; minor modes
(transient-mark-mode t)    ;; show regions as highlighted
(column-number-mode t)     ;; shows column number in modeline
(size-indication-mode t)   ;; show buffer size in modeline
(electric-indent-mode t)
(global-hl-line-mode t)
(make-variable-buffer-local 'global-hl-line-mode)
(if window-system
    (tool-bar-mode 0))
;; (global-linum-mode 1)
;; (setq scroll-conservatively 1)
(setq scroll-margin 5)
(if (not (window-system))
    (menu-bar-mode -1))

;; better switching between buffers, fuzzy matching
(iswitchb-mode t)
(ido-mode t)

;; Show matching parentheses for lisp editing
;; Highlight the entire parenthesized expression for easy visual understanding
(show-paren-mode t)
;; (setq show-paren-style 'expression)

;; allows mouse in terminal
(unless window-system
  (require 'mouse)
  (xterm-mouse-mode t)
  (defun track-mouse(e))
  (setq mouse-sel-mode t))


;; formatting / variables

;; Don't prompt for "really want to exit?" when I still have processes running
(defadvice save-buffers-kill-emacs (around no-query-kill-emacs activate)
  "Prevent annoying \"Active processes exist\" query when you quit Emacs."
  (let ((process-list ())) ad-do-it))

;; woman
(setq woman-use-topic-at-point t)


;; style

;; sets indentation style
(setq c-default-style "k&r")
; always use spaces instead of tabs
(setq-default indent-tabs-mode nil)
; return is newline & indent
(define-key global-map (kbd "RET") 'newline-and-indent)
; no backup files
(setq make-backup-files nil) ; prevents creation of backup files on first save
(setq backup-inhibited t)    ; never make backups
(setq auto-save-default nil) ; disables auto save
;;(set-face-attribute 'default nil
;;                    :family "menlo" :height 130)

(provide 'daf-global-settings)
