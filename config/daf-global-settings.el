;; Some simple global Settings
;; Daniel Friedman, Fall 2013

;;; set PATH to recognize homebrew, supercollider
(setenv "PATH"
  (concat
   "~/bin" ":"
   "/usr/local/bin" ":"
   (getenv "PATH")))
(push "/Applications/SuperCollider/Contents/Resources" exec-path)

;;; Delete trailing whitespace before saving
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;;; global keys
;; C-left/right/up/down resize the window
(global-set-key (kbd "C-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "C-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "C-<down>") 'shrink-window)
(global-set-key (kbd "C-<up>") 'enlarge-window)
;; expand-region
(global-set-key (kbd "C-=") 'er/expand-region)
;; multiple-cursors
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)
(global-unset-key (kbd "M-<down-mouse-1>"))
(global-set-key (kbd "M-<mouse-1>") 'mc/add-cursor-on-click)


;; personal bindings
(define-prefix-command 'personal-map)
(global-set-key (kbd "C-c") personal-map)

(define-key personal-map "." 'bookmark-set)
(define-key personal-map "/" 'bookmark-jump)
(define-key personal-map ";" 'comment-or-uncomment-line-or-region)
(define-key personal-map "c" 'compile)
(define-key personal-map "f" 'find-file-other-window)
(define-key personal-map "g" 'magit-status)
(define-key personal-map "l" 'goto-line)
(define-key personal-map "p" 'run-python)
(define-key personal-map "s" 'eshell)
(define-key personal-map "t" 'ansi-term)
(define-key personal-map " " 'ace-jump-mode)
(define-key personal-map "i" (lambda ()
                               (interactive)
                               (find-file "~/.emacs.d/init.el")))

;; minor modes
(transient-mark-mode t)    ;; show regions as highlighted
(delete-selection-mode t)  ;; can type over a highlighted region
(column-number-mode t)     ;; shows column number in modeline
(size-indication-mode t)   ;; show buffer size in modeline
(show-paren-mode t)
;; (setq show-paren-style 'expression)
(setq show-paren-delay 0)
(scroll-bar-mode 0)
;; (global-hl-line-mode t)
(iswitchb-mode t)
(ido-mode t)
(make-variable-buffer-local 'global-hl-line-mode)

;; fix annoyances
(setq inhibit-startup-screen t)
(setq initial-scratch-message)
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(setq linum-format "%d ")
(setq scroll-conservatively 1)
(when (display-graphic-p)               ; much better GUI scrolling
  (progn
    (setq mouse-wheel-scroll-amount '(0.01))
    (setq mouse-wheel-progressive-speed nil)))

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

;; gdb
(setq gdb-many-windows t)

;; style
(setq c-default-style "k&r")
(setq-default indent-tabs-mode nil)

; no backup files
(setq make-backup-files nil) ; prevents creation of backup files on first save
(setq backup-inhibited t)    ; never make backups
(setq auto-save-default nil) ; disables auto save

(provide 'daf-global-settings)
