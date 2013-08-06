(require 'funcs)

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
;; C-left/right/up/down moves the window
(global-set-key (kbd "C-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "C-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "C-<down>") 'shrink-window)
(global-set-key (kbd "C-<up>") 'enlarge-window)
;; Cycle windows backwards as well
(global-set-key (kbd "C-x O") (lambda ()
                                (interactive)
                                (other-window -1)))
;; Insert an extra newline when hitting C-j between braces
;; undo-tree
(global-set-key (kbd "s-Z") 'undo-tree-redo)
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
(define-key personal-map "t" '(lambda () (interactive) (ansi-term "bash")))
(define-key personal-map " " 'ace-jump-mode)
(define-key personal-map "i" (lambda ()
                               (interactive)
                               (find-file "~/.emacs.d/init.el")))

;; global minor modes
(transient-mark-mode t)    ;; show regions as highlighted
(delete-selection-mode t)  ;; can type over a highlighted region
(column-number-mode t)     ;; shows column number in modeline
(size-indication-mode t)   ;; show buffer size in modeline
(show-paren-mode t)
(setq show-paren-delay 0)
(global-hl-line-mode t)
(iswitchb-mode t)
(ido-mode t)
;; (make-variable-buffer-local 'global-hl-line-mode)
(global-auto-revert-mode t)

;; fix annoyances
(setq inhibit-startup-screen t)
(setq initial-scratch-message)
(setq comint-prompt-read-only t)
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(setq linum-format "%d ")
(setq scroll-conservatively 1)
(when (display-graphic-p)               ; much better GUI scrolling
  (progn
    (setq mouse-wheel-scroll-amount '(0.01))
    (setq mouse-wheel-progressive-speed nil)))

; show unicode in ansi-term mode
(defadvice ansi-term (after advise-ansi-term-coding-system activate)
  (set-buffer-process-coding-system 'utf-8-unix 'utf-8-unix))

(setq make-backup-files nil) ; prevents creation of backup files on first save
(setq backup-inhibited t)    ; never make backups
(setq auto-save-default nil) ; disables auto save

; allow mouse in terminal
(unless window-system
  (require 'mouse)
  (xterm-mouse-mode t)
  (defun track-mouse(e))
  (setq mouse-sel-mode t))

; Don't prompt for "really want to exit?" when I still have processes running
(defadvice save-buffers-kill-emacs (around no-query-kill-emacs activate)
  "Prevent annoying \"Active processes exist\" query when you quit Emacs."
  (let ((process-list ())) ad-do-it))

;;
;; style
;;

(setq c-default-style "k&r")
(setq-default indent-tabs-mode nil)


;;; Plugin settings
;; ag
(setq ag-highlight-search t)
(global-set-key (kbd "<f5>") 'ag-project)
(global-set-key (kbd "<f6>") 'ag-regexp-project-at-point)

;; skewer-mode
(add-hook 'js2-mode-hook 'skewer-mode)
(add-hook 'css-mode-hook 'skewer-css-mode)
(add-hook 'html-mode-hook 'skewer-html-mode)



;;; Advice
(defadvice newline-and-indent (before newline-and-indent-dwim activate)
  (newline-and-indent-dwim))

(defadvice evil-ret-and-indent (before newline-and-indent-dwim activate)
  (newline-and-indent-dwim))

; don't leave my terminal buffer hanging around when I'm done with it
; taken from the sweet blog post at
; http://emacs-journey.blogspot.com/2012/06/improving-ansi-term.html
(defadvice term-sentinel (around my-advice-term-sentinel (proc msg) activate)
  (if (memq (process-status proc) '(signal exit))
      (let ((buffer (process-buffer proc)))
        ad-do-it
        (kill-buffer buffer))
    ad-do-it))

(provide 'global-settings)
