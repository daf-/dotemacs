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
(setq mac-option-modifier 'super)
(setq mac-command-modifier 'meta)
(setq ns-function-modifier 'hyper)

;; C-left/right/up/down moves the window
(global-set-key (kbd "C-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "C-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "C-<down>") 'shrink-window)
(global-set-key (kbd "C-<up>") 'enlarge-window)

;; Cycle windows backwards as well
(global-set-key (kbd "C-x O") (lambda ()
                                (interactive)
                                (other-window -1)))
;; mouse
(global-set-key (kbd "<S-down-mouse-1>") 'mouse-yank-at-click) ; same as middle mouse, but for trackpad
(global-set-key (kbd "<mode-line> <C-mouse-1>") 'mouse-split-window-vertically)
(setq mouse-drag-copy-region t)

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
(setq show-paren-style 'expression)
(global-hl-line-mode t)
(iswitchb-mode t)
(ido-mode t)
(setq ido-enable-flex-matching t)
(global-auto-revert-mode t)
(electric-indent-mode t)
(electric-pair-mode t)
; type-break-mode... awesome
(setq type-break-interval 2700)
(setq type-break-demo-functions '(zone))
(type-break-mode t)

;; fix annoyances
(setq inhibit-startup-screen t)
(setq initial-scratch-message)
(setq comint-prompt-read-only t)
(setq linum-format "%d ")
(setq scroll-conservatively 1)
(setq make-backup-files nil) ; prevents creation of backup files on first save
(setq backup-inhibited t)    ; never make backups
(setq auto-save-default nil) ; disables auto save
(when (display-graphic-p)               ; much better GUI scrolling
  (progn
    (setq mouse-wheel-scroll-amount '(0.01))))

;; show unicode in ansi-term mode
(defadvice ansi-term (after advise-ansi-term-coding-system activate)
  (set-buffer-process-coding-system 'utf-8-unix 'utf-8-unix))

;; allow mouse in terminal
(unless window-system
  (require 'mouse)
  (xterm-mouse-mode t)
  (defun track-mouse(e))                ; what is this doing
  (setq mouse-sel-mode t))

;; Don't prompt for "really want to exit?" when I still have processes running
(defadvice save-buffers-kill-emacs (around no-query-kill-emacs activate)
  "Prevent annoying \"Active processes exist\" query when you quit Emacs."
  (let ((process-list ())) ad-do-it))

;;;
;;; style
;;;

(setq c-default-style "k&r")
(setq-default indent-tabs-mode nil)


;;; Plugin settings
;; yasnippet
(setq yas-prompt-functions '(yas-ido-prompt))

;; ag
(setq ag-highlight-search t)
(global-set-key (kbd "<f5>") 'ag-project)
(global-set-key (kbd "<f6>") 'ag-regexp-project-at-point)

;; skewer-mode
(add-hook 'js2-mode-hook 'skewer-mode)
(add-hook 'css-mode-hook 'skewer-css-mode)
(add-hook 'html-mode-hook 'skewer-html-mode)

;; multi-web-mode
(require 'multi-web-mode)
(setq mweb-default-major-mode 'html-mode)
(setq mweb-tags '((php-mode "<\\?php\\|<\\? \\|<\\?=" "\\?>")
                  (js-mode "<script +\\(type=\"text/javascript\"\\|language=\"javascript\"\\)[^>]*>" "</script>")
                  (css-mode "<style +type=\"text/css\"[^>]*>" "</style>")))
(setq mweb-filename-extensions '("php" "htm" "html" "ctp" "phtml" "php4" "php5"))
(multi-web-global-mode 1)



;;; Advice
(defadvice newline-and-indent (before newline-and-indent-dwim activate)
  (newline-and-indent-dwim))

(defadvice newline (before newline-dwim activate)
  (if electric-indent-mode
      (newline-and-indent-dwim)))

(defadvice evil-ret-and-indent (before newline-and-indent-dwim activate)
  (newline-and-indent-dwim))

(defadvice paredit-mode (before paredit-turn-off-electric-pair-mode activate)
  "Disable electric-pair-mode when we use paredit."
  (if electric-pair-mode
      (electric-pair-mode -1)))

;; don't leave my terminal buffer hanging around when I'm done with it
;; taken from the sweet blog post at
;; http://emacs-journey.blogspot.com/2012/06/improving-ansi-term.html
(defadvice term-sentinel (around my-advice-term-sentinel (proc msg) activate)
  (if (memq (process-status proc) '(signal exit))
      (let ((buffer (process-buffer proc)))
        ad-do-it
        (kill-buffer buffer))
    ad-do-it))

(provide 'global-settings)
