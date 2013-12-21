(require 'funcs)

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
;; personal bindings
(global-set-key (kbd "A-s") 'save-buffer)
(global-set-key (kbd "A-S") 'ido-write-file)
(global-set-key (kbd "A-z") 'undo-tree-undo)
(global-set-key (kbd "A-Z") 'undo-tree-redo)
(global-set-key (kbd "A-q") 'save-buffers-kill-terminal)
(global-set-key (kbd "A-k") 'kill-this-buffer)
(global-set-key (kbd "A-K") 'kill-buffer-and-window)
(global-set-key (kbd "A-1") 'delete-other-windows)
(global-set-key (kbd "A-2") 'daf-two-window-setup)
(global-set-key (kbd "A-3") 'daf-three-window-setup)
(global-set-key (kbd "A-o") 'other-window)
(global-set-key (kbd "A-RET") 'toggle-fullscreen)


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
(define-key personal-map "s" 'split-eshell)
(define-key personal-map "t" 'split-ansi-term)
(define-key personal-map " " 'ace-jump-mode)
(define-key personal-map "i" (lambda ()
                               (interactive)
                               (find-file "~/.emacs.d/init.el")))
(define-key personal-map "b" 'sr-speedbar-toggle)

;; global minor modes
(transient-mark-mode t)    ;; show regions as highlighted
(delete-selection-mode t)  ;; can type over a highlighted region
(column-number-mode t)     ;; shows column number in modeline
(size-indication-mode t)   ;; show buffer size in modeline
(setq show-paren-delay 0)
(show-paren-mode t)
(setq show-paren-style 'expression)
;; (global-hl-line-mode)
(iswitchb-mode t)
(ido-mode t)
(setq ido-enable-flex-matching t
      ido-create-new-buffer 'always)
(global-auto-revert-mode t)
(electric-indent-mode t)
(electric-pair-mode t)
(setq speedbar-select-frame-method 'attached)

; type-break-mode... awesome
;; (setq type-break-interval 2700)
;; (setq type-break-demo-functions '(zone))
;; (type-break-mode t)

;; fix annoyances
(setq inhibit-startup-screen t)
(setq initial-scratch-message)
(setq comint-prompt-read-only t)
(setq linum-format "%d ")
(setq scroll-conservatively 1)
(setq make-backup-files nil) ; prevents creation of backup files on first save
(setq backup-inhibited t)    ; never make backups
(setq auto-save-default nil) ; disables auto save
(setq visible-bell t)        ; disable annoying beeping
(when (display-graphic-p)    ; much better GUI scrolling
  (progn
    ;; (setq mouse-wheel-progressive-speed nil)
    (setq mouse-wheel-scroll-amount '(0.01))))

;; show unicode in ansi-term mode
(defadvice ansi-term (after advise-ansi-term-coding-system activate)
  (set-buffer-process-coding-system 'utf-8-unix 'utf-8-unix))

;; allow mouse in terminal
(unless window-system
  (require 'mouse)
  (xterm-mouse-mode t)
  (defun track-mouse(e))                ; what is this doing?
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
(require 'paredit)
(global-flycheck-mode 1)

(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

;; auto-complete
(require 'auto-complete-config)
(require 'auto-complete)
(ac-config-default)
(global-auto-complete-mode 1)
(setq ac-delay 0.001)

;; ido
(require 'flx-ido)
(ido-mode 1)
(flx-ido-mode 1)
(setq ido-use-faces nil)
(require 'ido-vertical-mode)
(ido-vertical-mode)
(require 'ido-ubiquitous)               ; better than ido-everywhere
(ido-ubiquitous-mode 1)

;; yasnippet
(yas-global-mode)
(setq yas-prompt-functions '(yas-ido-prompt))

;; ag
(setq ag-highlight-search t)
(global-set-key (kbd "<f5>") 'ag-project)
(global-set-key (kbd "<f6>") 'ag-regexp-project-at-point)

;; mouse+
(require 'mouse+)
(global-set-key [down-mouse-2]        'mouse-flash-position-or-M-x)
(global-set-key [S-down-mouse-2]      'mouse-scan-lines-or-M-:)
(global-set-key [mode-line C-mouse-1] 'mouse-tear-off-window)
(global-set-key [minibuffer mouse-3]        '(lambda ()
                                              (interactive)
                                              (ido-find-file)
                                              (end-of-line)))

;; projectile
(projectile-global-mode 1)
(setq projectile-remember-window-configs t
      projectile-use-git-grep t)
(global-set-key (kbd "A-p") 'projectile-switch-project)
(global-set-key (kbd "A-f") 'projectile-find-file)
(global-set-key (kbd "A-b") 'projectile-switch-to-buffer)

;; js2
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))

;; web-mode (see web-mode.org)
(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.jsp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
;; for plain html:
;; (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))

;; scss-mode
;; (setq scss-compile-at-save nil)

;; processing
(setq processing-sketchbook-dir "~/code/Processing")

;; undo-tree
(global-undo-tree-mode 1)
(global-set-key (kbd "s-Z") 'undo-tree-redo)

;; expand-region
(global-set-key (kbd "C-=") 'er/expand-region)

;; multiple-cursors
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)
(global-unset-key (kbd "M-<down-mouse-1>"))
(global-set-key (kbd "M-<mouse-1>") 'mc/add-cursor-on-click)

;; evil-numbers
(global-set-key (kbd "C-c +") 'evil-numbers/inc-at-pt)
(global-set-key (kbd "C-c -") 'evil-numbers/dec-at-pt)

;; daylight
(setq daylight-morning-theme 'solarized-light
      daylight-afternoon-theme 'monokai
      daylight-evening-theme 'zenburn
      daylight-late-theme 'spacegray)
(daylight-mode 1)

;;; Filetype stuff
;; ruby
(add-to-list 'auto-mode-alist '("\\.rake\\'" . ruby-mode))
(add-to-list 'auto-mode-alist '("Rakefile\\'" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.gemspec\\'" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.ru\\'" . ruby-mode))
(add-to-list 'auto-mode-alist '("Gemfile\\'" . ruby-mode))
(add-to-list 'auto-mode-alist '("Guardfile\\'" . ruby-mode))
(add-to-list 'auto-mode-alist '("Capfile\\'" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.thor\\'" . ruby-mode))
(add-to-list 'auto-mode-alist '("Thorfile\\'" . ruby-mode))
(add-to-list 'auto-mode-alist '("Vagrantfile\\'" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.jbuilder\\'" . ruby-mode))
;; C++
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))
;; scheme
(setq scheme-program-name "racket")

(provide 'global-settings)
