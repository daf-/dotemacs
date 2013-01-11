(setq vc-handled-backends nil)

(when (< emacs-major-version 24)
  (add-to-list 'load-path "~/.emacs.d/"))
(require 'package)
(setq package-archives '(("marmalade" . "http://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")))
(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

(defvar daf-packages '(auto-complete
                       color-theme
                       color-theme-solarized
                       evil
                       evil-leader
                       golden-ratio
                       magit
                       python-mode
                       undo-tree
                       yasnippet))
(dolist (p daf-packages)
  (when (not (package-installed-p p))
    (package-install p)))


;; initialize plugins
(require 'popup)
(require 'auto-complete-config)
(require 'evil-leader)
; (setq yas-snippet-dirs "~/.emacs.d/elpa/yasnippet-20121225.430/snippets")

(ac-config-default)
(add-to-list 'custom-theme-load-path "~/.emacs.d/elpa/color-theme-solarized-20121209.1204")
;; (load-theme 'solarized-dark t)
(evil-mode 1)
(golden-ratio-enable)
(yas-global-mode 1)
;; theming depends on major version
(if (and (>= emacs-major-version 24) (window-system))
    (load-theme 'deeper-blue t)
  (if (and (>= emacs-major-version 24) (not (window-system)))
      (load-theme 'solarized-dark t)
    ((color-theme-initialize)
     (color-theme-solarized-dark))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ------ PLUGIN SETTINGS ------ ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; YASnippet -- makes TAB work in terminal
(defun yas/advise-indent-function (function-symbol)
  (eval `(defadvice ,function-symbol (around yas/try-expand-first activate)
           ,(format
             "Try to expand a snippet before point, then call `%s' as usual"
             function-symbol)
           (let ((yas/fallback-behavior nil))
             (unless (and (interactive-p)
                          (yas/expand))
           ad-do-it)))))
(yas/advise-indent-function 'comment-indent-new-line)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ------- KEY MAPPINGS ------- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; M-SPC toggles evil/emacs state
(evil-set-toggle-key "M-SPC")

;; esc quits -- modified from http://stackoverflow.com/questions/8483182/emacs-evil-mode-best-practice
(define-key evil-normal-state-map [escape] 'keyboard-quit)
(define-key evil-visual-state-map [escape] 'keyboard-quit)
(define-key evil-motion-state-map [escape] 'keyboard-quit)
(define-key minibuffer-local-map [escape] 'abort-recursive-edit)
(define-key minibuffer-local-ns-map [escape] 'abort-recursive-edit)
(define-key minibuffer-local-completion-map [escape] 'abort-recursive-edit)
(define-key minibuffer-local-must-match-map [escape] 'abort-recursive-edit)
(define-key minibuffer-local-isearch-map [escape] 'abort-recursive-edit)

;; Maps "kj" to escape
;; taken from http://zuttobenkyou.wordpress.com/2011/02/15/some-thoughts-on-emacs-and-vim/ -- fantastic blog entry on vim->emacs
(define-key evil-insert-state-map "k" #'cofi/maybe-exit)
(define-key evil-replace-state-map "k" #'cofi/maybe-exit)
(evil-define-command cofi/maybe-exit ()
 :repeat change
 (interactive)
 (let ((modified (buffer-modified-p)))
   (insert "k")
   (let ((evt (read-event (format "Insert %c to exit insert state" ?j)
			   nil 0.5)))
     (cond
      ((null evt) (message ""))
      ((and (integerp evt) (char-equal evt ?j))
	(delete-char -1)
	(set-buffer-modified-p modified)
	(push 'escape unread-command-events))
      (t (setq unread-command-events (append unread-command-events
					      (list evt))))))))

;; Minimize hand fatigue
;; window mappings
(define-key evil-normal-state-map " " #'evil-toggle-fold)
(define-key evil-normal-state-map (kbd "C-h") #'evil-window-left)
(define-key evil-normal-state-map (kbd "C-j") #'evil-window-down)
(define-key evil-normal-state-map (kbd "C-k") #'evil-window-up)
(define-key evil-normal-state-map (kbd "C-l") #'evil-window-right)
(define-key evil-motion-state-map (kbd "C-h") #'evil-window-left)
(define-key evil-motion-state-map (kbd "C-j") #'evil-window-down)
(define-key evil-motion-state-map (kbd "C-k") #'evil-window-up)
(define-key evil-motion-state-map (kbd "C-l") #'evil-window-right)

;; Navigation and Editing
;; (setq evil-want-C-u-scroll t) ;; why isn't this working?
(define-key evil-normal-state-map (kbd "C-u") 'evil-scroll-up)
(define-key evil-insert-state-map (kbd "C-e") 'move-end-of-line)
(define-key evil-normal-state-map (kbd "TAB") #'evil-indent-line)
(define-key evil-visual-state-map (kbd "TAB") #'evil-indent)


;; Leader maps
(evil-leader/set-leader ",")
(evil-leader/set-key "," 'evil-repeat-find-char-reverse)
(define-key evil-motion-state-map "," evil-leader/map)  ;; allows leader in motion state, too
(evil-leader/set-key "x" ctl-x-map)
(evil-leader/set-key "f" 'find-file)
(evil-leader/set-key "l" 'load-file)
(evil-leader/set-key "c" 'comment-or-uncomment-region)
(evil-leader/set-key "s" 'save-buffer)
(evil-leader/set-key "t" 'ansi-term)
(evil-leader/set-key "g" 'magit-status)
(evil-leader/set-key "q" 'evil-quit)

;; Fun comments with boxing
(defun box-comment() ;; "defun" is a macro for defining named functions in emacs lisp
  ;; This is the docstring
  "Creates a box-style comment. Visually appealing, and useful for having
different types of comments section our your code more thoroughly."
  (interactive "*") ;; What kind of arguments does this function accept? (none, in this case)
  (let
    ((comment-style 'box))
    (comment-region
     (region-beginning)
     (region-end))))

(defun box-uncomment()
  "Destroys a box created with box-comment, leaving the original contents
intact. More specifically, it makes two calls to uncomment-region in
order to remove the comment characters from the front and back of the
box, then it attempts to remove the blank lines left over by this operation."
  (interactive "*")
  (let
      ((comment-style 'box)
       (clear1 (region-beginning))
       (clear2 (region-end)))
    
    ;; Uncomment twice: once for front comments, another for back comments
    (dotimes (i 2)
      (uncomment-region
       (region-beginning)
       (region-end)))
    
    (delete-trailing-whitespace)
    (save-excursion
      (save-restriction
	(goto-char (region-beginning))
	(forward-line -1)
	(setq clear1 (point))
	(save-excursion
	  (goto-char (region-end))
	  (beginning-of-line)
	  (forward-line 2)
	  (setq clear2 (point)))
	(narrow-to-region clear1 clear2)
	(delete-blank-lines)
	(goto-char (region-end))
	(delete-blank-lines)))))


;; This is useful for macbooks, where C-/ has been cleverly
;; bound to "make the terminal beep" by Apple. Thanks, Apple!
(global-set-key "\M-/" 'undo)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ------- MODE-HOOKS ------- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TODO: add mode-hooks for evil mode; e.g. optargs for shell-script-mode

(defun my-text-mode-hook()
  (turn-on-auto-fill)	;; allows text-wrapping
  (set-fill-column 80)	;; how many characters before we wrap?
  (cond
   ((eq major-mode "text-mode")	;; Allow comments in text-mode
    (setq comment-start "#"))))
(add-hook 'text-mode-hook 'my-text-mode-hook)
(setq default-major-mode 'text-mode) ;; default is fundamental-mode

;; Simplifies typing in the classic j-mantra.
(defun java-insert-main ()
  (interactive "*")
  (save-excursion
      (set-mark (point))
      (insert "public static void main (String[] args) {")
      (newline 2)
      (insert "}")
      (indent-region (region-beginning) (region-end)))
    (forward-line 1)
    (c-indent-line))

;; TODO: evillify
(defun my-java-mode-hook()
  (local-set-key (kbd "C-c C-c") 'compile)
  (local-set-key (kbd "C-c m") 'java-insert-main)
  (cond
   (if (fboundp 'c-subword-mode) (c-subword-mode t))
   (if (fboundp 'subword-mode) (subword-mode t))))
(add-hook 'java-mode-hook 'my-java-mode-hook)

;; TODO: why won't setting evil-lookup-func work?
(defun my-python-mode-hook()
  (setq evil-lookup-func 'py-documentation))
(add-hook 'python-mode-hook 'my-python-mode-hook)

;; make magit evil
(defun my-evil-magit-mode-hook()
  (define-key evil-motion-state-map (kbd "SPC") 'magit-toggle-section))
(add-hook 'magit-mode-hook 'my-evil-magit-mode-hook)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ------ MINOR MODES ------ ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(transient-mark-mode t)	                ;; show regions as highlighted
(column-number-mode t)	                ;; shows column number in modeline
(size-indication-mode t)                ;; show buffer size in modeline
(global-hl-line-mode 1)
(global-linum-mode 1)
(setq scroll-conservatively 1)
(setq scroll-margin 5)
(if (not (window-system))
    (menu-bar-mode -1))

;; better switching between buffers (this is VERY awesome)
(iswitchb-mode t)
;; fuzzy matching!
(ido-mode t)

;; Show matching parentheses for lisp editing
;; Highlight the entire parenthesized expression for easy visual understanding
(show-paren-mode t)
;; (setq show-paren-style 'expression)
(setq show-paren-style 'parentheses)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ------- FORMATTING/VARIABLES ------- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Don't prompt for "really want to exit?" when I still have processes running
(defadvice save-buffers-kill-emacs (around no-query-kill-emacs activate)
  "Prevent annoying \"Active processes exist\" query when you quit Emacs."
  (let ((process-list ())) ad-do-it))

;; Don't pollute my filesystem
(setq
 backup-by-copying t
 backup-directory-alist '(("." . ".swp_emacs"))
 delete-old-versions t)

;; Evil
(setq evil-default-cursor t)
;; from emacs wiki (http://emacswiki.org/emacs/Evil#toc8)
;; makes evil-emacs-state modes open up in motion state
(setq evil-motion-state-modes (append evil-emacs-state-modes evil-motion-state-modes))
(setq evil-emacs-state-modes nil)

;; woman
(setq woman-use-topic-at-point t)

;;;;;;;;;;;
;; Style ;;
;;;;;;;;;;;

;; sets indentation style
(setq c-default-style "k&r")
; always use spaces instead of tabs
(setq-default indent-tabs-mode nil)
; return is newline & indent
(define-key global-map (kbd "RET") 'newline-and-indent)
; no backup files -- luke's section may cover this...
(setq make-backup-files nil) ; prevents creation of backup files
(setq auto-save-default nil) ; disables auto save
;;(set-face-attribute 'default nil
;;                    :family "menlo" :height 130)

;; allows mouse in terminal
(unless window-system
  (require 'mouse)
  (xterm-mouse-mode t)
  (defun track-mouse(e))
  (setq mouse-sel-mode t))



;;;;;;;;;;;;;;;;;;;
;; Autogenerated ;;
;;;;;;;;;;;;;;;;;;;

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector ["black" "#d55e00" "#009e73" "#f8ec59" "#0072b2" "#cc79a7" "#56b4e9" "white"])
 '(c-basic-offset 4)
 '(column-number-mode t)
 '(custom-safe-themes (quote ("d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "1e7e097ec8cb1f8c3a912d7e1e0331caeed49fef6cff220be63bd2a6ba4cc365" "fc5fcb6f1f1c1bc01305694c59a1a861b008c534cae8d0e48e4d5e81ad718bc6" default)))
 '(fringe-mode (quote (nil . 0)) nil (fringe))
 '(indicate-buffer-boundaries (quote left))
 '(indicate-empty-lines t)
 '(inhibit-startup-screen nil)
 '(initial-buffer-choice nil)
 '(show-paren-mode t)
 '(size-indication-mode t)
 '(tool-bar-mode nil))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
