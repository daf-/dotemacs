;; Heavily modified .emacs file originally from Luke Lovett

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ------- PATH AND REQUIRES ------- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; run emacs in server-mode, so that new sessions start quickly
;; (server-start)

;; EVIL
;;(add-to-list 'load-path "~/.emacs.d/plugin/evil")
;;(require 'evil)
;;(evil-mode 1)

;; evil-leader
(add-to-list 'load-path "~/.emacs.d/plugin/evil-leader")
(require 'evil-leader)

;; evil-surround
(add-to-list 'load-path "~/.emacs.d/plugin/evil-surround")
(require 'surround)
(global-surround-mode 1)

;; popup
(add-to-list 'load-path "~/.emacs.d/plugin/popup-el")
(require 'popup)

;; auto-complete
(add-to-list 'load-path "~/.emacs.d/plugin/ac-install-files/")
(require 'auto-complete-config)
(ac-config-default)

;; Solarized
(when (>= emacs-major-version 24)
  (add-to-list 'custom-theme-load-path "~/.emacs.d/plugin/emacs-color-theme-solarized")
  (load-theme 'solarized-dark t))

;; golden-ratio
(add-to-list 'load-path "~/.emacs.d/plugin/golden-ratio")
(require 'golden-ratio)
(golden-ratio-enable)

;; YASnippet
(add-to-list 'load-path "~/.emacs.d/plugin/yasnippet")
(require 'yasnippet)
(yas-global-mode 1)

; for SuperCollider - from http://sam.aaron.name/2010/02/09/hooking-supercollider-up-to-emacs-on-os-x.html
;(setq path "/Applications/Supercollider.app/Contents/Resources:PATH")
;; (setq path "/Applications/Supercollider.app/Contents/Resources:$PATH")
;; (setenv "PATH" path)
;; (push "/Applications/SuperCollider/SuperCollider.app/Contents/Resources" exec-path)
;; (add-to-list 'load-path "~/.emacs.d/vendor/supercollider/el")
;; (require 'sclang)

;; SuperCollider -- finish this
;(add-to-list 'load-path "/Applications/SuperCollider/SuperCollider.app/Contents/Resources/sclang")
;(require 'sclang)


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
(yas/advise-indent-function 'c-indent-line-or-region)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ------- KEY MAPPINGS ------- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; EVIL ;;

;; esc quits -- from http://stackoverflow.com/questions/8483182/emacs-evil-mode-best-practice
(define-key evil-normal-state-map [escape] 'keyboard-quit)
(define-key evil-visual-state-map [escape] 'keyboard-quit)
(define-key minibuffer-local-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-ns-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-completion-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-must-match-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-isearch-map [escape] 'minibuffer-keyboard-quit)

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
(define-key evil-normal-state-map (kbd "C-u") #'evil-scroll-up)
(define-key evil-insert-state-map (kbd "C-e") #'move-end-of-line)

;; (define-key evil-insert-state-map (kbd "C-z") #'move-end-of-line)
;; (define-key evil-normal-state-map (kbd "C-z") #'move-end-of-line)
;; (define-key evil-visual-state-map (kbd "C-z") #'move-end-of-line)
;; (define-key evil-motion-state-map (kbd "C-z") #'move-end-of-line)

;; Leader maps
(evil-leader/set-leader ",")
(evil-leader/set-key "x" ctl-x-map)
(evil-leader/set-key "f" 'find-file)
(evil-leader/set-key "l" 'load-file)
(evil-leader/set-key "c" 'comment-or-uncomment-region)
(evil-leader/set-key "s" 'eshell)
;; (evil-leader/set-key "e" evil-toggle-key)

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

;; C-c <letter> is guaranteed never to be bound in the standard
;; distribution of Emacs.  It is reserved for users' personal
;; keybindings, and we, being reasonable people, will abide by this
;; convention, so that we don't override anything important:
;; Prepare your own personal keymap, and then put it on C-c:
(define-prefix-command 'personal-map)
(global-set-key "\C-c" personal-map)

(define-key personal-map "s" 'run-scheme)
(define-key personal-map "p" 'run-python)
(define-key personal-map "j" 'eshell)
(define-key personal-map "k" 'term)
(define-key personal-map "f" 'find-file-other-window)
(define-key personal-map "." 'bookmark-set)
(define-key personal-map "/" 'bookmark-jump)




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ------- MODE-HOOKS ------- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; A `hook' is something that runs when certain modes start up. For example,
;; text-mode-hook will run when you enter text-mode. Mode hooks are the place
;; for setting environmental variables and doing local keybindings.
(defun my-text-mode-hook()
  (turn-on-auto-fill)	;; allows text-wrapping
  (set-fill-column 80)	;; how many characters before we wrap?
  (cond
   ((eq major-mode "text-mode")	;; Allow comments in text-mode
    (setq comment-start "#")))		;; # is comment character
  )
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
    
(defun my-java-mode-hook()
  (local-set-key (kbd "C-c C-c") 'compile)
  (local-set-key (kbd "C-c m") 'java-insert-main)
  (cond
   (if (fboundp 'c-subword-mode) (c-subword-mode t))
   (if (fboundp 'subword-mode) (subword-mode t))))

(add-hook 'java-mode-hook 'my-java-mode-hook)

(defun my-python-mode-hook()
   (if (fboundp 'c-subword-mode) (c-subword-mode t))
   (if (fboundp 'subword-mode) (subword-mode t))
   ;; Make sure we get good comment syntax highlighting
   (set-face-foreground font-lock-comment-face 'red))
(add-hook 'python-mode-hook 'my-python-mode-hook)

;; TODO: add mode-hooks for evil mode; e.g. optargs for shell-script-mode



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ------ MINOR MODES ------ ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(transient-mark-mode t)	                ;; show regions as highlighted
(column-number-mode t)	                ;; shows column number in modeline
(size-indication-mode t)                ;; show buffer size in modeline
(linum-mode (< emacs-major-version 24)) ;; linum mode is broken in Emacs 24
(if (>= emacs-major-version 24)         ;; inserts matching brackets
    (electric-pair-mode t))

;; for some reason, scroll-conservatively really messes with
;; graphical emacs...
(unless (or (equal window-system 'x) (equal window-system 'ns))
  (setq scroll-conservatively 1)	;; Scroll as you navigate
  (setq scroll-margin 20)		;; Scroll when we are 20 lines from the top or bottom of the window
   (menu-bar-mode -1))			;; The menu bar is utterly useless in text mode.
(if (or (equal window-system 'x) (equal window-system 'ns))
    (tool-bar-mode 0))                ;; tool-bar sucks

;; better switching between buffers (this is VERY awesome)
(iswitchb-mode t)
;; fuzzy matching!
(ido-mode t)

;; Show matching parentheses for lisp editing
;; Highlight the entire parenthesized expression for easy visual understanding
(show-paren-mode t)
(setq show-paren-style 'expression)




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

;; allows mouse in terminal
(require 'mouse)
(xterm-mouse-mode t)
(defun track-mouse(e))
(setq mouse-sel-mode t)

;; turn on hl-line-mode
(hl-line-mode t)



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
 '(custom-safe-themes (quote ("1e7e097ec8cb1f8c3a912d7e1e0331caeed49fef6cff220be63bd2a6ba4cc365" "fc5fcb6f1f1c1bc01305694c59a1a861b008c534cae8d0e48e4d5e81ad718bc6" default)))
 '(inhibit-startup-screen nil)
 '(initial-buffer-choice nil)
 '(sclang-auto-scroll-post-buffer t)
 '(sclang-eval-line-forward nil)
 '(sclang-help-path (quote ("/Applications/SuperCollider/SuperCollider.app/Contents/Resources/Help")))
 '(sclang-runtime-directory "~/Applications/SuperCollider/SuperCollider.app/Contents/Resources/"))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
