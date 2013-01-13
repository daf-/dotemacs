;; Some simple global Settings
;; Daniel Friedman, Fall 2013


;; colors -- only for emacs 24

(when (>= emacs-major-version 24)
  (add-to-list 'custom-theme-load-path "~/.emacs.d/elpa/color-theme-solarized-20121209.1204"))
(if (and (>= emacs-major-version 24) (window-system))
    (load-theme 'deeper-blue t)
  (if (and (>= emacs-major-version 24) (not (window-system)))
      (load-theme 'solarized-dark t)))

;; minor modes

(transient-mark-mode t)    ;; show regions as highlighted
(column-number-mode t)     ;; shows column number in modeline
(size-indication-mode t)   ;; show buffer size in modeline
(global-hl-line-mode 1)
(global-linum-mode 1)
(setq scroll-conservatively 1)
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
(setq show-paren-style 'parentheses)

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

;; Evil
(setq evil-default-cursor t)
;; from emacs wiki (http://emacswiki.org/emacs/Evil#toc8)
;; makes evil-emacs-state modes open up in motion state
(setq evil-motion-state-modes (append evil-emacs-state-modes evil-motion-state-modes))
(setq evil-emacs-state-modes nil)

;; woman
(setq woman-use-topic-at-point t)


;; style

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

(provide 'daf-global-settings)
