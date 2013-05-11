;; Settings for evil-mode
;; Daniel Friedman, January 2013

(require 'evil)

;;
;; Basic Settings
;;

;; Don't override cursor color
(setq evil-default-cursor t)

;; M-SPC toggles evil/emacs state
(evil-set-toggle-key "M-SPC")

;; Remove useless vim bindings
(define-key evil-insert-state-map (kbd "C-n") nil)
(define-key evil-insert-state-map (kbd "C-p") nil)
(define-key evil-insert-state-map (kbd "C-k") nil)
(define-key evil-insert-state-map (kbd "C-r") nil)
(define-key evil-motion-state-map (kbd "RET") nil) ;; This somehow messes up RET in normal state, so...:
(define-key evil-normal-state-map (kbd "RET") 'evil-ret)
(define-key evil-motion-state-map (kbd "TAB") nil)
(define-key evil-motion-state-map (kbd "SPC") nil)

;; makes evil-emacs-state modes open up in motion state
(setq evil-motion-state-modes (append evil-emacs-state-modes evil-motion-state-modes))
(setq evil-emacs-state-modes nil)

;; esc quits -- modified from http://stackoverflow.com/questions/8483182/emacs-evil-mode-best-practice
(define-key evil-normal-state-map [escape] 'keyboard-quit)
(define-key evil-visual-state-map [escape] 'keyboard-quit)
(define-key evil-motion-state-map [escape] 'keyboard-quit)
(define-key minibuffer-local-map [escape] 'abort-recursive-edit)
(define-key minibuffer-local-ns-map [escape] 'abort-recursive-edit)
(define-key minibuffer-local-completion-map [escape] 'abort-recursive-edit)
(define-key minibuffer-local-must-match-map [escape] 'abort-recursive-edit)
(define-key minibuffer-local-isearch-map [escape] 'abort-recursive-edit)




;;
;; Remaps
;;

;; Maps "kj" to escape
;; from http://zuttobenkyou.wordpress.com/2011/02/15/some-thoughts-on-emacs-and-vim/
(evil-define-key 'insert global-map "k" #'cofi/maybe-exit)
(evil-define-key 'insert global-map "k" #'cofi/maybe-exit)
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
;; (define-key evil-normal-state-map (kbd "C-h") #'evil-window-left)
;; (define-key evil-normal-state-map (kbd "C-j") #'evil-window-down)
;; (define-key evil-normal-state-map (kbd "C-k") #'evil-window-up)
;; (define-key evil-normal-state-map (kbd "C-l") #'evil-window-right)
;; (define-key evil-motion-state-map (kbd "C-h") #'evil-window-left)
;; (define-key evil-motion-state-map (kbd "C-j") #'evil-window-down)
;; (define-key evil-motion-state-map (kbd "C-k") #'evil-window-up)
;; (define-key evil-motion-state-map (kbd "C-l") #'evil-window-right)

;; Navigation and Editing
;; (setq evil-want-C-u-scroll t) ;; why isn't this working?
(define-key evil-normal-state-map (kbd "C-u") 'evil-scroll-up)
(define-key evil-motion-state-map (kbd "C-u") 'evil-scroll-up)
(define-key evil-insert-state-map (kbd "C-e") 'move-end-of-line)
(define-key evil-visual-state-map (kbd "TAB") #'evil-indent)

(define-key evil-normal-state-map (kbd "H") #'evil-first-non-blank)
(define-key evil-normal-state-map (kbd "L") #'evil-end-of-line)
(define-key evil-visual-state-map (kbd "H") #'evil-first-non-blank)
(define-key evil-visual-state-map (kbd "L") #'evil-end-of-line)
(define-key evil-motion-state-map (kbd "H") #'evil-first-non-blank)
(define-key evil-motion-state-map (kbd "L") #'evil-end-of-line)

;; Fix latex-mode C-j binding
(evil-define-key 'insert latex-mode-map (kbd "C-j") #'evil-ret)

(provide 'daf-evil)
