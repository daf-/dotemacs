(require 'evil)

;;
;; Basic Settings
;;

(evil-set-toggle-key "M-SPC")
(setq evil-default-cursor t)

;; makes evil-emacs-state modes open up in motion state
(setq evil-motion-state-modes (append evil-emacs-state-modes evil-motion-state-modes))
(setq evil-emacs-state-modes nil)

;;
;; Remaps
;;

(define-key evil-normal-state-map (kbd "M-H") #'help)
(define-key evil-normal-state-map (kbd "Y") (kbd "y$"))

;;; Remove useless vim bindings
(define-key evil-insert-state-map (kbd "C-k") nil)
(define-key evil-motion-state-map (kbd "RET") nil) ;; This somehow messes up RET in normal state, so...:
(define-key evil-normal-state-map (kbd "RET") 'evil-ret)
(define-key evil-motion-state-map (kbd "TAB") nil)
(define-key evil-motion-state-map (kbd "SPC") nil)

;; Maps "kj" to escape
;; from http://zuttobenkyou.wordpress.com/2011/02/15/some-thoughts-on-emacs-and-vim/
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

;; window mappings
(define-key evil-normal-state-map " " #'evil-toggle-fold)
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
(define-key evil-motion-state-map (kbd "C-u") 'evil-scroll-up)
(define-key evil-insert-state-map (kbd "C-e") 'move-end-of-line)
(define-key evil-visual-state-map (kbd "TAB") #'evil-indent)

(define-key evil-normal-state-map (kbd "H") 'evil-first-non-blank)
(define-key evil-normal-state-map (kbd "L") 'evil-end-of-line)
(define-key evil-visual-state-map (kbd "H") 'evil-first-non-blank)
(define-key evil-visual-state-map (kbd "L") 'evil-end-of-line)
(define-key evil-motion-state-map (kbd "H") 'evil-first-non-blank)
(define-key evil-motion-state-map (kbd "L") 'evil-end-of-line)

(define-key evil-normal-state-map (kbd "C-i") 'evil-jump-forward)
(define-key evil-visual-state-map (kbd "C-i") 'evil-jump-forward)

;;
;; Mode maps
;;

(evil-define-key 'normal comint-mode-map "C-d" 'evil-scroll-down)
(evil-define-key 'insert comint-mode-map "C-j" 'comint-send-input)

;; Fix latex-mode C-j binding
(evil-define-key 'insert latex-mode-map (kbd "C-j") 'evil-ret)

;; make magit evil
(evil-define-key 'motion magit-mode-map
  "j" 'magit-goto-next-section
  "k" 'magit-goto-previous-section)

;;
;; Advice
;;

(defadvice comment-dwim (after evil-comment-dwim activate)
  (when (and (not mark-active)
             evil-mode
             (not (eq evil-state 'emacs)))
    (evil-insert 1)))
(defadvice paredit-comment-dwim (after evil-comment-dwim activate)
  (if (not mark-active)
      (evil-insert 1)))

(defadvice enable-paredit-mode (after enable-evil-paredit-mode activate)
  (if evil-mode
      (evil-paredit-mode)))

(defadvice magit-log-edit (after evil-magit-log-insert-mode activate)
  (if (evil-mode)
      (evil-insert 1)))

;; fix indentation in pony template mode
(defun pony-tpl-mode-fix-indent (function-symbol)
  (eval `(defadvice ,function-symbol (after pony-tpl-end-of-line activate)
           ,(format
             "Call `%s', then move to end of line"
             function-symbol)
           (move-end-of-line nil))))



(provide 'daf-evil)
