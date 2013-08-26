(require 'evil)
(require 'evil-leader)

;;
;; Basic Settings
;;

(evil-set-toggle-key "M-SPC")
(setq evil-default-cursor t)

;; makes evil-emacs-state modes open up in motion state
(setq evil-motion-state-modes (append evil-emacs-state-modes evil-motion-state-modes))
(setq evil-emacs-state-modes nil)

;;
;; Global Remaps
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

(define-key evil-normal-state-map (kbd "TAB") 'evil-jump-forward)
(define-key evil-visual-state-map (kbd "TAB") 'evil-jump-forward)

;;; ace-jump
(define-key evil-normal-state-map (kbd "SPC")   'evil-ace-jump-char-mode)
(define-key evil-normal-state-map (kbd "S-SPC") 'evil-ace-jump-word-mode)
(define-key evil-normal-state-map (kbd "C-SPC") 'evil-ace-jump-line-mode)
(define-key evil-operator-state-map (kbd "SPC")   'evil-ace-jump-char-mode)
(define-key evil-operator-state-map (kbd "S-SPC") 'evil-ace-jump-word-mode)
(define-key evil-operator-state-map (kbd "C-SPC") 'evil-ace-jump-line-mode)
(define-key evil-visual-state-map (kbd "SPC")   'evil-ace-jump-char-mode)
(define-key evil-visual-state-map (kbd "S-SPC") 'evil-ace-jump-word-mode)
(define-key evil-visual-state-map (kbd "C-SPC") 'evil-ace-jump-line-mode)

;;; Operators
(evil-define-operator evil-comment (beg end type register yank-handler)
  "Toggle comment on text from BEG to END with TYPE.
Save pre-commented text in REGISTER or in the kill-ring with YANK-HANDLER.
Copied from evil-delete implementation."
  (interactive "<R><x><y>")
  (unless register
    (let ((text (filter-buffer-substring beg end)))
      (unless (string-match-p "\n" text)
        ;; set the small delete register
        (evil-set-register ?- text))))
  (evil-yank beg end type register yank-handler)
  (cond
   ((eq type 'block)
    (evil-apply-on-block #'comment-or-uncomment-region beg end nil))
   ((and (eq type 'line)
         (= end (point-max))
         (or (= beg end)
             (/= (char-before end) ?\n))
         (/= beg (point-min))
         (=  (char-before beg) ?\n))
    (comment-or-uncomment-region (1- beg) end))
   (t
    (comment-or-uncomment-region beg end)))
  (when (and (evil-called-interactively-p)
             (eq type 'line))
    (evil-first-non-blank)))


;;
;; Advice
;;

(defadvice comment-dwim (after evil-comment-dwim activate)
  (when (and (not mark-active)
             evil-mode
             (not (eq evil-state 'emacs)))
    (evil-insert 1)))

(defadvice paredit-comment-dwim (after evil-comment-dwim activate)
  (if (and (not mark-active)
           evil-mode
           (not (eq evil-state 'emacs)))
      (evil-insert 1)))

(defadvice enable-paredit-mode (after enable-evil-paredit-mode activate)
  (if evil-mode
      (evil-paredit-mode)))

(defadvice magit-log-edit (after evil-magit-log-insert-mode activate)
  (if evil-local-mode
      (evil-insert 1)))


;; fix indentation in pony template mode
(defun pony-tpl-mode-fix-indent (function-symbol)
  (eval `(defadvice ,function-symbol (after pony-tpl-end-of-line activate)
           ,(format
             "Call `%s', then move to end of line"
             function-symbol)
           (move-end-of-line nil))))

;;
;; Mode-specific Settings
;;

;; Some modes clash too much with Evil
;;
;; Taken (and modified) shamelessly from cofi's evil config
;; ( https://github.com/cofi/dotfiles/tree/master/emacs.d/config )
(cl-loop for (mode . state) in '((inferior-emacs-lisp-mode  . emacs)
                                 (term-mode                 . emacs)
                                 (bc-menu-mode              . emacs)
                                 (magit-mode                . emacs)
                                 (magit-branch-manager-mode . emacs)
                                 (magit-cherry-mode         . emacs)
                                 (magit-commit-mode         . emacs)
                                 (magit-diff-mode           . emacs)
                                 (magit-key-mode            . emacs)
                                 (magit-log-edit-mode-map   . emacs)
                                 (magit-log-mode            . emacs)
                                 (magit-status-mode         . emacs))
         do (evil-set-initial-state mode state))

;; make magit evil
(evil-define-key 'emacs magit-mode-map
  "h" 'magit-toggle-diff-refine-hunk
  "j" 'magit-goto-next-section
  "k" 'magit-goto-previous-section
  "l" 'magit-key-mode-popup-logging
  "K" 'magit-discard-item
  "\C-h" 'evil-window-left
  "\C-j" 'evil-window-down
  "\C-k" 'evil-window-up
  "\C-l" 'evil-window-right)

(evil-define-key 'normal comint-mode-map "C-d" 'evil-scroll-down)
(evil-define-key 'insert comint-mode-map "C-j" 'comint-send-input)

;; Fix latex-mode C-j binding
(evil-define-key 'insert latex-mode-map (kbd "C-j") 'evil-ret)

;; use j/k as n/p in grep mode
(evil-define-key 'normal grep-mode-map
  "j" 'next-error-no-select
  "k" 'previous-error-no-select
  "q" 'quit-window)

;; Fix indentation in pony template mode
(add-hook 'pony-tpl-mode-hook
          (lambda ()
            (progn
              (pony-tpl-mode-fix-indent 'evil-ret)
              (pony-tpl-mode-fix-indent 'evil-open-below)
              (pony-tpl-mode-fix-indent 'evil-open-above)
              (pony-tpl-mode-fix-indent 'yas-expand))))


;;;;;;;;;;;;;;;;;;;;;
;; Plugin Settings ;;
;;;;;;;;;;;;;;;;;;;;;

;; evil-leader

(evil-leader/set-leader ",")
(setq evil-leader/in-all-states t)      ; in emacs state, use C-, as <leader>

(evil-leader/set-key
  "a" 'ag-project-regexp
  "x" ctl-x-map
  "f" 'find-file
  "b" 'ido-switch-buffer
  "l" 'load-file
  "c" 'evil-comment
  "w" 'save-buffer
  "t" '(lambda () (interactive) (ansi-term "bash"))
  "s" 'eshell
  "g" 'magit-status
  "q" 'evil-quit
  "o" 'occur
  "m" 'compile
  "d" 'kill-this-buffer
  "," 'evil-repeat-find-char-reverse
  "SPC" 'evil-ace-jump-char-mode)


(evil-leader/set-key-for-mode 'emacs-lisp-mode
  "er" 'eval-region
  "eb" 'eval-buffer
  "es" 'eval-last-sexp)

(evil-leader/set-key-for-mode 'lisp-interaction-mode
  "er" 'eval-region
  "eb" 'eval-buffer
  "es" 'eval-last-sexp)

(evil-leader/set-key-for-mode 'python-mode
  "eb" 'python-shell-send-buffer
  "ed" 'python-shell-send-defun
  "er" 'python-shell-send-region)

(evil-leader/set-key-for-mode 'scheme-mode
  "ed" 'geiser-eval-definition
  "er" 'geiser-eval-region
  "es" 'geiser-eval-last-sexp)



(provide 'evil-setup)
