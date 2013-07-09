;; Settings for evil-leader
;; Daniel Friedman, January 2013

(require 'evil-leader)

(evil-leader/set-leader ",")
;; (define-key evil-motion-state-map "," evil-leader/map)  ;; allows leader in motion state, too
;; (evil-leader/set-key
;;   "x" ctl-x-map
;;   "f" 'find-file
;;   "b" 'ido-switch-buffer
;;   "l" 'load-file
;;   "c" 'comment-or-uncomment-line-or-region
;;   "w" 'save-buffer
;;   "t" 'ansi-term
;;   "s" 'eshell
;;   "g" 'magit-status
;;   "q" 'evil-quit
;;   "o" 'occur
;;   "m" 'compile
;;   "d" 'kill-this-buffer
;;   "," 'evil-repeat-find-char-reverse)
(evil-leader/set-key "x" ctl-x-map)
(evil-leader/set-key "f" 'find-file)
(evil-leader/set-key "b" 'ido-switch-buffer)
(evil-leader/set-key "l" 'load-file)
(evil-leader/set-key "c" 'comment-or-uncomment-line-or-region)
(evil-leader/set-key "w" 'save-buffer)
(evil-leader/set-key "t" 'ansi-term)
(evil-leader/set-key "s" 'eshell)
(evil-leader/set-key "g" 'magit-status)
(evil-leader/set-key "q" 'evil-quit)
(evil-leader/set-key "o" 'occur)
(evil-leader/set-key "m" 'compile)
(evil-leader/set-key "d" 'kill-this-buffer)
(evil-leader/set-key "," 'evil-repeat-find-char-reverse)
(evil-leader/set-key "SPC" 'ace-jump-mode)

;; Mode-specific 'leader' macros (not possible with evil-leader
;; <Leader>-e corresponds to evaluate/run in interpreter
;; (evil-define-key 'visual scheme-mode-map ",er" 'geiser-eval-region)
;; (evil-define-key 'visual emacs-lisp-mode-map ",er" 'eval-region)
;; (evil-define-key 'visual lisp-interaction-mode-map ",er" 'eval-region)
;; (evil-define-key 'visual python-mode-map ",er" 'python-send-region)
;; (evil-define-key 'normal scheme-mode-map ",ed" 'geiser-eval-definition)
;; (evil-define-key 'normal python-mode-map ",ed" 'python-send-defun)
;; (evil-define-key 'normal scheme-mode-map ",es" 'geiser-eval-last-sexp)
;; (evil-define-key 'normal emacs-lisp-mode-map ",es" 'eval-last-sexp)
;; (evil-define-key 'normal lisp-interaction-mode-map ",es" 'eval-last-sexp)
;; (evil-define-key 'normal python-mode-map ",eb" 'python-send-buffer)

(evil-leader/set-key-for-mode 'scheme-mode "er" 'geiser-eval-region)
(evil-leader/set-key-for-mode 'emacs-lisp-mode "er" 'eval-region)
(evil-leader/set-key-for-mode 'lisp-interaction-mode "er" 'eval-region)
(evil-leader/set-key-for-mode 'python-mode "er" 'python-send-region)
(evil-leader/set-key-for-mode 'scheme-mode "ed" 'geiser-eval-definition)
(evil-leader/set-key-for-mode 'python-mode "ed" 'python-send-defun)
(evil-leader/set-key-for-mode 'scheme-mode "es" 'geiser-eval-last-sexp)
(evil-leader/set-key-for-mode 'emacs-lisp-mode "es" 'eval-last-sexp)
(evil-leader/set-key-for-mode 'lisp-interaction-mode "es" 'eval-last-sexp)
(evil-leader/set-key-for-mode 'python-mode "eb" 'python-send-buffer)


(provide 'daf-evil-leader)
