;; Settings for evil-leader
;; Daniel Friedman, January 2013

(require 'evil-leader)

(evil-leader/set-leader ",")

(evil-leader/set-key
  "x" ctl-x-map
  "f" 'find-file
  "b" 'ido-switch-buffer
  "l" 'load-file
  "c" 'comment-or-uncomment-line-or-region
  "w" 'save-buffer
  "t" 'ansi-term
  "s" 'eshell
  "g" 'magit-status
  "q" 'evil-quit
  "o" 'occur
  "m" 'compile
  "k" 'ido-kill-buffer
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




(provide 'daf-evil-leader)
