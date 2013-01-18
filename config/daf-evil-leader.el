;; Settings for evil-leader
;; Daniel Friedman, January 2013

(require 'evil-leader)

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
(evil-leader/set-key "o" 'occur)

(provide 'daf-evil-leader)
