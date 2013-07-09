;; Have C-d scroll down in comint buffers
(add-hook 'comint-mode-hook
          (lambda ()
            (evil-local-set-key 'normal (kbd "C-d") 'evil-scroll-down)))

(defun evil-lispy-mode-hook ()
  (evil-paredit-mode))
(add-hook 'emacs-lisp-mode-hook 'evil-lispy-mode-hook)
(add-hook 'lisp-mode-hook 'evil-lispy-mode-hook)
(add-hook 'lisp-interaction-mode-hook 'evil-lispy-mode-hook)
(add-hook 'scheme-mode-hook 'evil-lispy-mode-hook)
(add-hook 'clojure-mode-hook 'evil-lispy-mode-hook)

;; fix indentation in pony template mode
(defun pony-tpl-mode-fix-indent (function-symbol)
  (eval `(defadvice ,function-symbol (after pony-tpl-end-of-line activate)
           ,(format
             "Call `%s', then move to end of line"
             function-symbol)
           (move-end-of-line nil))))
(add-hook 'pony-tpl-mode-hook
          (lambda ()
            (progn
              (pony-tpl-mode-fix-indent 'evil-ret)
              (pony-tpl-mode-fix-indent 'evil-open-below)
              (pony-tpl-mode-fix-indent 'evil-open-above)
              (pony-tpl-mode-fix-indent 'yas-expand))))

;; make magit evil
(add-hook 'magit-mode-hook
          (lambda ()
            (define-key evil-motion-state-local-map (kbd "j") 'magit-goto-next-section)
            (define-key evil-motion-state-local-map (kbd "k") 'magit-goto-previous-section)))

;; make RET behave in occur-mode
(add-hook 'occur-mode-hook
          (lambda ()
            (define-key evil-motion-state-local-map (kbd "RET") 'occur-mode-goto-occurrence)))
;; make C-j play nice in comint-mode
(add-hook 'comint-mode-hook
          (lambda ()
            (define-key evil-insert-state-local-map (kbd "C-j") 'comint-send-input)))

;; use j/k as n/p in grep mode
(add-hook 'grep-mode-hook
          (lambda ()
            (define-key evil-normal-state-local-map (kbd "j") 'next-error-no-select)
            (define-key evil-normal-state-local-map (kbd "k") 'previous-error-no-select)
            (define-key evil-normal-state-local-map (kbd "q") 'quit-window)))

(add-hook 'geiser-repl-mode-hook
          (lambda ()
            (evil-define-key normal 'geiser-repl-mode-map (kbd "C-h") 'evil-window-left)
            (evil-define-key normal 'geiser-repl-mode-map (kbd "C-j") 'evil-window-down)))

(provide 'daf-evil-mode-hooks)
