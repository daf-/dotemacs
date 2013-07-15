;; Fix indentation in pony template mode
(add-hook 'pony-tpl-mode-hook
          (lambda ()
            (progn
              (pony-tpl-mode-fix-indent 'evil-ret)
              (pony-tpl-mode-fix-indent 'evil-open-below)
              (pony-tpl-mode-fix-indent 'evil-open-above)
              (pony-tpl-mode-fix-indent 'yas-expand))))

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
