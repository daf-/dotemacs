;; My mode hooks
;; Daniel Friedman, January 2013
;; TODO: use evil-local-set-key

;; open header files in c++ mode
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))

;; from luke
(defun my-text-mode-hook ()
  (turn-on-auto-fill)	;; allows text-wrapping
  (set-fill-column 80)	;; how many characters before we wrap?
  (cond
   ((eq major-mode "text-mode")	;; Allow comments in text-mode
    (setq comment-start "#"))))
(add-hook 'text-mode-hook 'my-text-mode-hook)
(setq default-major-mode 'text-mode) ;; default is fundamental-mode

(defun my-lispy-mode-hook ()
  (make-local-variable 'evil-move-cursor-back)
  (setq evil-move-cursor-back nil)
  (paredit-mode)
  (evil-paredit-mode))
  ;; (make-local-variable 'evil-highlight-closing-paren-at-point-states)
  ;; (cons 'normal evil-highlight-closing-paren-at-point-states))
(add-hook 'emacs-lisp-mode-hook 'my-lispy-mode-hook t)
(add-hook 'scheme-mode-hook 'my-lispy-mode-hook t)
(add-hook 'lisp-mode-hook 'my-lispy-mode-hook t)
(add-hook 'lisp-interaction-mode-hook 'my-lispy-mode-hook)

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
(defun my-magit-mode-hook ()
  (define-key evil-motion-state-local-map (kbd "TAB") 'magit-toggle-section)
  (define-key evil-motion-state-local-map (kbd "SPC") 'magit-goto-next-section)
  (define-key evil-motion-state-local-map (kbd "DEL") 'magit-goto-previous-section))
(add-hook 'magit-mode-hook 'my-magit-mode-hook)

;; make RET behave in occur-mode
(defun my-occur-mode-hook ()
  (define-key evil-motion-state-local-map (kbd "RET") 'occur-mode-goto-occurrence))
(add-hook 'occur-mode-hook 'my-occur-mode-hook)

;; don't highlight line in shell
(add-hook 'term-mode-hook
          (lambda ()
            (setq global-hl-line-mode nil)))

(provide 'daf-mode-hooks)
