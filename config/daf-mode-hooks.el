;; My mode hooks
;; Daniel Friedman, January 2013

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

;; from luke
(defun my-java-mode-hook ()
  (local-set-key (kbd "C-c C-c") 'compile)
  (local-set-key (kbd "C-c m") 'java-insert-main)
  (cond
   (if (fboundp 'c-subword-mode) (c-subword-mode t))
   (if (fboundp 'subword-mode) (subword-mode t))))
(add-hook 'java-mode-hook 'my-java-mode-hook)

;; make magit evil
(defun my-magit-mode-hook ()
  (define-key evil-motion-state-map (kbd "TAB") 'magit-toggle-section)
  (define-key evil-motion-state-map (kbd "SPC") 'magit-goto-next-section)
  (define-key evil-motion-state-map (kbd "DEL") 'magit-goto-previous-section))
(add-hook 'magit-mode-hook 'my-magit-mode-hook)

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

(provide 'daf-mode-hooks)
