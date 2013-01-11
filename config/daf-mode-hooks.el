;; My mode hooks
;; Daniel Friedman, January 2013

;; TODO: add mode-hooks for evil mode; e.g. optargs for shell-script-mode

(require 'python-mode)

;; from luke
(defun my-text-mode-hook()
  (turn-on-auto-fill)	;; allows text-wrapping
  (set-fill-column 80)	;; how many characters before we wrap?
  (cond
   ((eq major-mode "text-mode")	;; Allow comments in text-mode
    (setq comment-start "#"))))
(add-hook 'text-mode-hook 'my-text-mode-hook)
(setq default-major-mode 'text-mode) ;; default is fundamental-mode

;; from luke
;; TODO: evillify
(defun my-java-mode-hook()
  (local-set-key (kbd "C-c C-c") 'compile)
  (local-set-key (kbd "C-c m") 'java-insert-main)
  (cond
   (if (fboundp 'c-subword-mode) (c-subword-mode t))
   (if (fboundp 'subword-mode) (subword-mode t))))
(add-hook 'java-mode-hook 'my-java-mode-hook)

;; TODO: why won't setting evil-lookup-func work?
(defun my-python-mode-hook()
  (setq evil-lookup-func 'py-documentation))
(add-hook 'python-mode-hook 'my-python-mode-hook)

;; make magit evil
(defun my-evil-magit-mode-hook()
  (define-key evil-motion-state-map (kbd "SPC") 'magit-toggle-section))
(add-hook 'magit-mode-hook 'my-evil-magit-mode-hook)

(provide 'daf-mode-hooks)
