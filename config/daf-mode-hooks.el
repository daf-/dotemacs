;; open header files in c++ mode
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))

;; from luke
(defun my-text-mode-hook ()
  (turn-on-auto-fill)
  (set-fill-column 80)
  (cond
   ((eq major-mode "text-mode")
    (setq comment-start "#"))))
(add-hook 'text-mode-hook 'my-text-mode-hook)
(setq major-mode 'text-mode) ;; default is fundamental-mode

;; Have comments auto-wrap
(defun code-comments-mode-hook ()
  (auto-fill-mode))
(add-hook 'c-mode-hook 'code-comments-mode-hook)
(add-hook 'c++-mode-hook 'code-comments-mode-hook)
(add-hook 'java-mode-hook 'code-comments-mode-hook)
(add-hook 'python-mode-hook 'code-comments-mode-hook)

(add-hook 'latex-mode-hook
          (lambda ()
            (turn-on-auto-fill)
            (set-fill-column 90)))

;; don't highlight line in shell
(defun my-term-mode-hook ()
  (setq global-hl-line-mode nil))
(add-hook 'term-mode-hook 'my-term-mode-hook)
(add-hook 'term-mode-hook 'my-term-mode-hook)
(add-hook 'eshell-mode-hook 'my-term-mode-hook)


(provide 'daf-mode-hooks)
