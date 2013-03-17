;; open header files in c++ mode
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))

(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)
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
(defun c-like-mode-hook ()
  (setq c-basic-offset 2)
  (setq evil-shift-width 2)
  (auto-fill-mode))
(add-hook 'c-mode-hook 'c-like-mode-hook)
(add-hook 'c++-mode-hook 'c-like-mode-hook)
;; (add-hook 'java-mode-hook 'c-like-mode-hook)

(add-hook 'python-mode-hook (auto-fill-mode))

(add-hook 'java-mode-hook
          (lambda ()
            (unless (or (file-exists-p "makefile")
                        (file-exists-p "Makefile"))
              (set (make-local-variable 'compile-command)
                   (concat "javac "
                           ;; (buffer-file-name))))))
                           (buffer-name))))))

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
