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

(defun my-lispy-mode-hook ()
  (rainbow-delimiters-mode t)
  (enable-paredit-mode))
(add-hook 'emacs-lisp-mode-hook 'my-lispy-mode-hook)
(add-hook 'lisp-mode-hook 'my-lispy-mode-hook)
(add-hook 'lisp-interaction-mode-hook 'my-lispy-mode-hook)
(add-hook 'scheme-mode-hook 'my-lispy-mode-hook)
(add-hook 'clojure-mode-hook 'my-lispy-mode-hook)

;; Have comments auto-wrap
(defun c-like-mode-hook ()
  (setq c-basic-offset 4)
  (setq evil-shift-width 4)
  (auto-fill-mode))
(add-hook 'c-mode-hook 'c-like-mode-hook)
(add-hook 'c++-mode-hook 'c-like-mode-hook)

;; Fix js indentation
(add-hook 'js-mode-hook
          (lambda ()
            (setq js-indent-level 2)
            (setq evil-shift-width 2)
            (electric-indent-mode)))
(add-hook 'js2-mode-hook
          (lambda ()
            (setq js2-basic-offset 2)
            (electric-pair-mode)))

(add-hook 'python-mode-hook (lambda ()
                              (setq python-indent-offset 4)
                              (auto-fill-mode)))

(add-hook 'java-mode-hook
          (lambda ()
            (unless (or (file-exists-p "makefile")
                        (file-exists-p "Makefile"))
              (set (make-local-variable 'compile-command)
                   (concat "javac "
                           ;; (buffer-file-name))))))
                           (buffer-name))))
            (setq c-basic-offset 4)))

(add-hook 'latex-mode-hook
          (lambda ()
            (turn-on-auto-fill)
            (set-fill-column 90)))

(add-hook 'html-mode-hook
          (lambda ()
            (set (make-local-variable 'sgml-basic-offset) 4)))

;; don't highlight line in shell
(defun my-term-mode-hook ()
  (setq global-hl-line-mode nil))
(add-hook 'term-mode-hook 'my-term-mode-hook)
(add-hook 'term-mode-hook 'my-term-mode-hook)
(add-hook 'eshell-mode-hook 'my-term-mode-hook)

(add-hook 'actionscript-mode-hook
          (lambda ()
            (auto-complete-mode)))

(provide 'daf-mode-hooks)
