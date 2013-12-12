;;; Mac OS X settings

;; Set up PATH
(setenv "PATH"
        (concat
         "~/bin" ":"
         "/usr/local/bin" ":"
         (getenv "PATH")))

(setq exec-path '("/usr/local/share/python" "/usr/local/bin" "/usr/bin" "/bin" "/usr/sbin" "/sbin" "/usr/local/Cellar/emacs/24.3/libexec/emacs/24.3/x86_64-apple-darwin12.3.0"))

;;; mac keys
(setq mac-option-modifier 'super)
(setq mac-command-modifier 'meta)
(setq ns-function-modifier 'hyper)

;; treat super as command key
(setq mac-emulate-three-button-mouse t) ; use option as middle click, command as right.
(global-set-key (kbd "s-s") 'save-buffer)
(global-set-key (kbd "s-S") 'ido-write-file)
(global-set-key (kbd "s-z") 'undo-tree-undo)
(global-set-key (kbd "s-Z") 'undo-tree-redo)
(global-set-key (kbd "s-q") 'save-buffers-kill-terminal)
(global-set-key (kbd "s-o") 'ido-find-file)
(global-set-key (kbd "s-b") 'ido-switch-buffer)
(global-set-key (kbd "s-k") 'kill-this-buffer)
(global-set-key (kbd "s-K") 'kill-buffer-and-window)

(provide 'osx)
