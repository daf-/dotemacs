;;; Mac OS X settings

;;; mac keys
(setq mac-option-modifier 'alt)
(setq mac-command-modifier 'meta)
(setq ns-function-modifier 'hyper)

;; exec-path-from-shell
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))

(provide 'osx)
