(setq vc-handled-backends nil)
(setq show-paren-delay 0)

(when (< emacs-major-version 24)
  (add-to-list 'load-path "~/.emacs.d/"))
(require 'package)
(setq package-archives '(("marmalade" . "http://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")))

(defvar daf-packages'(
                      ace-jump-mode
		      deep-thought-theme
		      evil
		      evil-leader
		      geiser
		      magit
		      markdown-mode
		      pony-mode
		      popup
		      rainbow-delimiters
		      undo-tree
		      zenburn-theme
                      actionscript-mode
                      auto-complete
                      color-theme
                      evil-paredit
                      flycheck
                      js2-mode
                      paredit
                      solarized-theme
                      ujelly-theme
                      ;; yasnippet
                      ))

(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

(dolist (p daf-packages)
  (when (not (package-installed-p p))
    (package-install p)))


;; initialize plugins
(defun evil-all ()
  (require 'evil-paredit)
  (global-evil-leader-mode)
  (evil-mode 1))

(defun load-plugins ()
  (evil-all)
  (global-undo-tree-mode)
  (global-flycheck-mode)
  (yas-global-mode)
  (add-to-list 'auto-mode-alist '("\\.as$" . actionscript-mode))
  (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode)))

;; initialize settings
(defun load-settings ()
  (add-to-list 'load-path "~/.emacs.d/config")
  (require 'daf-mode-hooks)
  (require 'daf-global-settings)
  (require 'funcs)
  (require 'daf-evil)
  (require 'daf-evil-leader)
  (require 'daf-evil-mode-hooks))


;; load settings -- wait for packages to load first (emacswiki.org/emacs/ELPA)
(add-hook 'after-init-hook
	  (lambda ()
            (load-settings)
            (load-plugins)))

;; Colors, font
(if (< emacs-major-version 24)
    (add-to-list 'load-path "~/.emacs.d/themes")
  (add-to-list 'custom-theme-load-path "~/.emacs.d/themes"))
(if (display-graphic-p)
    (load-theme 'underwater t)
  (load-theme 'zenburn t))
(if (eq system-type 'darwin)
    (set-frame-font "-apple-Monaco-medium-normal-normal-*-*-*-*-*-m-0-iso10646-1"))



;;;;;;;;;;;;;;;;;;;
;; Autogenerated ;;
;;;;;;;;;;;;;;;;;;;

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector ["#3F3F3F" "#CC9393" "#7F9F7F" "#F0DFAF" "#8CD0D3" "#DC8CC3" "#93E0E3" "#DCDCCC"])
 '(custom-safe-themes (quote ("828d47ac5f3c9c5c06341b28a1d0ebd9f0a0a9c4074504626148f36c438321c2" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "3d6b08cd1b1def3cc0bc6a3909f67475e5612dba9fa98f8b842433d827af5d30" "246a51f19b632c27d7071877ea99805d4f8131b0ff7acb8a607d4fd1c101e163" default)))
 '(exec-path (quote ("/usr/local/share/python" "/usr/local/bin" "/usr/bin" "/bin" "/usr/sbin" "/sbin" "/usr/local/Cellar/emacs/24.3/libexec/emacs/24.3/x86_64-apple-darwin12.3.0")))
 '(fci-rule-color "#383838")
 '(fringe-mode 6 nil (fringe))
 '(highlight-changes-colors (quote ("#d33682" "#6c71c4")))
 '(highlight-tail-colors (quote (("#073642" . 0) ("#546E00" . 20) ("#00736F" . 30) ("#00629D" . 50) ("#7B6000" . 60) ("#8B2C02" . 70) ("#93115C" . 85) ("#073642" . 100))))
 '(linum-format " %7d " t)
 '(main-line-color1 "#191919")
 '(main-line-color2 "#111111")
 '(powerline-color1 "#191919")
 '(powerline-color2 "#111111")
 '(vc-annotate-background "#2B2B2B")
 '(vc-annotate-color-map (quote ((20 . "#BC8383") (40 . "#CC9393") (60 . "#DFAF8F") (80 . "#D0BF8F") (100 . "#E0CF9F") (120 . "#F0DFAF") (140 . "#5F7F5F") (160 . "#7F9F7F") (180 . "#8FB28F") (200 . "#9FC59F") (220 . "#AFD8AF") (240 . "#BFEBBF") (260 . "#93E0E3") (280 . "#6CA0A3") (300 . "#7CB8BB") (320 . "#8CD0D3") (340 . "#94BFF3") (360 . "#DC8CC3"))))
 '(vc-annotate-very-old-color "#DC8CC3")
 '(weechat-color-list (quote (unspecified "#002b36" "#073642" "#990A1B" "#dc322f" "#546E00" "#859900" "#7B6000" "#b58900" "#00629D" "#268bd2" "#93115C" "#d33682" "#00736F" "#2aa198" "#839496" "#657b83"))))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(put 'set-goal-column 'disabled nil)
