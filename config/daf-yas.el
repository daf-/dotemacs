;; Settings for yasnippet
;; Daniel Friedman, January 2013

(require 'yasnippet)

;; makes TAB work in terminal -- from http://capitaomorte.github.com/yasnippet/faq.html
(defun yas/advise-indent-function (function-symbol)
  (eval `(defadvice ,function-symbol (around yas/try-expand-first activate)
           ,(format
             "Try to expand a snippet before point, then call `%s' as usual"
             function-symbol)
           (let ((yas/fallback-behavior nil))
             (unless (and (interactive-p)
                          (yas/expand))
           ad-do-it)))))
(yas/advise-indent-function 'comment-indent-new-line)

;; allow nested snippets to be triggered correctly
(setq yas/triggers-in-field)

(provide 'daf-yas)
