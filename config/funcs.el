(defun comment-or-uncomment-line-or-region ()
  "Toggles commenting on the current line if no region is defined,
   otherwise toggles comments on the region"
  (interactive "*")
  (let ((use-empty-active-region t) (mark-even-if-inactive nil))
    (cond
     ((use-region-p) (comment-or-uncomment-region (region-beginning) (region-end)))
     (t (comment-or-uncomment-region (line-beginning-position) (line-end-position))))))

;; Fun comments with boxing
(defun box-comment() ;; "defun" is a macro for defining named functions in emacs lisp
  ;; This is the docstring
  "Creates a box-style comment. Visually appealing, and useful for having
different types of comments section our your code more thoroughly."
  (interactive "*") ;; What kind of arguments does this function accept? (none, in this case)
  (let
    ((comment-style 'box))
    (comment-region
     (region-beginning)
     (region-end))))

(defun box-uncomment()
  "Destroys a box created with box-comment, leaving the original contents
intact. More specifically, it makes two calls to uncomment-region in
order to remove the comment characters from the front and back of the
box, then it attempts to remove the blank lines left over by this operation."
  (interactive "*")
  (let
      ((comment-style 'box)
       (clear1 (region-beginning))
       (clear2 (region-end)))

    ;; Uncomment twice: once for front comments, another for back comments
    (dotimes (i 2)
      (uncomment-region
       (region-beginning)
       (region-end)))

    (delete-trailing-whitespace)
    (save-excursion
      (save-restriction
	(goto-char (region-beginning))
	(forward-line -1)
	(setq clear1 (point))
	(save-excursion
	  (goto-char (region-end))
	  (beginning-of-line)
	  (forward-line 2)
	  (setq clear2 (point)))
	(narrow-to-region clear1 clear2)
	(delete-blank-lines)
	(goto-char (region-end))
	(delete-blank-lines)))))

(provide 'funcs)
