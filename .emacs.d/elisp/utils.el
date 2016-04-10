(provide 'utils)

(defun partition (l n)
  "Return a list of L's consecutive sublists of length N."
  (assert (zerop (mod (length l) n)))
  (loop for l on l by #'(lambda (l) (nthcdr n l)) collect (subseq l 0 n)))

(defun define-keys (keymap &rest key-and-fn-pairs)
  "Like define-key, but takes a variable number of arguments -- two per key binding pair."
  (dolist (pair (partition key-and-fn-pairs 2))
    (define-key keymap (first pair) (second pair))))

(defun save-buffer-if-dirty ()
  (when (and buffer-file-name (buffer-modified-p))
    (save-buffer)))

(defun preserve-selected-window (f)
  "Runs the given function and then restores focus to the original window. Useful when you want to invoke
   a function (like showing documentation) but don't want to keep editing your current buffer."
  (lexical-let ((f f))
    (let ((original-window (selected-window)))
      (funcall f)
      (select-window original-window))))

(defun without-confirmation (fn)
  (flet ((y-or-n-p (&rest args) t)) ; Skip the confirmation prompts.
    (funcall fn)))

(defun describe-item-at-point ()
  "Show the documentation of the Elisp function and variable near point. This checks in turn:
	-- for a function name where point is
	-- for a variable name where point is"
  (interactive)
  (let (sym)
    ;; sigh, function-at-point is too clever.  we want only the first half.
    (cond ((setq sym (ignore-errors
		       (with-syntax-table emacs-lisp-mode-syntax-table
			 (save-excursion
			   (or (not (zerop (skip-syntax-backward "_w")))
			       (eq (char-syntax (char-after (point))) ?w)
			       (eq (char-syntax (char-after (point))) ?_)
			       (forward-sexp -1))
			   (skip-chars-forward "`'")
			   (let ((obj (read (current-buffer))))
			     (and (symbolp obj) (fboundp obj) obj))))))
	   (describe-function sym))
	  ((setq sym (variable-at-point)) (describe-variable sym))
	  ;; now let it operate fully -- i.e. also check the
	  ;; surrounding sexp for a function call.
	  ((setq sym (function-at-point)) (describe-function sym)))))

;; NOTE this should be changed if this moves
(defconst global-docs-dir "~/git/thought-docs/" )
