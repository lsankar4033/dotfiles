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

;; NOTE this should be changed if this moves
(defconst global-docs-dir "~/git/thought-docs/" )
