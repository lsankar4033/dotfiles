(provide 'cider-personal)
(require 'utils)

(defun nrepl-connection-for-buffer (buffer)
  "Returns either the corresponding nrepl buffer for the given buffer, or a string error message."
  (if (not (cider-connected-p))
      "No active nREPL connection."
    (let ((project-directory (cider-current-dir)))
      (if (not project-directory)
          "No project directory found."
        (let ((buf (cond
                    ;; I'm special casing shared_lib et al so that I can eval files from that project against
                    ;; the most recent repl.
                    ((or (search "shared_lib" project-directory)
                         (search "ml_lib" project-directory))
                     (car nrepl-connection-list))
                    (project-directory
                     (car (-filter
                           (lambda (conn)
                             (let ((conn-proj-dir (with-current-buffer (get-buffer conn)
                                                    nrepl-project-dir)))
                               (when conn-proj-dir
                                 (equal (file-truename project-directory)
                                        (file-truename conn-proj-dir)))))
                           nrepl-connection-list))))))
          (if buf
              (get-buffer buf)
            "No relevant nREPL connection found."))))))

(defun my-cider-restart-nrepl ()
  "Restarts or starts afresh the nrepl."
  (interactive)
  (let ((repl-buffer (nrepl-connection-for-buffer (current-buffer))))
    (without-confirmation (lambda ()
			    (when (not (stringp repl-buffer))
			      (nrepl-close repl-buffer))
			    (cider-jack-in nil)))))

(defun my-cider-make-connection-buffer-the-current-connection (connection-buffer)
  (cons connection-buffer (delq connection-buffer nrepl-connection-list)))

(defun my-cider-eval-echo (&optional print-result)
  "Eval the sexp the cursor is currently in. In Emacs' syntax table, this is called a list of
   expressions. Displays output to echo area"
  (interactive)
  (cider-interactive-eval (current-sexp)))

(defun my-cider-eval-repl ()
  (interactive)
  (cider-interactive-eval (current-sexp)
			  (cider-insert-eval-handler (cider-current-connection)))
  (cider-switch-to-repl-buffer))

;; cider refcard here: https://github.com/clojure-emacs/cider/blob/master/doc/cider-refcard.pdf
(evil-leader/set-key-for-mode 'clojure-mode
  "w" 'cider-load-buffer
  "j" 'cider-jack-in
  "k" 'cider-jack-in-clojurescript
  "ee" 'my-cider-eval-echo
  "er" 'my-cider-eval-repl
  "ew" 'cider-pprint-eval-defun-at-point
  "tw" 'cider-test-run-ns-tests
  "te" 'cider-test-run-test
  "q" 'cider-interrupt
  "cl" 'cider-repl-clear-buffer)

(evil-define-key 'normal clojure-mode-map [?\]?d]
  (lambda () (interactive) (preserve-selected-window (lambda () (call-interactively 'cider-doc)))))

(evil-define-key 'normal clojure-mode-map [?\C-,] 'cider-find-var)
(evil-define-key 'normal clojure-mode-map [?\C-.] 'cider-pop-back)

(add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode)
(add-hook 'cider-repl-mode-hook 'paredit-mode)

(setq cider-repl-pop-to-buffer-on-connect t)
(setq cider-repl-use-pretty-printing t)
