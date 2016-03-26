(provide 'cider-personal)
(require 'utils)

(defun my-cider-restart-nrepl ()
  "Restarts or starts afresh the nrepl."
  (interactive)
  (let ((repl-buffer (nrepl-connection-for-buffer (current-buffer))))
    (util/without-confirmation (lambda ()
                            (when (not (stringp repl-buffer))
                              (nrepl-close repl-buffer))
                            (cider-jack-in nil)))))

;; cider refcard here: https://github.com/clojure-emacs/cider/blob/master/doc/cider-refcard.pdf
(evil-leader/set-key-for-mode 'clojure-mode
  "c" 'my-cider-restart-nrepl
  )

(evil-define-key 'normal clojure-mode-map "D"
  (lambda () (interactive) (preserve-selected-window (lambda () (call-interactively 'cider-doc)))))

(add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode)
(add-hook 'cider-repl-mode-hook 'paredit-mode)
(setq cider-repl-pop-to-buffer-on-connect t)
