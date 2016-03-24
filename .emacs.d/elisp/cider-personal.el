(provide 'cider-personal)

;; cider refcard here: https://github.com/clojure-emacs/cider/blob/master/doc/cider-refcard.pdf
(evil-leader/set-key-for-mode 'clojure-mode
  "c" 'cider-jack-in
  "e" 'cider-eval-last-sexp
  "d" 'cider-doc)

(add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode)
(add-hook 'cider-repl-mode-hook 'paredit-mode)
(setq cider-repl-pop-to-buffer-on-connect t)
