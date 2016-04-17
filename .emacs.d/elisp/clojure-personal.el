(provide 'clojure-personal)

(add-hook 'clojure-mode-hook 'enable-paredit-mode)
(add-to-list 'auto-mode-alist '("\\.edn$" . clojure-mode))
(add-to-list 'auto-mode-alist '("\\.cljs.*$" . clojure-mode))

;; Clojure indentation rules
(eval-after-load 'clojure-mode
  '(define-clojure-indent
     (assoc 1) (-> 0) (->> 0) (cond-> 0) (cond->> 0)                   ; Override defaults
     (send-off 1) (cli 1) (go-loop 1)                                  ; Core
     (ANY 2) (GET 2) (POST 2) (PUT 2) (PATCH 2) (DELETE 2) (context 2) ; Compojure
     (OPTIONS 2)
     (select 1) (insert 1) (update 1) (where 1) (set-fields 1)         ; Korma
     (values 1) (delete 1) (upsert 1) (subselect 1)
     (clone-for 1)                                                     ; Enlive
     (up 1) (down 1) (alter 1) (table 1) (create 1)                    ; Lobos
     (checker 1)                                                       ; Midje
     ))
