;; Changes all yes/no questions to y/n type
(fset 'yes-or-no-p 'y-or-n-p)

;; shell scripts
(setq-default sh-basic-offset 2)
(setq-default sh-indentation 2)

;; No need for ~ files when editing
(setq create-lockfiles nil)

;; Go straight to scratch buffer on startup
(setq inhibit-startup-message t)

;; NOTE(lakshman) this is a hack to get 'goto' in clj evil mode. taken from here:
;; http://emacs.stackexchange.com/questions/608/evil-map-keybindings-the-vim-way
(defun my-jump-to-tag ()
  (interactive)
  (evil-emacs-state)
  (call-interactively (key-binding (kbd "M-.")))
  (evil-change-to-previous-state (other-buffer))
  (evil-change-to-previous-state (current-buffer)))

(define-key evil-normal-state-map (kbd "C-]") 'my-jump-to-tag)
