(provide 'elpy-personal)

(elpy-enable)
(elpy-use-ipython)

; Basic nav
(evil-define-key 'normal python-mode-map [?\C-j] 'elpy-nav-forward-block)
(evil-define-key 'normal python-mode-map [?\C-k] 'elpy-nav-backward-block)
(evil-define-key 'normal python-mode-map [?\C-l] 'elpy-nav-forward-indent)
(evil-define-key 'normal python-mode-map [?\C-h] 'elpy-nav-backward-indent)

; evaluating code. keys are picked to match clojure mappings for minimal cognitive load
(evil-leader/set-key-for-mode 'python-mode
  "j" 'elpy-shell-switch-to-shell
  "w" 'elpy-shell-send-region-or-buffer
  "e" 'elpy-shell-send-current-statement)
