(provide 'magit-personal)
(require 'utils)

;; initialize magit-status and magit-commit in same mode as everything else
(evil-set-initial-state 'magit-status-mode evil-default-state)
(evil-set-initial-state 'magit-commit-mode evil-default-state)

;; key binding for magit status
(defun magit-status-and-focus-unstaged ()
  "Opens the magit-status view and focuses the cursor on the first unstaged file."
  (interactive)
  (call-interactively 'magit-status)
  (magit-jump-to-unstaged)
  (magit-goto-next-section))
(evil-leader/set-key
  "g" (lambda() (interactive)
          (save-buffer-if-dirty)
          (magit-status-and-focus-unstaged)))

(defun init-magit-status-mode-keybindings ()
  (define-keys evil-normal-state-local-map
    "c" 'magit-commit
    "C" (lambda() (interactive) (with-env-var "SKIP_GIT_STYLE_CHECK" "true" 'magit-commit))
    "e" 'magit-show-level-4-all ; e for exapnd
    "d" 'magit-discard-item
    "s" 'magit-stage-item
    "S" 'magit-stage-all
    "u" 'magit-unstage-item
    "U" 'magit-unstage-all
    "gu" 'magit-jump-to-unstaged
    (kbd "TAB") 'magit-toggle-section
    "r" 'magit-refresh
    ",ps" 'magit-push
    ",pu" 'magit-pull))

(add-hook 'magit-status-mode-hook 'init-magit-status-mode-keybindings)

(defun init-magit-commit-mode-keybindings ()
  (define-keys evil-normal-state-local-map
    (kbd "C-c") 'git-commit-commit))
(add-hook 'magit-commit-mode-hook 'init-magit-commit-mode-keybindings)

;; Ensure evil-leader works in non-editing modes like magit. This is referenced from evil-leader's README.
(setq evil-leader/no-prefix-mode-rx '("magit-.*-mode"))
