;; The structure of this configuration is as follows:
;; 1. package management
;; 2. initialization of evil/evil-leader. many of the items in the later steps utilize these
;; 3. generic customization
;; 4. package specific stuff. most of this should be extracted into other files

(add-to-list 'load-path "~/.emacs.d/elisp")

(require 'utils)

;;;;
;; 1. Package Management
;;;;

;; Define package repositories
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/"))
(package-initialize)
(when (not package-archive-contents) ; refresh package archives
  (package-refresh-contents))

;; Install all of these packages
(defvar my-packages
  '(
    ag
    auto-complete
    clojure-mode
    clojure-mode-extra-font-locking
    company
    deft
    evil
    evil-leader
    exec-path-from-shell
    fill-column-indicator
    ido-ubiquitous
    magit
    markdown-mode
    neotree
    paredit
    projectile
    rainbow-delimiters
    tagedit
))
(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

;;;;
;; 2. Evil + evil-leader
;; Currently the only reason this is split out is that it's a prereq for almost every dep that follows.
;;;;

(global-evil-leader-mode)
(evil-leader/set-leader ",")
(evil-mode 1)

;;;;
;; 3. Generic Customization
;;;;

(fset 'yes-or-no-p 'y-or-n-p) ; Changes all yes/no questions to y/n type
(setq inhibit-startup-message t) ; Go straight to scratch buffer on startup

;; Make it easy to reload this file
(defun reload-init ()
  (interactive)
  (load-file "~/.emacs.d/init.el"))
(evil-leader/set-key "l" 'reload-init)

(ac-config-default) ; autocomplete

(evil-leader/set-key "b" 'switch-to-buffer)

;; elisp code evaluation
(evil-leader/set-key-for-mode 'emacs-lisp-mode
  "e" 'eval-last-sexp)

(menu-bar-mode -1) ; turn off menu bar at top

(global-linum-mode)

;; Don't need the tool-bar
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))

;; Don't show native OS scroll bars for buffers because they're redundant
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))

; darcula theme
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
(add-to-list 'load-path "~/.emacs.d/themes")
(load-theme 'darcula t)

(set-face-attribute 'default nil :height 140) ; increase font size
(setq initial-frame-alist '((top . 0) (left . 0) (width . 177) (height . 52))) ; default window size

;; Copy/paste stuff
(setq x-select-enable-clipboard t
      x-select-enable-primary t

      ;; Save clipboard strings into kill ring before replacing them.
      ;; When one selects something in another program to paste it into Emacs,
      ;; but kills something in Emacs before actually pasting it,
      ;; this selection is gone unless this variable is non-nil
      save-interprogram-paste-before-kill t

      ;; Mouse yank commands yank at point instead of at click.
      mouse-yank-at-point t)

(blink-cursor-mode 0)
(setq-default frame-title-format "%b (%f)") ; full path in title bar
(setq ring-bell-function 'ignore) ; no bell!

;; max column width
(setq-default fill-column 110)
(setq-default auto-fill-function 'do-auto-fill)
(define-globalized-minor-mode global-fci-mode fci-mode (lambda () (fci-mode 1)))
(global-fci-mode 1)

(show-paren-mode 1) ; highlight matching parens

(global-hl-line-mode 1) ; highlight current line

;; Automatically load paredit when editing a lisp file
;; More at http://www.emacswiki.org/emacs/ParEdit
(autoload 'enable-paredit-mode "paredit" "Turn on pseudo-structural editing of Lisp code." t)
(add-hook 'emacs-lisp-mode-hook       #'enable-paredit-mode)
(add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode)
(add-hook 'ielm-mode-hook             #'enable-paredit-mode)
(add-hook 'lisp-mode-hook             #'enable-paredit-mode)
(add-hook 'lisp-interaction-mode-hook #'enable-paredit-mode)
(add-hook 'scheme-mode-hook           #'enable-paredit-mode)

;; eldoc-mode shows documentation in the minibuffer when writing code
;; http://www.emacswiki.org/emacs/ElDoc
(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
(add-hook 'lisp-interaction-mode-hook 'turn-on-eldoc-mode)
(add-hook 'ielm-mode-hook 'turn-on-eldoc-mode)

(setq create-lockfiles nil) ; don't need lockfiles when editing

(setq backup-inhibited t) ;disable backup
(setq auto-save-default nil) ;disable auto save

(setq split-height-threshold nil) ; only split window horizontally by default

;;;;
;; 4. Package Specific Customization
;;
;; when customization for a single package becomes unwieldy, move it out to a personal file and require it.
;;
;; NOTE - dependencies between packages aren't captured well in this structure. it might make more sense to
;; organize by semantic function or major mode
;;;;

;; company
(global-company-mode)

;; uniquify
(setq uniquify-buffer-name-style 'forward)

;; ido
(ido-mode t)
(setq ido-enable-flex-matching t)
(setq ido-use-filename-at-point nil) ; turn this annoying behaviour off
(ido-ubiquitous-mode 1) ; ido everywhere!
(evil-leader/set-key "r" 'ido-find-file) ; I miss ctrlp...

;; magit
(require 'magit-personal)

;; exec-from-shell
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize)
  (exec-path-from-shell-copy-envs
   '("PATH")))

;; clojure
(add-hook 'clojure-mode-hook 'enable-paredit-mode)
(add-to-list 'auto-mode-alist '("\\.edn$" . clojure-mode))
(add-to-list 'auto-mode-alist '("\\.cljs.*$" . clojure-mode))

;; cider
(add-to-list 'load-path "~/.emacs.d/vendor/cider-0.11.0")
(require 'cider)
(require 'cider-personal)

;; org-mode
(require 'org-personal)

;; Markdown mode
(add-to-list 'auto-mode-alist '("\\.text\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

;; projectile
(projectile-global-mode)
(evil-leader/set-key "f" 'projectile-find-file)

;; Deft
(setq deft-extensions '("txt" "md" "org"))
(setq deft-directory global-docs-dir)
(setq deft-recursive t)
(evil-leader/set-key "v" 'deft)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(cider-cljs-lein-repl
   "(do (require 'cljs.repl.node) (cemerick.piggieback/cljs-repl (cljs.repl.node/repl-env)))"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
