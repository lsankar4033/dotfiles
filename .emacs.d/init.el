;; The structure of this configuration is as follows:
;; 1. package management
;; 2. initialization of evil/evil-leader. many of the items in the later steps utilize these
;; 3. generic customization
;; 4. package specific stuff. most of this should be extracted into other files

(add-to-list 'load-path "~/.emacs.d/elisp")

;; (load "~/.emacs.d/vendor/looking_glass/looking_glass.el")
(load "~/.emacs.d/vendor/cljfmt/cljfmt.el")

(require 'utils)

;;;;
;; 1. Package Management
;;;;

;; Define package repositories
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/"))
(add-to-list 'package-archives '("elpy" . "http://jorgenschaefer.github.io/packages/"))
(package-initialize)
(when (not package-archive-contents) ; refresh package archives
  (package-refresh-contents))


;; Attempt installation all of these packages
(defvar my-packages
  '(
    ag
    auto-complete
    cider
    clojure-mode
    clojure-mode-extra-font-locking
    coffee-mode
    company
    elpy
    enh-ruby-mode
    ess
    esup
    evil
    evil-leader
    exec-path-from-shell
    fill-column-indicator
    flycheck
    grizzl
    haml-mode
    less-css-mode
    magit
    markdown-mode
    neotree
    org-page
    paredit
    projectile
    rainbow-delimiters
    solidity-mode
    tagedit
    yaml-mode
))
(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

(find-lisp-object-file-name 'package-install (symbol-function 'package-install))

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
(global-auto-complete-mode)

(evil-leader/set-key "b" 'switch-to-buffer)

;; elisp code evaluation
(evil-leader/set-key-for-mode 'emacs-lisp-mode
  "e" 'eval-last-sexp)
(evil-leader/set-key-for-mode 'emacs-lisp-mode
  "w" 'eval-buffer)

(menu-bar-mode -1) ; turn off menu bar at top

(global-linum-mode) ; line numbers

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
(setq-default fci-rule-column 110)
(setq-default auto-fill-function 'do-auto-fill)

(define-globalized-minor-mode global-fci-mode fci-mode (lambda () (fci-mode 1)))
(global-fci-mode 1)

(show-paren-mode 1) ; highlight matching parens

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

;; put backups in temporary-file-directory
(setq backup-directory-alist
      `((".*" .,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

(setq split-height-threshold nil) ; only split window horizontally by default

(add-hook 'before-save-hook 'delete-trailing-whitespace) ; delete trailing whitespace on save

(evil-define-key 'normal emacs-lisp-mode-map [?\]?d] 'describe-item-at-point)

(setq-default evil-symbol-word-search t) ; make '*' and '#' symbol search instead of word search

;; Making text bigger/smaller
(define-key evil-normal-state-map (kbd "s-=") 'text-scale-increase)
(define-key evil-normal-state-map (kbd "s--") 'text-scale-decrease)

;; Toggling debug messages
(define-key evil-normal-state-map (kbd "C-d") 'toggle-debug-on-error)

;; Don't use tabs by default
(setq-default indent-tabs-mode nil)

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
  (exec-path-from-shell-initialize))

;; clojure
(require 'clojure-personal)

;; cider
(require 'cider-personal)

;; org-mode
(require 'org-personal)

;; Markdown mode
(add-to-list 'auto-mode-alist '("\\.text\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

;; Keybinding for using markdownd to preview markdown rendering in browser.
(defun watch-and-render-markdown ()
  "Use markdownd to set a watch on the markdown file in the current buffer. Will render in the default
  browser."
  (interactive)
  (shell-command
   (format "markdownd -w %s >/dev/null &"
           (shell-quote-argument (buffer-file-name)))))
(evil-leader/set-key-for-mode 'markdown-mode "d" 'watch-and-render-markdown)

;; projectile
(require 'grizzl)
(projectile-global-mode)
(setq projectile-enable-caching t)
(setq projectile-completion-system 'grizzl)
(evil-leader/set-key "f" 'projectile-find-file)

;; paredit
(evil-leader/set-key-for-mode 'clojure-mode
  "C-l" 'paredit-forward-slurp-sexp)
(evil-leader/set-key-for-mode 'emacs-lisp-mode
  "C-l" 'paredit-forward-slurp-sexp)

;; looking-glass
;; (global-looking-glass-mode)

;; sql-postgres
(define-key evil-normal-state-map (kbd "] C-p") 'sql-postgres)

; NOTE - turned off elpy while I'm not using it
;; elpy
;(require 'elpy-personal)

;; flycheck
; TODO - uncomment these once I get pylint working without errors again
(add-hook 'after-init-hook #'global-flycheck-mode)

;; neotree
(define-key evil-normal-state-map (kbd "C-n") 'neotree-toggle)
(add-hook 'neotree-mode-hook
	  (lambda ()
	    (define-key evil-normal-state-local-map (kbd "TAB") 'neotree-enter)
	    (define-key evil-normal-state-local-map (kbd "RET") 'neotree-change-root)
	    (define-key evil-normal-state-local-map (kbd "c") 'neotree-create-node)
	    (define-key evil-normal-state-local-map (kbd "d") 'neotree-delete-node)
	    (define-key evil-normal-state-local-map (kbd "m") 'neotree-rename-node)
	    (define-key evil-normal-state-local-map (kbd "H") 'neotree-hidden-file-toggle)))
(setq-default neo-show-hidden-files t)
(setq neo-smart-open t) ; always open to directory of current buffer
(setq neo-theme 'nerd)

;; yaml-mode
(add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))

;; cljfmt
(add-hook 'before-save-hook 'cljfmt-before-save)

;; coffee-script
(evil-leader/set-key-for-mode 'coffee-mode "c" 'coffee-compile-buffer)
(evil-leader/set-key-for-mode 'coffee-mode "p" 'coffee-repl)

;; html
(evil-leader/set-key-for-mode 'html-mode "c" 'sgml-close-tag)
(evil-leader/set-key-for-mode 'html-mode "l" 'sgml-skip-tag-forward)
(evil-leader/set-key-for-mode 'html-mode "h" 'sgml-skip-tag-backward)

;; org-page
(require 'org-page)
(require 'org-page-personal)

;; esup
(setq-default user-init-file "~/.emacs.d/init.el")

;; java
(require 'java-personal)

;; less/css mode
(setq css-indent-offset 2)

;; ruby
;; NOTE: This sets ruby mode properly for a number of filetypes in addition to just '.rb' files
(add-to-list 'auto-mode-alist
             '("\\(?:\\.rb\\|ru\\|rake\\|thor\\|jbuilder\\|gemspec\\|podspec\\|/\\(?:Gem\\|Rake\\|Cap\\|Thor\\|Vagrant\\|Guard\\|Pod\\)file\\)\\'" . enh-ruby-mode))
(setq ruby-indent-level 2)

;; Solidity
(require 'solidity-mode)

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; haml mode
(require 'haml-mode)

;; ag
(evil-leader/set-key "s" 'ag-project)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (solidity-mode yaml-mode tagedit sql-indent rainbow-delimiters projectile paredit org-page neotree markdown-mode magit lua-mode less-css-mode ido-ubiquitous go-mode flycheck fill-column-indicator exec-path-from-shell evil-vimish-fold evil-leader esup ess enh-ruby-mode elpy coffee-mode clojure-mode-extra-font-locking cider auto-complete android-mode ag))))
