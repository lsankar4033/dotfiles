;;;;
;; Packages
;;;;

;; Define package repositories
(require 'package)
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives '("tromey" . "http://tromey.com/elpa/") t)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/"))

(package-initialize)

;; Download the ELPA archive description if needed.
;; This informs Emacs about the latest versions of all packages, and
;; makes them available for download.
(when (not package-archive-contents)
  (package-refresh-contents))

(defvar my-packages
  '(paredit
    clojure-mode
    clojure-mode-extra-font-locking
    cider
    ido-ubiquitous
    rainbow-delimiters
    tagedit
    magit
    markdown-mode
    evil
    evil-leader
    neotree
    ag
    fill-column-indicator
    auto-complete))

;; Set up exec-path-from-shell on osx
(if (eq system-type 'darwin)
    (add-to-list 'my-packages 'exec-path-from-shell))

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

;;;;
;; Customization
;;;;

(add-to-list 'load-path "~/.emacs.d/customizations")

;; Evil leader
(global-evil-leader-mode)
(evil-leader/set-leader ",")

;; Evil mode
(evil-mode 1)

;; Magit w/ evil
(evil-leader/set-key "g" 'magit-status) 
;; TODO determine a good way to get evil mode bindings in magit
;;(evil-set-initial-state 'magit-status-mode 'normal)
;; (evil-define-key 'normal magit-status-mode-map
;;  "j" 'magit-goto-next-section
;;  "k" 'magit-goto-previous-section) 

;; Auto-complete
(ac-config-default)

;; Sets up exec-path-from shell
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize)
  (exec-path-from-shell-copy-envs
   '("PATH")))

;; TODO inline the below items

;; These customizations make it easier for you to navigate files,
;; switch buffers, and choose options from the minibuffer.
(load "navigation.el")

(evil-leader/set-key "b" 'switch-to-buffer)

;; These customizations change the way emacs looks and disable/enable
;; some user interface elements
(load "ui.el")

;; These customizations make editing a bit nicer.
;; TODO - validate
(load "editing.el")

;; Hard-to-categorize customizations
(load "misc.el")

;; For editing lisps
;; TODO - validate
(load "elisp-editing.el")

;; Langauage-specific
;; TODO validate
(load "setup-clojure.el")
(load "setup-js.el")

;; Clojure specific bindings
;; cider refcard here: https://github.com/clojure-emacs/cider/blob/master/doc/cider-refcard.pdf
;; TODO cljs bindings
(evil-leader/set-key-for-mode 'clojure-mode
  "c" 'cider-jack-in
  "e" 'cider-eval-last-sexp
  "d" 'cider-doc
  "f" 'cider-load-file)

;; Org mode bindings
(evil-leader/set-key-for-mode 'org-mode
  "t"  'org-show-todo-tree) 

;; Org mode TODO keywords
(setq org-todo-keywords 
      '((sequence "WAITING" "TODO" "|" "DONE" "DELEGATED")))

;; Markdown mode
(add-to-list 'auto-mode-alist '("\\.text\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
