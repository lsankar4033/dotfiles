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
    magit ;; TODO check this out
    markdown-mode
    evil
    evil-leader
    neotree
    ag
    fill-column-indicator))

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

;; Sets up exec-path-from shell
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize)
  (exec-path-from-shell-copy-envs
   '("PATH")))

;; These customizations make it easier for you to navigate files,
;; switch buffers, and choose options from the minibuffer.
(load "navigation.el")

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

;; Markdown mode
(add-to-list 'auto-mode-alist '("\\.text\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
