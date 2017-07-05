(provide 'org-personal)
(require 'utils)

(setq org-indent-mode t)
(setq org-startup-indented t)

;; structure bindings
(evil-define-key 'normal org-mode-map (kbd "C-l") 'org-do-demote)
(evil-define-key 'normal org-mode-map (kbd "C-h") 'org-do-promote)
(evil-define-key 'normal org-mode-map (kbd "C-S-l") 'org-demote-subtree)
(evil-define-key 'normal org-mode-map (kbd "C-S-h") 'org-promote-subtree)

;; todo bindings
(evil-leader/set-key-for-mode 'org-mode
  "c" 'org-todo
  "a" 'org-agenda-list
  "z" 'org-open-at-point
  "h" 'org-insert-heading
  "y" 'org-insert-heading-respect-content)

;; timestamp bindings
(evil-leader/set-key-for-mode 'org-mode
  "s" 'org-schedule
  "d" 'org-deadline
  "q" 'org-time-stamp

  ;; TODO revisit these- they're not ergonomic
  "j" 'org-timestamp-down-day
  "k" 'org-timestamp-up-day)

;; tags bindings
(evil-leader/set-key-for-mode 'org-mode
  "e" 'org-set-tags-command
  "w" 'org-tags-view)

;; NOTE turn these back on when I need them (and make sure they work)
;; babel bindings
;(evil-leader/set-key-for-mode 'org-mode
;  "x" 'org-babel-execute-src-block
;(setq org-src-tab-acts-natively t)
;(setq org-confirm-babel-evaluate nil)
; (org-babel-do-load-languages 'org-babel-load-languages
; '((emacs-lisp . t)
;  (R . t)
;  (sql . t)
;  (sh . t)))

(evil-leader/set-key-for-mode 'org-mode
  "i" 'org-toggle-inline-images)

(setq org-todo-keywords
      '((sequence "WAITING" "TODO" "MUST DO" "IN PROGRESS" "|" "DONE" "INVALIDATED" "DELEGATED")))

;; spreadsheet bindings
(evil-leader/set-key-for-mode 'org-mode
  "ti" 'org-table-iterate
  "ts" 'org-table-sort-lines)

;; recursively find .org files in provided directory
;; modified from an Emacs Lisp Intro example
(defun sa-find-org-file-recursively (&optional directory filext)
  "Return .org and .org_archive files recursively from DIRECTORY.
If FILEXT is provided, return files with extension FILEXT instead."
  (let* (org-file-list
	 (case-fold-search t)	      ; filesystems are case sensitive
	 (file-name-regex "^[^.#].*") ; exclude dot, autosave, and backup files
	 (filext (or filext "org$\\\|org_archive"))
	 (fileregex (format "%s\\.\\(%s$\\)" file-name-regex filext))
	 (cur-dir-list (directory-files directory t file-name-regex)))
    ;; loop over directory listing
    (dolist (file-or-dir cur-dir-list org-file-list) ; returns org-file-list
      (cond
       ((file-regular-p file-or-dir) ; regular files
	(if (string-match fileregex file-or-dir) ; org files
	    (add-to-list 'org-file-list file-or-dir)))
       ((file-directory-p file-or-dir)
	(dolist (org-file (sa-find-org-file-recursively file-or-dir filext)
			  org-file-list) ; add files found to result
	  (add-to-list 'org-file-list org-file)))))))

;; Make org-indent-mode play nicely with autofill. Taken from this Stackoverflow answer:
;; http://stackoverflow.com/questions/14351154/org-mode-outline-level-specific-fill-column-values#
;; NOTE this doesn't work yet! I should debug when I get a chance

(defun calc-offset-on-org-level ()
  "Calculate offset (in chars) on current level in org mode file."
  (* (or (org-current-level) 0) org-indent-indentation-per-level))

(defun my-org-fill-paragraph (&optional JUSTIFY)
  "Calculate apt fill-column value and fill paragraph."
  (let* ((fill-column (- fill-column (calc-offset-on-org-level))))
    (org-fill-paragraph JUSTIFY)))

(defun my-org-auto-fill-function ()
  "Calculate apt fill-column value and do auto-fill"
  (let* ((fill-column (- fill-column (calc-offset-on-org-level))))
    (org-auto-fill-function)))

(defun my-org-mode-hook ()
  (setq fill-paragraph-function   'my-org-fill-paragraph
        normal-auto-fill-function 'my-org-auto-fill-function))

(add-hook 'org-load-hook 'my-org-mode-hook)
(add-hook 'org-mode-hook 'my-org-mode-hook)
(setq org-agenda-files (sa-find-org-file-recursively global-docs-dir))
