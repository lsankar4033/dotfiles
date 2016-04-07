(provide 'org-personal)
(require 'utils)

(setq org-indent-mode t)
(setq org-startup-indented t)

;; structure bindings
(evil-define-key 'normal org-mode-map (kbd "C-j") 'org-demote-subtree)
(evil-define-key 'normal org-mode-map (kbd "C-k") 'org-promote-subtree)

;; todo bindings
(evil-leader/set-key-for-mode 'org-mode
  "c" 'org-todo
  "a" 'org-agenda-list
  "z" 'org-open-at-point
  "g" 'org-insert-heading
  "h" 'org-insert-heading-respect-content)

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

;; babel bindings
(evil-leader/set-key-for-mode 'org-mode
  "x" 'org-babel-execute-src-block)
(setq org-confirm-babel-evaluate nil)

(setq org-todo-keywords
      '((sequence "WAITING" "TODO" "IN PROGRESS" "|" "DONE" "DELEGATED")))

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

(setq org-agenda-files (sa-find-org-file-recursively global-docs-dir)) 
