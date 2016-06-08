(provide 'org-page-personal)

(setq op/repository-directory "~/src/personal-website")
(setq op/site-domain "http://lakshmansankar.com")
;; for commenting; disabled for now
;;(setq op/personal-disqus-shortname "your_disqus_shortname")

;; analytics set up at ~/.emacs.secrets file
;;(setq op/personal-google-analytics-id "UA-NNNNNNNN-N")

(setq op/personal-github-link "https://github.com/lsankar4033")
(setq op/site-main-title "Lakshman Sankar")
;(setq op/site-sub-title "")

(evil-leader/set-key-for-mode 'org-mode
  "pt" (progn
	  (setq op/item-cache nil)
	  (op/do-publication t "/tmp/blog" nil t nil)))
