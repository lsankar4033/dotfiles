(setq op/repository-directory "~/src/personal-website/")
(setq op/site-domain "http://lsankar.com/")
;; for commenting; disabled for now
;;(setq op/personal-disqus-shortname "your_disqus_shortname")

;; analytics set up at ~/.emacs.secrets file
;;(setq op/personal-google-analytics-id "UA-NNNNNNNN-N")

(setq op/personal-github-link "https://github.com/lsankar4033")
(setq op/site-main-title "Lakshman Sankar")
(setq op/site-sub-title "Thoughts about the machine")
(setq op/site-preview-directory "~/tmp/blog/")

(setq op/category-config-alist
      '(("blog" ;; this is the default configuration
	 :show-meta t
	 :show-comment t
	 :uri-generator op/generate-uri
	 :uri-template "/blog/%y/%m/%d/%t/"
	 :sort-by :date     ;; how to sort the posts
	 :category-index t) ;; generate category index or not
	("index"
	 :show-meta nil
	 :show-comment nil
	 :uri-generator op/generate-uri
	 :uri-template "/"
	 :sort-by :date
	 :category-index nil)
	("about"
	 :show-meta nil
	 :show-comment nil
	 :uri-generator op/generate-uri
	 :uri-template "/about/"
	 :sort-by :date
	 :category-index nil)))

(provide 'org-page-personal)
