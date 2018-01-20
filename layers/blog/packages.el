(setq blog-packages
      '(
        blog-admin
        org-page
        ))

(defun blog/init-blog-admin ()
  (use-package blog-admin
    :init
    (progn
      (setq blog-admin-backend-path "~/Blog")
      (setq blog-admin-backend-type 'org-page)
      (setq blog-admin-backend-new-post-in-drafts t)
      (setq blog-admin-backend-new-post-with-same-name-dir t)
      (setq blog-admin-backend-org-page-drafts "_drafts")
      (add-hook 'blog-admin-backend-after-new-post-hook 'find-file)
      )))

(defun blog/init-org-page ()
  (use-package org-page
    :init
    (progn
      (setq op/repository-directory "~/Blog")
      (setq op/site-domain "https://zwb-ict.github.io")
      (setq op/personal-disqus-shortname "zwb-ict")
      (setq op/personal-google-analytics-id "UA-58174747-1")
      ; (setq op/theme 'default)
      (setq op/site-main-title "Nortrom的信条")
      (setq op/site-sub-title "=============> 这里没有上帝，只有属于我们自己的信条。")
      (setq op/personal-github-link "https://github.com/zwb-ict")

      (setq op/category-config-alist
            '(("blog" ;; this is the default configuration
               :show-meta t
               :show-comment t
               :uri-generator op/generate-uri
               :uri-template "/blog/%y/%m/%d/%t/"
               :sort-by :date     ;; how to sort the posts
               :category-index t) ;; generate category index or not
              ("wiki"
               :show-meta t
               :show-comment nil
               :uri-generator op/generate-uri
               :uri-template "/wiki/%t/"
               :sort-by :mod-date
               :category-index t)
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
               :category-index nil))))))
