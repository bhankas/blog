(require 'ox-publish)
(load "~/org/blog/ox-rss.el")
(setq org-publish-project-alist
      '(("blog-posts"
         :base-directory "~/org/blog/src/posts/"
         :base-extension "org"
         :publishing-directory "~/org/blog/docs/posts/"
         :recursive t
         :publishing-function org-html-publish-to-html
         :headline-levels 3
         :org-html-preamble nil
         :html-self-link-headlines nil
         :org-export-with-title nil
         :org-export-with-toc nil
         :org-export-with-section-numbers nil
         :org-export-with-properties nil
         :org-export-with-tags nil
         :org-export-with-date t
         :org-export-with-time-stamp-file t
         :org-export-with-tables t
         :org-html-table-use-header-tags-for-first-column nil)
        ("blog-pages"
         :base-directory "~/org/blog/src/"
         :base-extension "org"
         :publishing-directory "~/org/blog/docs/"
         :exclude "rss.org"
         :recursive t
         :publishing-function org-html-publish-to-html
         :headline-levels 4
         :org-html-preamble nil
         :html-self-link-headlines nil
         :org-export-with-title nil
         :org-export-with-toc nil
         :org-export-with-section-numbers nil
         :org-export-with-tags nil
         :org-html-table-use-header-tags-for-first-column nil)
        ("blog-static"
         :base-directory "~/org/blog/src/"
         :base-extension "css\\|js\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg\\|swf"
         :publishing-directory "~/org/blog/docs/"
         :recursive t
         :publishing-function org-publish-attachment)
        ("blog-rss"
         :base-directory "~/org/blog/src/posts/"
         :base-extension "src"
         :recursive nil
         :exclude ".*"
         :include ("../rss.org")
         :publishing-function (org-rss-publish-to-rss)
         :publishing-directory "~/org/blog/docs/"
         :with-toc nil
         :section-numbers nil
         :title "Bhankas")
        ("blog"
         :components ("blog-posts" "blog-pages" "blog-static" "blog-rss"))))
