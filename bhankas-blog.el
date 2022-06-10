;; bhankas-blog.el --- an org-mode blog engine -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;; A static site generator using org-mode, org-publish and Emacs-Lisp
;;;
;;; TODO: Use shr-dom-to-xml to generate HTML from Elisp quoted list.
;;; TODO: Use libxml-parse-html-region to generate Elisp from HTML.

;;; Code:

(require 'org)
(require 'shr)

(defconst bhankas-blog-src-path  "~/org/blog/src/posts/")
(defvar bhankas-blog-src-file-list)
(defvar bhankas-blog-attr-hashtable)
(defvar bhankas-blog-posts-list)

(defun bhankas-blog-src-list-files ()
  "List all post files."
  (setq bhankas-blog-src-file-list
        (mapcar (lambda (fname) (concat bhankas-blog-src-path fname))
                (remove "rss.org" (directory-files bhankas-blog-src-path nil "\.org$" t)))))

(defun bhankas-get-file-attrs (file-path)
  "Get TITLE and ID of `FILE-PATH'.

Returns a property list of:
1. Title
2. ID
3. Date_created
4. Path."
  (with-temp-buffer
    (insert-file-contents file-path)
    (let ((keywords (mapcar #'car (mapcar #'cdr (org-collect-keywords '("TITLE" "DATE")))))
          (id (org-entry-get 1 "ID")))
      `(:title ,(car keywords) :id ,id :date_created ,(cadr keywords) :path ,file-path))))

(defun bhankas-blog-get-src-attrs ()
  "Generate a table with TITLE as key and attribute property-list as value.

Returns a hashmap where:
key = title
value = property-list of title, ID, date_created and path."
  (setq bhankas-blog-attr-hashtable (make-hash-table :size (length bhankas-blog-src-file-list) :test #'equal))
  (dolist (file bhankas-blog-src-file-list)
    (let (attrs)
      (setq attrs (bhankas-get-file-attrs file))
      (puthash (plist-get attrs :title) attrs bhankas-blog-attr-hashtable))))

(defun bhankas-index-sort-reverse-chrono ()
  "List posts reverse chronologically."
  (setq bhankas-blog-posts-list
        (let ((values ()))
          (maphash (lambda (k v) (push v values)) bhankas-blog-attr-hashtable)
          (cl-stable-sort values
                          #'string>
                          :key #'(lambda (attrs) (plist-get attrs :date_created))))))

; TODO rewrite:
; 1. Only have single heading in index.org, with some default, identifiable name.
; 2. Publish blog
; 3. Convert published index.html to dom (lisp) using libxml-parse-html-region.
; 4. Insert the reverse chronological list of posts using a macro or something
; 5. Convert the dom lisp back to html and save in index.html
; 6. ???
; 7. Profit!

(defun bhankas-blog-org-publish-update-index ()
  "Update `TITLE' and `FILE-ID' in blog index."
  (interactive))

(defun bhankas-blog-rebuild ()
  "Rebuild the blog."
  (interactive)
  (make-thread (lambda ()
                 (progn
                   (bhankas-blog-src-list-files)
                   (bhankas-blog-get-src-attrs)
                   ;; (org-publish "blog" t)
                   ;; (bhankas-blog-org-publish-update-index)
                   ))
               "bhankas-blog-rebuild"))

;;; bhankas-blog.el ends here
