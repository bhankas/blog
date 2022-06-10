;; bhankas-blog.el --- an org-mode blog engine -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;; A static site generator using org-mode, org-publish and Emacs-Lisp

;;; Code:

(require 'org)

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

; TODO: Rewrite for new plist data structure
(defun bhankas-blog-org-publish-update-index ()
  "Update `TITLE' and `FILE-ID' in blog index."
  (interactive)
  (with-temp-buffer
    (insert-file-contents "~/org/blog/src/index.org")
    (maphash (lambda (key value)
               (let ((title (car value))
                     (file-id (cdr value)))
                 (unless (search-forward file-id nil t)
                   (goto-char (point-max))
                   (write-region (format (concat  "| [[id:%s][%s]] | \\hellip |  "
                                                  (format-time-string "%Y-%m-%d")
                                                  " | \n")
                                         file-id title)
                                 nil
                                 "~/org/blog/src/index.org"
                                 'append)
                   (goto-char (point-min)))))
             bhankas-blog-attr-hashtable)))

(defun bhankas-blog-rebuild ()
  "Rebuild the blog."
  (interactive)
  (make-thread (lambda ()
                 (progn
                   (bhankas-blog-src-list-files)
                   (bhankas-blog-get-src-attrs)
                   (bhankas-blog-org-publish-update-index)
                   (org-publish "blog" t)))
               "bhankas-blog-rebuild"))

;;; bhankas-blog.el ends here
