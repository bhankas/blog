;; bhankas-blog.el --- an org-mode blog engine -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;; A static site generator using org-mode, org-publish and Emacs-Lisp

;;; Code:

(require 'org)

(defconst bhankas-blog-src-path  "~/org/blog/src/posts/")
(defvar bhankas-blog-src-file-list)
(defvar bhankas-blog-attr-hashtable)

(defun bhankas-blog-src-list-files ()
  "List all post files."
  (setq bhankas-blog-src-file-list
        (mapcar (lambda (fname) (concat bhankas-blog-src-path fname))
                (remove "rss.org" (directory-files bhankas-blog-src-path nil "\.org$" t)))))

(defun bhankas-blog-get-file-attrs (file-path)
  "Get TITLE and ID of `FILE-PATH'."
  (with-temp-buffer (insert-file-contents file-path)
                    (cons (mapconcat 'identity (cdar (org-collect-keywords '("TITLE"))) "")
                          (with-no-warnings
                            (goto-line 2)
                            (replace-regexp-in-string "\\(^:ID:[ \t]+\\|\n\\)" "" (thing-at-point 'line t))))))

(defun bhankas-blog-get-src-attrs ()
  "Generate a table with title and ID of all posts."
  (setq bhankas-blog-attr-hashtable (make-hash-table :size (length bhankas-blog-src-file-list) :test #'equal))
  (dolist (file bhankas-blog-src-file-list)
    (puthash file (bhankas-blog-get-file-attrs file) bhankas-blog-attr-hashtable)))

; done : for now.
; TODO: Sort by date published.
(defun bhankas-blog-org-publish-update-index ()
  "Update `TITLE' and `FILE-ID' in blog index."
  (interactive)
  (with-temp-buffer (insert-file-contents "~/org/blog/src/index.org")
                    (maphash (lambda (key value)
                               (let ((title (car value))
                                     (file-id (cdr value)))
                                 (unless (search-forward file-id nil t)
                                   (goto-char (point-max))
                                   (write-region (format (concat (format-time-string "%Y-%m-%d") "\n- [[id:%s][%s]]\n")
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
