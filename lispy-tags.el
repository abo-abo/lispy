;;; lispy-tags.el --- Facilitate getting a pretty list of tags for many files -*- lexical-binding: t -*-

;; Copyright (C) 2015 Oleh Krehel

;; This file is not part of GNU Emacs

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;

;;; Code:

(defvar lispy-db (make-hash-table :test 'equal)
  "An alist of file to a pretty list of tags.")

(cl-defstruct lispy-dbfile
  file
  tags
  modtime)

(defun lispy--file-list ()
  "Get the list of same type files in current directory."
  (let ((ext (file-name-extension (buffer-file-name))))
    (nreverse
     (cl-remove-if
      (lambda (x) (string-match "\\(?:^\\.?#\\|~$\\|loaddefs.el\\)" x))
      (file-expand-wildcards (format "*.%s" ext))))))

(defun lispy-build-semanticdb (&optional dir)
  "Build and save semanticdb for DIR."
  (interactive)
  (setq dir (or dir default-directory))
  (let ((default-directory dir))
    (dolist (f (lispy--file-list))
      (let ((buf (get-file-buffer f)))
         (with-current-buffer (find-file-noselect f)
           (semantic-mode 1)
           (let ((semantic-parse-tree-state 'needs-rebuild))
             (lispy--fetch-this-file-tags))
           (unless buf
             (kill-buffer))))))
  (let ((db (semanticdb-directory-loaded-p dir)))
    (or (semanticdb-save-db db) db)))

(defun lispy--format-tag-line (x)
  "Add file name to (`lispy--tag-name' X)."
  (if (and (eq lispy-completion-method 'ido)
           (not (or (bound-and-true-p ido-vertical-mode)
                    (bound-and-true-p ivy-mode))))
      x
    (let* ((width (min (window-width) (cadr lispy-helm-columns)))
           (s1 (car x))
           (s2 (file-name-nondirectory
                (cadr x))))
      (cons (format (format "%%s%% %ds" (- width
                                           (length s1)))
                    s1 s2)
            x))))

(defun lispy--file-fresh-p (actual-time stored-time)
  "Return t when ACTUAL-TIME isn't much larger than STORED-TIME."
  (and stored-time
       (< (time-to-seconds
           (time-subtract
            actual-time
            stored-time))
          1.0)))

(defvar lispy-force-reparse nil
  "When non-nil, ignore that tags are up-to-date and parse anyway.")

(defun lispy--fetch-tags (&optional file-list)
  "Get a list of tags for FILE-LIST."
  (setq file-list (or file-list (lispy--file-list)))
  (let (res dbfile db-to-save)
    (dolist (file file-list)
      (let ((file-modtime (nth 5 (file-attributes file 'integer)))
            (exfile (expand-file-name file)))
        (unless (and (null lispy-force-reparse)
                     (setq dbfile
                           (gethash exfile lispy-db))
                     (lispy--file-fresh-p
                      file-modtime
                      (lispy-dbfile-modtime dbfile))
                     (lispy-dbfile-tags dbfile))
          (let ((table (semanticdb-create-table-for-file (expand-file-name file))))
            (if (null table)
                (error "Couldn't open semanticdb for file: %S" file)
              (let ((db (car table))
                    (table (cdr table)))
                (unless (and (null lispy-force-reparse)
                             (lispy--file-fresh-p
                              file-modtime
                              (oref table lastmodtime))
                             (ignore-errors
                               (oref table tags)))
                  (let ((buf (get-file-buffer file)))
                    (with-current-buffer (or buf (find-file-noselect file))
                      (semantic-new-buffer-fcn)
                      (semantic-mode 1)
                      (oset table tags
                            (let ((semantic-parse-tree-state 'needs-update))
                              (lispy--fetch-this-file-tags file)))
                      (oset table lastmodtime
                            (current-time))
                      (semanticdb-set-dirty table)
                      (cl-pushnew db db-to-save)
                      (unless buf
                        (kill-buffer)))))
                (puthash
                 exfile
                 (setq dbfile
                       (make-lispy-dbfile
                        :file file
                        :modtime (oref table lastmodtime)
                        :tags (mapcar
                               (lambda (x)
                                 (lispy--make-tag x exfile))
                               (oref table tags))))
                 lispy-db)))))
        (setq res (append (lispy-dbfile-tags dbfile) res))))
    (dolist (db db-to-save)
      (semanticdb-save-db db))
    res))

(defun lispy--make-tag (tag file)
  "Construct a modified TAG entry including FILE."
  (list (lispy--tag-name tag file)
        file
        (semantic-tag-overlay tag)))


(provide 'lispy-tags)

;;; lispy-tags.el ends here
