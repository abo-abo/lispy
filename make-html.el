(defun make-html-region--replace-1 (x)
  (format "<cursor>%c</cursor><span class=\"region\">%s</span>"
          (aref x 1)
          (regexp-quote
           (substring x 2 (- (length x) 1)))))

(defun make-html-region--replace-2 (x)
  (let ((ch (aref x (- (length x) 1))))
    (if (eq ch ?|)
        (format "<span class=\"region\">%s</span><cursor> </cursor>"
                (regexp-quote (substring x 1 (- (length x) 1))))
      (format "<span class=\"region\">%s</span><cursor>%c</cursor>"
          (regexp-quote
           (substring x 1 (- (length x) 2)))
          ch))))

(defun make-html-cursor--replace (x)
  (if (string= "|\n" x)
      "<cursor> </cursor>\n"
    (if (string= "|[" x)
        "<cursor>[</cursor>"
      (format "<cursor>%s</cursor>"
              (regexp-quote
               (substring x 1))))))

(defun make-html-region (str x y)
  (setq str
        (replace-regexp-in-string
         "|[^|~]+~"
         #'make-html-region--replace-1
         str))
  (setq str
        (replace-regexp-in-string
         "~[^|~]+|\\(?:.\\|$\\)"
         #'make-html-region--replace-2
         str))
  (replace-regexp-in-string
   "|\\(.\\|\n\\)"
   #'make-html-cursor--replace
   str))

(defun org-src-denote-region (&optional context)
  (when (and (memq major-mode '(emacs-lisp-mode))
             (region-active-p))
    (let ((pt (point))
          (mk (mark)))
      (deactivate-mark)
      (insert "|")
      (goto-char (if (> pt mk) mk (1+ mk)))
      (insert "~"))))

(advice-add 'org-edit-src-exit :before #'org-src-denote-region)

(defun org-babel-edit-prep:elisp (info)
  (when (string-match "[~|][^~|]+[|~]" (cadr info))
    (let (mk pt deactivate-mark)
      (goto-char (point-min))
      (re-search-forward "[|~]")
      (if (looking-back "~")
          (progn
            (backward-delete-char 1)
            (setq mk (point))
            (re-search-forward "|")
            (backward-delete-char 1)
            (set-mark mk))
        (backward-delete-char 1)
        (setq pt (point))
        (re-search-forward "~")
        (backward-delete-char 1)
        (set-mark (point))
        (goto-char pt)))))

(provide 'make-html)
