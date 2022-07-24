(require 'indium)

(defun lispy--eval-js (str)
  (let ((r nil))
    (indium-eval
     str (lambda (value) (setq r (indium-render-remote-object-to-string value))))
    (while (not r)
      (accept-process-output))
    (substring-no-properties r)))

(defun lispy--eval-js-str ()
  (if (region-active-p)
      (lispy--string-dwim)
    (lispy--string-dwim
     (lispy-bounds-python-block))))

(defun lispy--js-completion-at-point ()
  (let* ((prefix (buffer-substring-no-properties
                  (let ((bol (point-at-bol))
                        (prev-delimiter (1+ (save-excursion
                                              (re-search-backward "[([:space:]]" nil t)))))
                    (if prev-delimiter
                        (max bol prev-delimiter)
                      bol))
                  (point)))
         (expression (if (string-match-p "\\." prefix)
                         (replace-regexp-in-string "\\.[^\\.]*$" "" prefix)
                       "this"))
         (cands nil))
    (indium-client-get-completion
     expression
     indium-debugger-current-frame
     (lambda (candidates)
       (setq cands candidates)))
    (while (null cands)
      (accept-process-output))
    (list (- (point) (length (company-grab-symbol)))
          (point)
          (mapcar #'identity cands))))

(provide 'le-js)
