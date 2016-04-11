(require 'check-declare)
(setq check-declare-ext-errors t)
(setq files '("lispy.el"
              "lispy-inline.el"
              "le-clojure.el"
              "le-scheme.el"
              "le-lisp.el"))
(add-to-list 'load-path
             (concat (file-name-directory
                      (locate-library "slime"))
                     "contrib/"))
(require 'slime-repl)

(defun find-library-file (lib)
  (save-window-excursion
    (let ((buf (find-library lib)))
      (when buf
        (buffer-file-name buf)))))


(let ((sly-file (find-library-file "sly")))
  (when sly-file
    (add-to-list
     'load-path
     (expand-file-name
      "contrib" (file-name-directory sly-file)))))

(apply #'check-declare-files files)
