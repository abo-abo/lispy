(add-to-list 'load-path (expand-file-name "../../org-mode/lisp"))
(add-to-list 'load-path (expand-file-name "../../swiper/doc"))
(add-to-list 'load-path "../../eclipse-theme")
(add-to-list 'load-path "../../htmlize")
(require 'ivy-ox)
(require 'org nil t)
(require 'eclipse-theme nil t)
(require 'htmlize nil t)

(setq org-confirm-babel-evaluate nil)

(defun org-export-get-reference (_datum _info)
  "foobar")

(defun doexport ()
  (interactive)
  (org-html-export-to-html)
  (kill-emacs))
