(setq files '("lispy.el"
              "lispy-inline.el"
              "le-clojure.el"
              "le-scheme.el"
              "le-lisp.el"))
(mapc #'byte-compile-file files)
(require 'checkdoc)
(dolist (file files)
  (with-current-buffer (find-file file)
    (checkdoc-current-buffer t)))
