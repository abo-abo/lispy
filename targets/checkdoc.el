(setq files '("lispy.el"
              "lispy-inline.el"
              "le-clojure.el"
              "le-scheme.el"
              "le-lisp.el"))
(require 'checkdoc)
(dolist (file files)
  (checkdoc-file file))
