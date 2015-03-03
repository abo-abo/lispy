(setq files '("lispy.el"
              "lispy-inline.el"
              "le-clojure.el"
              "le-scheme.el"
              "le-lisp.el"))
(mapc #'byte-compile-file files)

