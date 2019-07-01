(setq files '("lispy.el"
              "lispy-inline.el"
              "le-clojure.el"
              "le-scheme.el"
              "le-lisp.el"
              "le-python.el"
              "le-racket.el"))
(setq byte-compile--use-old-handlers nil)
(mapc #'byte-compile-file files)
