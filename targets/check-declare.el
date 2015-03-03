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
(apply #'check-declare-files files)
