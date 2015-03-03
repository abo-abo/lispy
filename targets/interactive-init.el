;;* Base
(require 'lispy)
(dolist (h '(emacs-lisp-mode-hook
             lisp-interaction-mode-hook
             clojure-mode-hook
             scheme-mode-hook
             lisp-mode-hook))
  (add-hook h #'lispy-mode))

;;* Common Lisp
(add-to-list 'load-path "~/git/slime")
(require 'slime-autoloads)
(setq inferior-lisp-program "/usr/bin/sbcl")
(setq slime-contribs '(slime-fancy))

;;* Clojure
(add-to-list 'load-path "~/git/clojure-semantic")
(load "~/git/clojure-semantic/clojure.el")
