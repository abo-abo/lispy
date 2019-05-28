;;* Base
(require 'lispy)
(dolist (h '(emacs-lisp-mode-hook
             lisp-interaction-mode-hook
             clojure-mode-hook
             scheme-mode-hook
             lisp-mode-hook))
  (add-hook h #'lispy-mode))
(ivy-mode)

;;* Common Lisp
(require 'slime-autoloads)
(setq inferior-lisp-program "/usr/bin/sbcl")
(setq slime-contribs '(slime-fancy))

;;* Clojure
(require 'clojure-semantic nil t)
