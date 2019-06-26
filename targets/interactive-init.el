;;* Base
(require 'lispy)
(dolist (h '(emacs-lisp-mode-hook
             lisp-interaction-mode-hook
             clojure-mode-hook
             scheme-mode-hook
             lisp-mode-hook))
  (add-hook h #'lispy-mode))
(ivy-mode)
(defadvice save-buffers-kill-emacs (around no-query-kill-emacs activate)
  "Prevent annoying \"Active processes exist\" query when you quit Emacs."
  (lispy-flet (process-list ()) ad-do-it))

;;* Common Lisp
(setq inferior-lisp-program "sbcl")
(setq slime-contribs '(slime-fancy))
(setq lispy-use-sly nil)
;; SLIME and SLY modify this hook even before they're required. Yuck.
(setq lisp-mode-hook '(lispy-mode))

;;* Clojure
(require 'clojure-semantic nil t)
