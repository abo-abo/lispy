(require 'slime)

(defun lispy--eval-lisp (str)
  "Eval STR as Common Lisp code."
  (unless (slime-current-connection)
    (slime))
  (let (deactivate-mark)
    (cadr (slime-eval `(swank:eval-and-grab-output ,str)))))

(provide 'le-lisp)
