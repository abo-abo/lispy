(require 'geiser-eval)

(defun lispy--eval-scheme (str)
  "Eval STR as Scheme code."
  (unless (geiser-repl--connection*)
    (save-window-excursion
      (if geiser-impl--implementation
          (run-geiser geiser-impl--implementation)
        (call-interactively 'run-geiser))))
  (let* ((code `(:eval (:scm ,str)))
         (ret (geiser-eval--send/wait code))
         (err (geiser-eval--retort-error ret)))
    (if err
        (format "Error: %s" (s-trim (cdr (assoc 'output ret))))
      (format "%s" (cadr (assoc 'result ret))))))

(provide 'le-scheme)
