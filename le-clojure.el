;;; le-clojure.el --- lispy support for Clojure. -*- lexical-binding: t -*-

;; Copyright (C) 2014-2015 Oleh Krehel

;; This file is not part of GNU Emacs

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;

;;; Code:

(require 'lispy)
(require 'cider-client nil t)
(require 'cider-interaction nil t)

(defun lispy-clojure-apropos ()
  (interactive)
  (let ((cands
         (split-string (lispy--eval-clojure
                        "(lispy-clojure/all-docs 'clojure.core)")
                       "::")))
    (ivy-read "var: " cands
              :action (lambda (s)
                        (lispy-message
                         (substring-no-properties
                          (replace-regexp-in-string
                           "\\\\n" "\n" s)) t)))))

(defun lispy-eval-clojure (&optional _plain)
  (let ((e-str (lispy--string-dwim))
        (c-str (let ((deactivate-mark nil))
                 (save-mark-and-excursion
                   (lispy--out-backward 1)
                   (deactivate-mark)
                   (lispy--string-dwim)))))
    (if (string= e-str c-str)
        (lispy--eval-clojure e-str e-str)
      (let ((f-str (format
                    "(lispy-clojure/reval %S %S :pretty-print %s)"
                    e-str
                    c-str
                    (if lispy-do-pprint "true" "false"))))
        (if (eq current-prefix-arg 7)
            (kill-new f-str)
          (lispy--eval-clojure f-str e-str))))))

(defvar lispy--clojure-hook-lambda nil
  "Store a lambda to call.")

(defun lispy--clojure-eval-hook-lambda ()
  "Call `lispy--clojure-hook-lambda'."
  (when lispy--clojure-hook-lambda
    (funcall lispy--clojure-hook-lambda)
    (setq lispy--clojure-hook-lambda nil))
  (remove-hook 'nrepl-connected-hook
               'lispy--clojure-eval-hook-lambda))

(defun lispy--clojure-pretty-string (str)
  "Return STR fontified in `clojure-mode'."
  (cond ((string-match "\\`\"error: \\([^\0]+\\)\"\\'" str)
         (concat (propertize "error: " 'face 'error)
                 (match-string 1 str)))
        ((> (length str) 4000)
         str)
        (t
         (condition-case nil
             (with-temp-buffer
               (clojure-mode)
               (insert str)
               (lispy-font-lock-ensure)
               (buffer-string))
           (error str)))))

(defun lispy--eval-nrepl-clojure (str namespace)
  (condition-case nil
      (with-no-warnings
        (nrepl-sync-request:eval
         str
         (cider-current-connection)
         (cider-current-session)
         namespace))
    (error
     (nrepl-sync-request:eval
      str
      (cider-current-connection)
      namespace))))

(defvar lispy--clojure-ns "user"
  "Store the last evaluated *ns*.")

(defun lispy--eval-clojure (str &optional add-output)
  "Eval STR as Clojure code.
The result is a string.

When ADD-OUTPUT is non-nil, add the standard output to the result."
  (require 'cider)
  (let (deactivate-mark)
    (if (null (cider-default-connection t))
        (progn
          (setq lispy--clojure-hook-lambda
                `(lambda ()
                   (set-window-configuration
                    ,(current-window-configuration))
                   (lispy--clojure-middleware-load)
                   (message
                    (lispy--eval-clojure-1 ,str ,add-output))))
          (add-hook 'nrepl-connected-hook
                    'lispy--clojure-eval-hook-lambda t)
          (cider-jack-in)
          "Starting CIDER...")
      (lispy--eval-clojure-1 str add-output))))

(defvar lispy--clojure-errorp nil)

(defun lispy--eval-clojure-1 (str add-output)
  (setq lispy--clojure-errorp nil)
  (or
   (and (stringp add-output)
        (lispy--eval-clojure-handle-ns add-output))
   (let* ((stra (if (string-match-p "\\`(lispy-clojure/reval" str)
                    str
                  (format "(do %s)" str)))
          (res (lispy--eval-nrepl-clojure
                stra
                lispy--clojure-ns))
          (status (nrepl-dict-get res "status"))
          (res (cond ((or (member "namespace-not-found" status))
                      (nrepl-sync-request:eval
                       stra
                       (cider-current-connection)
                       (cider-current-session)))
                     ((member "eval-error" status)
                      (setq lispy--clojure-errorp t)
                      res)
                     (t
                      res)))
          (val
           (nrepl-dict-get res "value"))
          out)
     (cond ((null val)
            (lispy--clojure-pretty-string
             (nrepl-dict-get res "err")))

           (add-output
            (concat
             (if (setq out (nrepl-dict-get res "out"))
                 (concat (propertize out 'face 'font-lock-string-face) "\n")
               "")
             (lispy--clojure-pretty-string val)))

           (lispy-do-pprint
            (read res))

           (t
            (lispy--clojure-pretty-string val))))))

(defun lispy--eval-clojure-handle-ns (str)
  (when (or (string-match "\\`(ns \\([a-z-_0-9\\.]+\\)" str)
            (string-match "\\`(in-ns '\\([a-z-_0-9\\.]+\\)" str))
    (setq lispy--clojure-ns (match-string 1 str))
    (lispy--eval-nrepl-clojure str "user")
    lispy--clojure-ns))

(defvar cider--debug-mode-response)
(declare-function cider--debug-mode "ext:cider-debug")
(defvar nrepl-ongoing-sync-request)

(defun lispy--clojure-debug-quit ()
  (let ((pt (save-excursion
              (if (lispy--leftp)
                  (forward-list)
                (lispy--out-forward 1))
              (lispy-up 1)
              (point)))
        (str (format "(do %s)"
                     (mapconcat
                      (lambda (x)
                        (format "(def %s %s)" (car x) (cadr x)))
                      (nrepl-dict-get cider--debug-mode-response "locals")
                      "\n"))))
    (nrepl-send-request
     (list "op" "debug-input" "input" ":quit"
           "key" (nrepl-dict-get cider--debug-mode-response "key"))
     (lambda (_response))
     (cider-current-connection))
    (lispy--eval-clojure str)
    (ignore-errors
      (let ((nrepl-ongoing-sync-request nil))
        (cider--debug-mode -1)))
    (goto-char pt)))

(defun lispy--clojure-resolve (symbol)
  "Return resolved SYMBOL.
Return 'special or 'keyword appropriately.
Otherwise try to resolve in current namespace first.
If it doesn't work, try to resolve in all available namespaces."
  (let ((str (lispy--eval-clojure
              (format "(lispy-clojure/resolve-sym '%s)" symbol))))
    (cond
      (lispy--clojure-errorp
       (user-error str))

      ((string-match "^#'\\(.*\\)$" str)
       (match-string 1 str))
      (t
       (read str)))))

(defun lispy--clojure-symbol-to-args (symbol)
  (cond ((string= symbol ".")
         (lispy--clojure-dot-args))
        ((string-match "\\`\\(.*\\)\\.\\'" symbol)
         (lispy--clojure-constructor-args (match-string 1 symbol)))
        (t
         (let ((sym (lispy--clojure-resolve symbol)))
           (cond
             ((eq sym 'special)
              (read
               (lispy--eval-clojure
                (format "(lispy-clojure/arglist '%s)" symbol))))
             ((eq sym 'keyword)
              (list "[map]"))
             ((eq sym 'undefined)
              (error "Undefined"))
             ((and (listp sym) (eq (car sym) 'variable))
              (list "variable"))
             ((null sym)
              (read
               (lispy--eval-clojure
                (format
                 "(let [[_ cname mname] (re-find #\"(.*)/(.*)\" \"%s\")
                           methods (and cname
                                     (try (load-string (format \"(.getMethods %%s)\" cname))
                                          (catch Exception e)))
                           methods (filter #(= (.getName %%) mname) methods)]
                       (if (= 0 (count methods))
                           \"method not found\"
                         (map (fn [m]
                                  (->> m
                                    .getParameterTypes
                                    (map #(.toString %%))
                                    (clojure.string/join \" \")))
                              (filter #(java.lang.reflect.Modifier/isStatic
                                        (.getModifiers %%))
                                      methods))))"
                 symbol))))
             (t
              (read
               (lispy--eval-clojure
                (format "(lispy-clojure/arglist '%s)" symbol)))))))))

(defun lispy--clojure-args (symbol)
  "Return a pretty string with arguments for SYMBOL.
Besides functions, handles specials, keywords, maps, vectors and sets."
  (let ((args (lispy--clojure-symbol-to-args symbol)))
    (if (listp args)
        (format
         "(%s %s)"
         (propertize symbol 'face 'lispy-face-hint)
         (mapconcat
          #'identity
          (mapcar (lambda (x) (propertize (downcase x)
                                          'face 'lispy-face-req-nosel))
                  args)
          (concat "\n"
                  (make-string (+ 2 (length symbol)) ?\ ))))
      (propertize args 'face 'lispy-face-hint))))

(defun lispy--describe-clojure-java (sym)
  "Return description for Clojure Java symol SYM."
  (read
   (lispy--eval-clojure
    (format
     "(let [[_ cname mname] (re-find #\"(.*)/(.*)\" \"%s\")
          methods (and cname
                    (try (load-string (format \"(.getMethods %%s)\" cname))
                         (catch Exception e)))
          methods (filter #(= (.getName %%) mname) methods)]
      (if (= 0 (count methods))
          nil
        (clojure.string/join
         \"\\n\" (map (fn [m] (.toString m))
                   methods))))"
     sym))))

(defun lispy--clojure-jump (symbol)
  "Jump to Clojure SYMBOL."
  (let* ((dict (nrepl-send-sync-request
                (list
                 "op" "info"
                 "session" (cider-current-session)
                 "ns" (cider-current-ns)
                 "symbol" symbol)
                (cider-current-connection)))
         (file (nrepl-dict-get dict "file"))
         (line (nrepl-dict-get dict "line"))
         (col (nrepl-dict-get dict "column")))
    (switch-to-buffer (cider-find-file file))
    (goto-char (point-min))
    (forward-line (1- line))
    (forward-char (1- col))))

(defun lispy--clojure-macrop (symbol)
  "Test if SYMBOL is a macro."
  (equal (lispy--eval-clojure
          (format "(:macro (meta #'%s))" symbol))
         "true"))

(defvar lispy--clojure-middleware-loaded-p nil
  "Nil if the Clojure middleware in \"lispy-clojure.clj\" wasn't loaded yet.")

(defun lispy--clojure-middleware-unload ()
  "Mark the Clojure middleware in \"lispy-clojure.clj\" as not loaded."
  (setq lispy--clojure-middleware-loaded-p nil)
  (setq lispy--clojure-ns "user"))

(defun lispy--clojure-middleware-load ()
  "Load the custom Clojure code in \"lispy-clojure.clj\"."
  (unless lispy--clojure-middleware-loaded-p
    (lispy--eval-clojure
     (format "(load-file \"%s\")"
             (expand-file-name "lispy-clojure.clj" lispy-site-directory)))
    (setq lispy--clojure-middleware-loaded-p t)
    (add-hook 'nrepl-disconnected-hook #'lispy--clojure-middleware-unload)))

(defun lispy-flatten--clojure (_arg)
  "Inline a Clojure function at the point of its call."
  (let* ((begp (if (looking-at lispy-left)
                   t
                 (if (lispy-right-p)
                     (progn (backward-list)
                            nil)
                   (lispy-left 1))))
         (bnd (lispy--bounds-list))
         (str (lispy--string-dwim bnd))
         (expr (lispy--read str))
         (result
          (if (and (symbolp (car expr))
                   (lispy--clojure-macrop (symbol-name (car expr))))
              (lispy--eval-clojure
               (format "(macroexpand '%s)" str))
            (lispy--eval-clojure
             (format "(lispy-clojure/flatten-expr '%s)" str)))))
    (goto-char (car bnd))
    (delete-region (car bnd) (cdr bnd))
    (insert result)
    (when begp
      (goto-char (car bnd))))
  (lispy-alt-multiline))

(defun lispy--clojure-debug-step-in ()
  "Inline a Clojure function at the point of its call."
  (let* ((e-str (format "(lispy-clojure/debug-step-in\n'%s)"
                        (lispy--string-dwim)))
         (str (substring-no-properties
               (lispy--eval-clojure e-str))))
    (lispy-follow)
    (lispy--eval-clojure str)
    (lispy-flow 1)))

(defun lispy-goto-symbol-clojure (symbol)
  "Goto SYMBOL."
  (let ((rsymbol (lispy--clojure-resolve symbol)))
    (cond ((stringp rsymbol)
           (lispy--clojure-jump rsymbol))
          ((eq rsymbol 'special)
           (error "Can't jump to '%s because it's special" symbol))
          ((eq rsymbol 'keyword)
           (error "Can't jump to keywords"))
          ((and (listp rsymbol)
                (eq (car rsymbol) 'variable))
           (error "Can't jump to Java variables"))
          (t
           (error "Could't resolve '%s" symbol))))
  (lispy--back-to-paren))

(defun lispy-goto-symbol-clojurescript (symbol)
  "Goto SYMBOL."
  (cider-find-var nil symbol))

(defun lispy-clojure-complete-at-point ()
  (let* ((lispy-ignore-whitespace t)
         (bnd (or (bounds-of-thing-at-point 'symbol)
                  (cons (point) (point))))
         (obj (cond
                ((save-excursion
                   (lispy--out-backward 1)
                   (looking-at "(\\.\\."))
                 (concat
                  (buffer-substring-no-properties (match-beginning 0) (car bnd))
                  ")"))
                ((save-excursion
                   (lispy--back-to-paren)
                   (when (looking-at "(\\.[\t\n ]")
                     (forward-char 1)
                     (forward-sexp 2)
                     (lispy--string-dwim)))))))
    (when obj
      (let ((cands (read (lispy--eval-clojure
                          (format "(lispy-clojure/object-members %s)" obj)))))
        (when (> (cdr bnd) (car bnd))
          (setq cands (all-completions (lispy--string-dwim bnd) cands)))
        (list (car bnd) (cdr bnd) cands)))))

(defun lispy--clojure-dot-args ()
  (save-excursion
    (lispy--back-to-paren)
    (let* ((object (save-mark-and-excursion
                     (lispy-mark-list 2)
                     (lispy--string-dwim)))
           (method (save-mark-and-excursion
                     (lispy-mark-list 3)
                     (lispy--string-dwim)))
           (sig (read
                 (lispy--eval-clojure
                  (format "(lispy-clojure/method-signature %s \"%s\")" object method)))))
      (when (> (length sig) 0)
        (if (string-match "\\`public \\(.*\\)(\\(.*\\))\\'" sig)
            (let ((name (match-string 1 sig))
                  (args (match-string 2 sig)))
              (format "%s\n(. %s %s%s)"
                      name object method
                      (if (> (length args) 0)
                          (concat " " args)
                        "")))
          sig)))))

(defun lispy--clojure-constructor-args (symbol)
  (read (lispy--eval-clojure
         (format "(lispy-clojure/ctor-args %s)" symbol))))

(provide 'le-clojure)

;;; le-clojure.el ends here
