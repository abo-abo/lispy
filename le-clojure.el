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

(defun lispy--clojure-lax (str)
  "Possibly transform STR into a more convenient Clojure expression."
  (let ((expr (lispy--read str)))
    (if (and expr
             (symbolp expr)
             (< (length (symbol-name expr))
                (- (length str) 3)))
        (setq str (format "(do (def %s) %s)" str str))
      str)))

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
  (with-temp-buffer
    (clojure-mode)
    (insert str)
    (lispy-font-lock-ensure)
    (buffer-string)))

(defun lispy--eval-clojure (str &optional add-output lax)
  "Eval STR as Clojure code.
The result is a string.

When ADD-OUTPUT is non-nil, add the standard output to the result.

When LAX is non-nil, expect STR to be two sexps from a let binding.
Generate an appropriate def from for that let binding and eval it."
  (require 'cider)
  (let (deactivate-mark)
    (if (null (cider-default-connection t))
        (progn
          (setq lispy--clojure-hook-lambda
                `(lambda ()
                   (set-window-configuration
                    ,(current-window-configuration))
                   (message
                    (lispy--eval-clojure ,str ,add-output ,lax))))
          (add-hook 'nrepl-connected-hook
                    'lispy--clojure-eval-hook-lambda t)
          (cider-jack-in)
          "Starting CIDER...")
      (when lax
        (setq str (lispy--clojure-lax str)))
      (let* ((str
              (if lispy-do-pprint
                  (format "(clojure.core/let [x %s] (with-out-str (clojure.pprint/pprint x)))"
                          str)
                str))
             (res (nrepl-sync-request:eval
                   str
                   (cider-current-connection)
                   (cider-current-session)
                   (if lax
                       "user"
                     (cider-current-ns))))
             (status (nrepl-dict-get res "status"))
             (res (if (or (member "namespace-not-found" status)
                          (member "eval-error" status))
                      (nrepl-sync-request:eval
                       str
                       (cider-current-connection)
                       (cider-current-session))
                    res))
             (val (nrepl-dict-get res "value"))
             out)
        (cond ((null val)
               (error "Eval error: %S"
                      (nrepl-dict-get res "err")))

              (add-output
               (if (setq out (nrepl-dict-get res "out"))
                   (format "%s\n%s"
                           (propertize
                            out 'face 'font-lock-string-face)
                           val)
                 (lispy--clojure-pretty-string val)))

              (lispy-do-pprint
               (read res))

              (t
               (lispy--clojure-pretty-string val)))))))

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
              (format
               "(if (symbol? '%s)
                   (if (special-symbol? '%s)
                       'special
                     (or (resolve '%s)
                         (first (keep #(ns-resolve %% '%s) (all-ns)))
                         (if-let [val (try (load-string \"%s\") (catch Exception e))]
                                 (list 'variable (str val)))))
                 (if (keyword? '%s)
                     'keyword
                   'unknown))"
               symbol
               symbol
               symbol
               symbol
               symbol
               symbol))))
    (if (string-match "^#'\\(.*\\)$" str)
        (match-string 1 str)
      (read str))))

(defun lispy--clojure-args (symbol)
  "Return a pretty string with arguments for SYMBOL.
Besides functions, handles specials, keywords, maps, vectors and sets."
  (let* ((sym (lispy--clojure-resolve symbol))
         (args (cond
                 ((eq sym 'special)
                  (read
                   (lispy--eval-clojure
                    (format
                     "(->> (with-out-str (clojure.repl/doc %s))
                       (re-find #\"\\(.*\\)\")
                       read-string rest
                       (map str)
                       (clojure.string/join \" \")
                       (format \"[%%s]\")
                       list)"
                     symbol))))
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
                  (read (lispy--eval-clojure
                         (format
                          "(let [args (map str (:arglists (meta #'%s)))]
                            (if (empty? args)
                                (eval '(list
                                        (condp #(%%1 %%2) %s
                                         map? \"[key]\"
                                         set? \"[key]\"
                                         vector? \"[idx]\"
                                         \"is uncallable\")))
                              args))"
                          sym
                          sym)))))))
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
  (setq lispy--clojure-middleware-loaded-p nil))

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
            (lispy--clojure-middleware-load)
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
  (lispy--clojure-middleware-load)
  (let* ((str (lispy--eval-clojure
               (format "(lispy-clojure/debug-step-in '%s)"
                       (lispy--string-dwim))))
         (expr (lispy--read str))
         (n-args (1- (length expr))))
    (lispy-follow)
    (lispy--eval-clojure str)
    (forward-char 1)
    (forward-sexp 2)
    (when (looking-at "[ \t\n]*\"")
      (goto-char (1- (match-end 0)))
      (forward-sexp 1))
    (if (looking-at "[ \t\n]*\\[")
        (progn
          (goto-char (1- (match-end 0)))
          (lispy-flow 1))
      (lispy-forward 1)
      (lispy-backward 1)
      (lispy-flow 1)
      (while (/= n-args (length (lispy--read (lispy--string-dwim))))
        (lispy--out-backward 1)
        (lispy-down 1)
        (lispy-flow 1))
      (lispy-down 1))))

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

(provide 'le-clojure)

;;; le-clojure.el ends here
