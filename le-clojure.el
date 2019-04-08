;;; le-clojure.el --- lispy support for Clojure. -*- lexical-binding: t -*-

;; Copyright (C) 2014-2018 Oleh Krehel

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

;;* Requires
(require 'lispy)
(require 'cider-client nil t)
(require 'cider-connection nil t)
(require 'cider-eval nil t)
(require 'cider-find nil t)

(defcustom lispy-clojure-eval-method 'cider
  "REPL used for eval."
  :type '(choice
          (const :tag "CIDER" cider)
          (const :tag "UNREPL" spiral))
  :group 'lispy)

(defcustom lispy-cider-connect-method 'cider-jack-in
  "Function used to create a CIDER connection."
  :type '(choice
          (const cider-jack-in)
          (const cider-connect)
          (function :tag "Custom"))
  :group 'lispy)

;;* Namespace
(defvar lispy--clojure-ns "user"
  "Store the last evaluated *ns*.")

(defun lispy--clojure-detect-ns ()
  "When there's only one (ns ...) in the buffer, use it."
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward clojure-namespace-name-regex nil t)
      (let ((ns (match-string-no-properties 4)))
        (when (not (re-search-forward clojure-namespace-name-regex nil t))
          (setq lispy--clojure-ns ns))))))

;;* User wrapper for eval
(defvar lispy--clojure-middleware-loaded-p nil
  "Nil if the Clojure middleware in \"lispy-clojure.clj\" wasn't loaded yet.")

(defun lispy-eval-clojure (e-str)
  "User facing eval."
  (lispy--clojure-detect-ns)
  (let (c-str)
    (unless (stringp e-str)
      (setq e-str (lispy--string-dwim))
      (setq c-str (let ((deactivate-mark nil)
                        (lispy-ignore-whitespace t))
                    (save-mark-and-excursion
                      (lispy--out-backward 1)
                      (deactivate-mark)
                      (lispy--string-dwim)))))
    (let ((f-str
           (cond
             ((eq major-mode 'clojurescript-mode)
              e-str)
             (lispy--clojure-middleware-loaded-p
              (format (if (eq this-command 'special-lispy-eval)
                          "(lispy-clojure/pp (lispy-clojure/reval %S %S :file %S :line %S))"
                        "(lispy-clojure/reval %S %S :file %S :line %S)")
                      e-str c-str (buffer-file-name) (line-number-at-pos)))
             (t
              e-str))))
      (cond ((eq current-prefix-arg 7)
             (kill-new f-str))
            ((and (eq current-prefix-arg 0)
                  (lispy--eval-clojure
                   "(lispy-clojure/shadow-unmap *ns*)")
                  nil))
            ((eq lispy-clojure-eval-method 'spiral)
             (lispy--eval-clojure-spiral e-str))
            (t
             (lispy--eval-clojure f-str e-str))))))

;;* Start REPL wrapper for eval
(defvar lispy--clojure-hook-lambda nil
  "Store a lambda to call.")

(defun lispy--clojure-eval-hook-lambda ()
  "Call `lispy--clojure-hook-lambda'."
  (when lispy--clojure-hook-lambda
    (funcall lispy--clojure-hook-lambda)
    (setq lispy--clojure-hook-lambda nil))
  (remove-hook 'nrepl-connected-hook
               'lispy--clojure-eval-hook-lambda))

(eval-after-load 'cider
  '(progn
     (cider-add-to-alist 'cider-jack-in-dependencies
                         "org.tcrawley/dynapath" "0.2.5")
     (cider-add-to-alist 'cider-jack-in-dependencies
                         "com.cemerick/pomegranate" "0.4.0")
     (cider-add-to-alist 'cider-jack-in-dependencies
                         "compliment" "0.3.6")
     (cider-add-to-alist 'cider-jack-in-dependencies
                         "me.raynes/fs" "1.4.6")))

(declare-function cider-connections "ext:cider-connection")

(defun lispy--eval-clojure (str &optional add-output)
  "Eval STR as Clojure code.
The result is a string.

When ADD-OUTPUT is non-nil, add the standard output to the result."
  (require 'cider)
  (unless (eq major-mode 'clojurescript-mode)
    (add-hook 'cider-connected-hook #'lispy--clojure-middleware-load))
  (let (deactivate-mark)
    (if (null (car (cider-connections)))
        (progn
          (setq lispy--clojure-hook-lambda
                `(lambda ()
                   (set-window-configuration
                    ,(current-window-configuration))
                   (message
                    (lispy--eval-clojure-1 ,str ,add-output))))
          (add-hook 'nrepl-connected-hook
                    'lispy--clojure-eval-hook-lambda t)
          (call-interactively lispy-cider-connect-method)
          (format "Starting CIDER using %s ..." lispy-cider-connect-method))
      (unless lispy--clojure-middleware-loaded-p
        (lispy--clojure-middleware-load))
      (lispy--eval-clojure-1 str add-output))))

;;* Base eval
(defvar lispy--clojure-errorp nil)
(defun lispy--eval-clojure-1 (str add-output)
  (setq lispy--clojure-errorp nil)
  (or
   (and (stringp add-output)
        (lispy--eval-clojure-handle-ns add-output))
   (let* (pp
          (stra (if (setq pp (string-match "\\`(lispy-clojure/\\(pp\\|reval\\)" str))
                    str
                  (format "(do %s)" str)))
          (res (lispy--eval-nrepl-clojure stra lispy--clojure-ns))
          (status (nrepl-dict-get res "status"))
          (res (cond ((or (member "namespace-not-found" status))
                      (lispy--eval-nrepl-clojure stra))
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
             (lispy--clojure-pretty-string
              (if pp
                  (condition-case nil
                      (string-trim (read val))
                    (error val))
                val))))

           (t
            (lispy--clojure-pretty-string val))))))

(defun lispy--eval-clojure-handle-ns (str)
  (when (or (string-match "\\`(ns \\([a-z-_0-9\\.]+\\)" str)
            (string-match "\\`(in-ns '\\([a-z-_0-9\\.]+\\)" str))
    (setq lispy--clojure-ns (match-string 1 str))
    (let* ((res (lispy--eval-nrepl-clojure str "user"))
           (status (nrepl-dict-get res "status")))
      (when (member "eval-error" status)
        (error (nrepl-dict-get res "err"))))
    lispy--clojure-ns))

;;* Handle NREPL version incompat
(defun lispy--eval-nrepl-clojure (str &optional namespace)
  (nrepl-sync-request:eval
   str
   (cider-current-connection)
   namespace))

(defvar spiral-conn-id)
(defvar spiral-aux-sync-request-timeout)
(declare-function spiral-projects-as-list "ext:spiral-project")
(declare-function spiral-pending-eval-add "ext:spiral-project")
(declare-function spiral-ast-unparse-to-string "ext:spiral-ast")
(declare-function spiral-loop--send "ext:spiral-loop")

(defun lispy--eval-clojure-spiral (str)
  (let* ((start (current-time))
         (repl-buf (cdr (assoc :repl-buffer (car (spiral-projects-as-list)))))
         (conn-id (with-current-buffer repl-buf spiral-conn-id))
         (unparse-no-properties
          (lambda (node) (substring-no-properties
                     (spiral-ast-unparse-to-string node))))
         stdout
         result)
    (spiral-loop--send conn-id :aux str)
    (spiral-pending-eval-add
     :aux conn-id
     :status :sent
     :eval-callback (lambda (eval-payload)
                      (setq result (funcall unparse-no-properties eval-payload)))
     :stdout-callback (lambda (stdout-payload &rest _)
                        (setq stdout
                              (concat stdout
                                      (funcall unparse-no-properties stdout-payload)))))
    (while (and (not result)
                (not (input-pending-p)) ;; do not hang UI
                (or (not spiral-aux-sync-request-timeout)
                    (< (cadr (time-subtract (current-time) start))
                       spiral-aux-sync-request-timeout)))
      (accept-process-output nil 0.01))
    (if stdout
        (concat stdout "\n" result)
      result)))

;;* Rest
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

(defun lispy--clojure-macrop (symbol)
  "Test if SYMBOL is a macro."
  (equal (lispy--eval-clojure
          (format "(:macro (meta #'%s))" symbol))
         "true"))

(defun lispy--clojure-middleware-unload ()
  "Mark the Clojure middleware in \"lispy-clojure.clj\" as not loaded."
  (setq lispy--clojure-middleware-loaded-p nil))

(defvar cider-jdk-src-paths)

(defun lispy-cider-load-file (filename)
  (let ((ns-form  (cider-ns-form)))
    (cider-map-repls :auto
      (lambda (connection)
        (when ns-form
          (cider-repl--cache-ns-form ns-form connection))
        (cider-request:load-file (cider--file-string filename)
                                 (funcall cider-to-nrepl-filename-function
                                          (cider--server-filename filename))
                                 (file-name-nondirectory filename)
                                 connection)))))

(defun lispy--clojure-middleware-load ()
  "Load the custom Clojure code in \"lispy-clojure.clj\"."
  (unless lispy--clojure-middleware-loaded-p
    (setq lispy--clojure-ns "user")
    (save-window-excursion
      (lispy-cider-load-file
       (expand-file-name "lispy-clojure.clj" lispy-site-directory)))
    (setq lispy--clojure-middleware-loaded-p t)
    (add-hook 'nrepl-disconnected-hook #'lispy--clojure-middleware-unload)
    (let ((sources-expr
           (format
            "(do \n  %s)"
            (mapconcat
             (lambda (p) (format "(cemerick.pomegranate/add-classpath %S)" p))
             cider-jdk-src-paths
             "\n  "))))
      (lispy--eval-clojure sources-expr))))

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
  (lispy--clojure-detect-ns)
  (let* ((e-str (format "(lispy-clojure/debug-step-in\n'%s)"
                        (lispy--string-dwim)))
         (str (substring-no-properties
               (lispy--eval-clojure e-str))))
    (lispy-follow)
    (when (string-match "(clojure.core/in-ns (quote \\([^)]+\\))" str)
      (setq lispy--clojure-ns (match-string 1 str)))
    (lispy--eval-clojure str)
    (lispy-flow 1)))

(defun lispy-goto-line (line)
  (goto-char (point-min))
  (forward-line (1- line)))

(declare-function archive-zip-extract "arc-mode")

(defun lispy-find-archive (archive path)
  (require 'arc-mode)
  (let ((name (format "%s:%s" archive path)))
    (switch-to-buffer
     (or (find-buffer-visiting name)
         (with-current-buffer (generate-new-buffer name)
           (archive-zip-extract archive path)
           (set-visited-file-name name)
           (setq-local default-directory (file-name-directory archive))
           (setq-local buffer-read-only t)
           (set-buffer-modified-p nil)
           (set-auto-mode)
           (current-buffer))))))

(defun lispy-goto-symbol-clojure (symbol)
  "Goto SYMBOL."
  (lispy--clojure-detect-ns)
  (let* ((r (read (lispy--eval-clojure
                   (format "(lispy-clojure/location '%s)" symbol))))
         (url (car r))
         (line (cadr r))
         archive)
    (cond
      ((file-exists-p url)
       (find-file url)
       (lispy-goto-line line))
      ((and (string-match "\\`file:\\([^!]+\\)!/\\(.*\\)\\'" url)
            (file-exists-p (setq archive (match-string 1 url))))
       (let ((path (match-string 2 url)))
         (lispy-find-archive archive path)
         (lispy-goto-line line)))
      (t
       (warn "unexpected: %S" symbol)
       (cider-find-var symbol)))))

(defun lispy-goto-symbol-clojurescript (symbol)
  "Goto SYMBOL."
  (cider-find-var nil symbol))

(defun lispy-clojure-complete-at-point ()
  (when (car (cider-connections))
    (ignore-errors
      (lispy--clojure-detect-ns)
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
                         (ignore-errors
                           (forward-char 1)
                           (forward-sexp 2)
                           (lispy--string-dwim)))))))
             res)
        (cond ((and obj
                    (setq res (lispy--eval-clojure
                               (format "(lispy-clojure/object-members %s)" obj)))
                    (null lispy--clojure-errorp))
               (let ((cands (read res)))
                 (when (> (cdr bnd) (car bnd))
                   (setq cands (all-completions (lispy--string-dwim bnd) cands)))
                 (list (car bnd) (cdr bnd) cands)))
              ((save-excursion
                 (lispy--out-backward 2)
                 (looking-at "(import"))
               (let* ((prefix (save-excursion
                                (lispy--out-backward 1)
                                (forward-char)
                                (thing-at-point 'symbol t)))
                      (cands (read (lispy--eval-clojure
                                    (format
                                     "(lispy-clojure/complete %S)"
                                     prefix))))
                      (len (1+ (length prefix)))
                      (candsa (mapcar (lambda (s) (substring s len)) cands)))
                 (when (> (cdr bnd) (car bnd))
                   (setq candsa (all-completions (lispy--string-dwim bnd) candsa)))
                 (list (car bnd) (cdr bnd) candsa)))
              (t
               (let* ((prefix (lispy--string-dwim bnd))
                      (cands (read (lispy--eval-clojure
                                    (format
                                     "(lispy-clojure/complete %S)"
                                     prefix)))))
                 (list (car bnd) (cdr bnd) cands))))))))

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
                  (format "(lispy-clojure/method-signature (lispy-clojure/reval \"%s\" nil) \"%s\")" object method)))))
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

(defun lispy-clojure-apropos-action (s)
  (cider-doc-lookup
   (substring
    (car (split-string s "\\\\n"))
    2)))

(defun lispy-clojure-apropos ()
  (interactive)
  (let ((cands
         (split-string (lispy--eval-clojure
                        "(lispy-clojure/all-docs 'clojure.core)")
                       "::")))
    (ivy-read "var: " cands
              :action #'lispy-clojure-apropos-action)))

(provide 'le-clojure)

;;; le-clojure.el ends here
