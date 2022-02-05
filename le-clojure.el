;;; le-clojure.el --- lispy support for Clojure. -*- lexical-binding: t -*-

;; Copyright (C) 2014-2019 Oleh Krehel

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
(require 'cider-client nil t)
(require 'cider-connection nil t)
(require 'cider-eval nil t)
(require 'cider-find nil t)
(require 'cider-debug nil t)

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

(defvar lispy--clojure-namespace-name-regex
  "^(\\(clojure.core/\\)?\\(in-\\)?ns\\+?[
[:space:]]+\\(?:\\(?:\\(#?\\^{[^}]*}\\)\\|\\(?:\\^:[^[:space:]]+\\)*\\)[
[:space:]]+\\)*[':]?\\([^\"()[:space:]]+\\_>\\)"
  "Store the obsoleted `clojure-namespace-name-regex'.")

(defun lispy--clojure-detect-ns ()
  "When there's only one (ns ...) in the buffer, use it."
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward lispy--clojure-namespace-name-regex nil t)
      (let ((ns (match-string-no-properties 4)))
        (when (not (re-search-forward lispy--clojure-namespace-name-regex nil t))
          (setq lispy--clojure-ns ns))))))

;;* User wrapper for eval
(defvar lispy--clojure-middleware-loaded-hash (make-hash-table :test #'equal)
  "Nil if the Clojure middleware in \"lispy-clojure.clj\" wasn't loaded yet.")

(defun lispy--clojure-process-buffer ()
  (if (or org-src-mode (eq major-mode 'org-mode))
      (cadr (first (sesman--all-system-sessions 'CIDER)))
    (let ((cur-type (cider-repl-type-for-buffer)))
      (car (cider-repls cur-type nil)))))

(defun lispy--clojure-middleware-loaded-p ()
  (let ((conn (lispy--clojure-process-buffer)))
    (and conn (gethash conn lispy--clojure-middleware-loaded-hash))))

(defun lispy--clojure-babashka-p ()
  (ignore-errors (cider--babashka-version)))

(defun lispy--eval-clojure-context (e-str)
  (cond
   ((or (eq major-mode 'clojurescript-mode)
        (lispy--clojure-babashka-p))
    e-str)
   ((string-match-p "#break" e-str)
    e-str)
   ((lispy--clojure-middleware-loaded-p)
    (let ((context-str
           (condition-case nil
               (let ((deactivate-mark nil))
                 (save-mark-and-excursion
                   (lispy--out-backward 1 t)
                   (deactivate-mark)
                   (lispy--string-dwim)))
             (error ""))))
      (when (and (lispy--leftp)
                 (looking-back "(for[ \t\n]*" (line-beginning-position -1)))
        (let* ((e-str-1 (save-excursion
                          (forward-char 1)
                          (forward-sexp 2)
                          (lispy--string-dwim)))
               (coll (read (lispy--eval-clojure-1
                            (format "(map str %s)" e-str-1)
                            nil)))
               (idx (lispy--idx-from-list coll))
               (sym (save-excursion
                      (forward-char 1)
                      (lispy--string-dwim))))
          (setq e-str (format "%s (nth %s %d)" sym e-str-1 idx))))
      (format (if (memq this-command '(special-lispy-eval
                                       special-lispy-eval-and-insert
                                       lispy-eval-current-outline))
                  "(lispy.clojure/pp (lispy.clojure/reval %S %S :file %S :line %S))"
                "(lispy.clojure/reval %S %S :file %S :line %S)")
              e-str
              context-str
              (buffer-file-name)
              (line-number-at-pos))))
   (t
    e-str)))

(defun lispy-eval-clojure (str)
  "Eval STR as a Clojure expression."
  (lispy--clojure-detect-ns)
  (if (eq lispy-clojure-eval-method 'spiral)
      (lispy--eval-clojure-spiral str)
    (lispy--eval-clojure-cider str)))

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

(defvar lispy-cider-jack-in-dependencies nil)

(defvar cider-jack-in-cljs-dependencies)
(defvar cider-jack-in-dependencies)

(declare-function cider-connections "ext:cider-connection")
(defvar cider-allow-jack-in-without-project)

(defvar lispy-clojure-projects-alist nil
  "Use `cider-connect' instead of `cider-jack-in' for some projects.
Each entry is (DIRECTORY :host HOSTNAME :port PORT).
Example: '((\"~/git/luminous-1\" :host \"localhost\" :port 7000))")

(defun lispy--clojure-middleware-load-hook ()
  "Don't load the middleware too early for a ClojureScript REPL.
It will cause an error, since before the init finishes it's a Clojure REPL."
  (unless (eq (lispy--clojure-process-type) 'cljs)
    (lispy--clojure-middleware-load)))

(defun lispy--eval-clojure-cider (e-str)
  "Eval STR as Clojure code and return a string.
Add the standard output to the result."
  (require 'cider)
  (let ((f-str (lispy--eval-clojure-context e-str))
        deactivate-mark)
    (cond ((null (lispy--clojure-process-buffer))
           (unless (eq major-mode 'clojurescript-mode)
             (setq lispy--clojure-hook-lambda
                   `(lambda ()
                      (set-window-configuration
                       ,(current-window-configuration))
                      (lispy--clojure-middleware-load)
                      (lispy-message
                       (lispy--eval-clojure-1 ,f-str ,e-str))))
             (add-hook 'nrepl-connected-hook
                       'lispy--clojure-eval-hook-lambda t))
           (let ((project-cfg (assoc (clojure-project-dir (cider-current-dir))
                                     lispy-clojure-projects-alist)))
             (cond (project-cfg
                    (cider-connect (cons :project-dir project-cfg))
                    "Using cider-connect")
                   ((eq major-mode 'clojurescript-mode)
                    (let ((cider-jack-in-cljs-dependencies nil))
                      (call-interactively #'cider-jack-in-cljs))
                    "Starting CIDER using cider-jack-in-cljs ...")
                   (t
                    (let ((cider-allow-jack-in-without-project t)
                          (cider-jack-in-dependencies
                           (delete-dups
                            (append
                             cider-jack-in-dependencies
                             (and (eq major-mode 'clojure-mode)
                                  lispy-cider-jack-in-dependencies)))))
                      (call-interactively lispy-cider-connect-method))
                    (format "Starting CIDER using %s ..." lispy-cider-connect-method)))))
          ((eq current-prefix-arg 7)
           (kill-new f-str))
          ((and (eq current-prefix-arg 0)
                (lispy--eval-clojure-cider
                 "(lispy.clojure/shadow-unmap *ns*)")
                nil))
          (t
           (lispy--clojure-middleware-load)
           (lispy--eval-clojure-1 f-str e-str)))))

;;* Base eval
(defun lispy--eval-clojure-1 (f-str e-str)
  (or
   (and (stringp e-str)
        (lispy--eval-clojure-handle-ns e-str))
   (let* ((res (lispy--eval-nrepl-clojure f-str lispy--clojure-ns))
          (status (nrepl-dict-get res "status"))
          (res (cond ((or (member "namespace-not-found" status))
                      (lispy--eval-nrepl-clojure f-str))
                     ((member "eval-error" status)
                      (signal 'eval-error (lispy--clojure-pretty-string
                                           (nrepl-dict-get res "err"))))
                     (t
                      res)))
          (val
           (nrepl-dict-get res "value"))
          (out (nrepl-dict-get res "out")))
     (when out
       (setq lispy-eval-output
             (concat (propertize out 'face 'font-lock-string-face) "\n")))
     (if (string-match "\\`(lispy.clojure/\\(pp\\|reval\\)" f-str)
         (condition-case nil
             (string-trim (read val))
           (error val))
       (if (stringp val)
           (string-trim val))))))

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
   (or (cider-current-connection)
       (lispy--clojure-process-buffer))
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
(defun lispy--clojure-debug-quit ()
  (interactive)
  (let ((pt (save-excursion
              (if (lispy--leftp)
                  (forward-list)
                (lispy--out-forward 1))
              (lispy-up 1)
              (lispy-different)
              (point)))
        (str (format "(do %s)"
                     (mapconcat
                      (lambda (x)
                        (format "(lispy.clojure/shadow-def '%s %s)" (car x) (cadr x)))
                      (nrepl-dict-get cider--debug-mode-response "locals")
                      "\n"))))
    (catch 'exit
      (cider-debug-mode-send-reply ":quit"))
    (lispy--eval-clojure-1 str nil)
    (goto-char pt)))

(when (boundp 'cider--debug-mode-map)
  (define-key cider--debug-mode-map "Z" 'lispy--clojure-debug-quit))

(defun lispy--clojure-resolve (symbol)
  "Return resolved SYMBOL.
Return 'special or 'keyword appropriately.
Otherwise try to resolve in current namespace first.
If it doesn't work, try to resolve in all available namespaces."
  (let ((str (lispy--eval-clojure-cider
              (format "(lispy.clojure/resolve-sym '%s)" symbol))))
    (cond
      ((string-match "^#'\\(.*\\)$" str)
       (match-string 1 str))
      (t
       (read str)))))

(defun lispy--clojure-symbol-to-args (symbol)
  (cond
    ((eq major-mode 'clojurescript-mode)
     (let (info)
       (and (cider-nrepl-op-supported-p "info")
            (setq info (cider-sync-request:info symbol))
            (let ((args (nrepl-dict-get info "arglists-str")))
              (if args
                  (split-string args "\n")
                (nrepl-dict-get info "forms-str"))))))
    ((string= symbol ".")
     (lispy--clojure-dot-args))
    ((string-match "\\`\\(.*\\)\\.\\'" symbol)
     (lispy--clojure-constructor-args (match-string 1 symbol)))
    (t
     (let ((sym (lispy--clojure-resolve symbol)))
       (cond
         ((eq sym 'special)
          (read
           (lispy--eval-clojure-cider
            (format "(lispy.clojure/arglist '%s)" symbol))))
         ((eq sym 'keyword)
          (list "[map]"))
         ((eq sym 'undefined)
          (error "Undefined"))
         ((and (listp sym) (eq (car sym) 'variable))
          (list "variable"))
         (t
          (read
           (lispy--eval-clojure-cider
            (format "(lispy.clojure/arglist '%s)" symbol)))))))))

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
   (lispy--eval-clojure-cider
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
  (equal (lispy--eval-clojure-cider
          (format "(:macro (meta #'%s))" symbol))
         "true"))

(defun lispy--clojure-middleware-unload ()
  "Mark the Clojure middleware in \"lispy-clojure.clj\" as not loaded."
  (puthash (lispy--clojure-process-buffer) nil lispy--clojure-middleware-loaded-hash))

(defun lispy-cider-load-file (filename)
  (let ((ns-form (cider-ns-form)))
    (cider-map-repls :auto
      (lambda (connection)
        (when ns-form
          (cider-repl--cache-ns-form ns-form connection))
        (cider-request:load-file (cider--file-string filename)
                                 (funcall cider-to-nrepl-filename-function
                                          (cider--server-filename filename))
                                 (file-name-nondirectory filename)
                                 connection)))))

(defcustom lispy-clojure-middleware-tests nil
  "When non-nil, run the tests from lispy-clojure.clj when loading it."
  :type 'boolean
  :group 'lispy)

(defun lispy--clojure-process-type (&optional conn)
  (let ((conn (or conn (lispy--clojure-process-buffer))))
    (if (string-match "(.*cljs" (buffer-name conn))
        'cljs
      'clj)))

(defun lispy--clojure-middleware-load ()
  "Load the custom Clojure code in \"lispy-clojure.clj\"."
  (let* ((access-time (lispy--clojure-middleware-loaded-p))
         (conn (lispy--clojure-process-buffer))
         (conn-type (lispy--clojure-process-type conn))
         (middleware-fname
          (expand-file-name
           (if (eq conn-type 'cljs) "lispy-clojure.cljs" "lispy-clojure.clj")
           lispy-site-directory))
         (middleware-access-time (file-attribute-access-time
                                  (file-attributes middleware-fname))))
    (when (or (null access-time) (time-less-p access-time middleware-access-time))
      (setq lispy--clojure-ns "user")
      (unless (lispy--clojure-babashka-p)
        (save-window-excursion
          (lispy-cider-load-file
           (expand-file-name middleware-fname lispy-site-directory))))
      (puthash conn middleware-access-time lispy--clojure-middleware-loaded-hash)
      (add-hook 'nrepl-disconnected-hook #'lispy--clojure-middleware-unload)
      (when (equal conn-type 'clj)
        (let ((test-fname (expand-file-name "lispy-clojure-test.clj"
                                            lispy-site-directory)))
          (when (and lispy-clojure-middleware-tests
                     (file-exists-p test-fname))
            (lispy-message
             (lispy--eval-clojure-cider (format "(load-file \"%s\")" test-fname)))))))))

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
              (lispy--eval-clojure-cider
               (format "(macroexpand '%s)" str))
            (lispy--eval-clojure-cider
             (format "(lispy.clojure/flatten-expr '%s)" str)))))
    (goto-char (car bnd))
    (delete-region (car bnd) (cdr bnd))
    (insert result)
    (when begp
      (goto-char (car bnd))))
  (lispy-alt-multiline))

(defun lispy--clojure-debug-step-in ()
  "Inline a Clojure function at the point of its call."
  (lispy--clojure-detect-ns)
  (let* ((e-str (format "(lispy.clojure/debug-step-in\n'%s)"
                        (lispy--string-dwim)))
         (str (substring-no-properties
               (lispy--eval-clojure-1 e-str nil)))
         (old-session (sesman-current-session 'CIDER)))
    (lispy-follow)
    (when (string-match "(clojure.core/in-ns (quote \\([^)]+\\))" str)
      (setq lispy--clojure-ns (match-string 1 str)))
    (when (equal (file-name-nondirectory (buffer-file-name)) "lispy-clojure.clj")
      (sesman-link-session 'CIDER old-session))
    (lispy--eval-clojure-cider str)
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
  (let* ((r (read (lispy--eval-clojure-cider
                   (format "(lispy.clojure/location '%s)" symbol))))
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

(defun lispy--clojure-dot-object (&optional bnd)
  (let* ((bnd (or bnd
                  (bounds-of-thing-at-point 'symbol)
                  (cons (point) (point))))
         (nested-p (eq (char-before (car bnd)) ?\()))
    (when (save-excursion (lispy--out-backward (if nested-p 2 1) t) (looking-at "(\\.+"))
      (string-trim
       (let ((str
              (concat
               (buffer-substring-no-properties (match-beginning 0) (1- (car bnd)))
               ")")))
         (if (<= (save-excursion
                   (when nested-p
                     (lispy--out-backward 1 t))
                   (lispy-dotimes 100 (backward-sexp 1)))
                 (if (or nested-p (= (car bnd) (cdr bnd))) 2 3))
             (string-trim str "[(.]+" ")")
           str))))))

(defun lispy-clojure-complete-at-point ()
  (cond ((lispy-complete-fname-at-point))
        ((and (memq major-mode lispy-clojure-modes)
              (lispy--clojure-middleware-loaded-p))
         (ignore-errors
           (lispy--clojure-detect-ns)
           (let* ((bnd (or (bounds-of-thing-at-point 'symbol)
                           (cons (point) (point))))
                  (obj (lispy--clojure-dot-object bnd))
                  res)
             (cond ((and obj
                         (setq res (lispy--eval-clojure-cider-noerror
                                    (format "(lispy.clojure/object-members %s)" obj))))
                    (let ((cands (read res)))
                      (when (> (cdr bnd) (car bnd))
                        (setq cands (all-completions (lispy--string-dwim bnd) cands)))
                      (list (car bnd) (cdr bnd) cands)))
                   ((eq (lispy--clojure-process-type) 'cljs)
                    nil)))))))

(defun lispy--eval-clojure-cider-noerror (e-str)
  (condition-case nil
      (lispy--eval-clojure-cider e-str)
    (eval-error nil)))

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
                 (lispy--eval-clojure-cider
                  (format "(lispy.clojure/method-signature (lispy.clojure/reval \"%s\" nil) \"%s\")" object method)))))
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
  (read (lispy--eval-clojure-cider
         (format "(lispy.clojure/ctor-args %s)" symbol))))

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
         (split-string (lispy--eval-clojure-cider
                        "(lispy.clojure/all-docs 'clojure.core)")
                       "::")))
    (ivy-read "var: " cands
              :action #'lispy-clojure-apropos-action)))

(provide 'le-clojure)

;;; le-clojure.el ends here
