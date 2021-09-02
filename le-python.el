;;; le-python.el --- lispy support for Python. -*- lexical-binding: t -*-

;; Copyright (C) 2016-2019 Oleh Krehel

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

(require 'python)
(require 'json)

(defun lispy-trim-python (str)
  "Trim extra Python indentation from STR.

STR is a string copied from Python code. It can be that each line
of STR is prefixed by e.g. 4 or 8 or 12 spaces.
Stripping them will produce code that's valid for an eval."
  (if (string-match "\\`\\( +\\)" str)
      (let* ((indent (match-string 1 str))
             (re (concat "^" indent)))
        (apply #'concat
               (split-string str re t)))
    str))

(defun lispy-eval-python-bnd ()
  (let (bnd)
    (save-excursion
      (cond ((region-active-p)
             (cons
              (if (> (count-lines (region-beginning) (region-end)) 1)
                  (save-excursion
                    (goto-char (region-beginning))
                    (skip-chars-backward " ")
                    (point))
                (region-beginning))
              (region-end)))
            ((and (looking-at lispy-outline)
                  (looking-at lispy-outline-header))
             (lispy--bounds-outline))
            ((looking-at "@")
             (setq bnd (cons (point)
                             (save-excursion
                               (forward-sexp)
                               (skip-chars-forward "[ \t\n]")
                               (cdr (lispy-bounds-python-block))))))
            ((setq bnd (lispy-bounds-python-block)))
            ((bolp)
             (lispy--bounds-c-toplevel))
            ((lispy-bolp)
             (lispy--bounds-c-toplevel))
            (t
             (cond ((lispy-left-p))
                   ((lispy-right-p)
                    (backward-list))
                   (t
                    (error "Unexpected")))
             (setq bnd (lispy--bounds-dwim))
             (ignore-errors (backward-sexp))
             (while (or (eq (char-before) ?.)
                        (eq (char-after) ?\())
               (backward-sexp))
             (setcar bnd (point))
             bnd)))))

(defun lispy--count-regex (re bnd)
  (let ((count 0))
    (save-excursion
      (goto-char (car bnd))
      (while (re-search-forward re (cdr bnd) t)
        (unless (lispy--in-string-or-comment-p)
          (cl-incf count))))
    count))

(defun lispy-extended-eval-str (bnd)
  (let ((lp (lispy--count-regex "(" bnd))
        (rp (lispy--count-regex ")" bnd)))
    (save-excursion
      (goto-char (cdr bnd))
      (while (< rp lp)
        (re-search-forward "[()]" nil t)
        (cond ((string= (match-string 0) "(")
               (cl-incf lp))
              ((string= (match-string 0) ")")
               (cl-incf rp))
              (t
               (error "Unexpected"))))
      (if (lispy-after-string-p ")")
          (let ((end (point)))
            (save-excursion
              (forward-sexp -1)
              (concat (buffer-substring-no-properties
                       (car bnd) (point))
                      (replace-regexp-in-string
                       "[\\]*\n[\t ]*" " "
                       (replace-regexp-in-string
                        "^ *#.*$" ""
                        (buffer-substring-no-properties
                         (point) end))))))
        (buffer-substring-no-properties (car bnd) (point))))))

(defun lispy-eval-python-str (&optional bnd)
  (let* ((bnd (or bnd (lispy-eval-python-bnd)))
         (str1 (lispy-trim-python
                (lispy-extended-eval-str bnd)))
         (str1.5 (replace-regexp-in-string "^ *#[^\n]+\n" "" str1))
         ;; (str2 (replace-regexp-in-string "\\\\\n +" "" str1.5))
         ;; (str3 (replace-regexp-in-string "\n *\\([])}]\\)" "\\1" str2))
         ;; (str4 (replace-regexp-in-string "\\([({[,]\\)\n +" "\\1" str3))
         ;; (str5 (replace-regexp-in-string "\"\n *\"" "\" \"" str4))
         )
    str1.5))

(defun lispy-bounds-python-block ()
  (if (save-excursion
        (when (looking-at " ")
          (forward-char))
        (python-info-beginning-of-block-p))
      (let ((indent (if (bolp)
                        0
                      (1+ (- (point) (line-beginning-position))))))
        (cons
         (line-beginning-position)
         (save-excursion
           (python-nav-end-of-block)
           (while (let ((pt (point))
                        bnd)
                    (skip-chars-forward "\n ")
                    (when (setq bnd (lispy--bounds-comment))
                      (goto-char (cdr bnd)))
                    (beginning-of-line)
                    (if (looking-at (format "[\n ]\\{%d,\\}\\(except\\|else\\|elif\\)" indent))
                        t
                      (goto-char pt)
                      nil))
             (goto-char (match-beginning 1))
             (python-nav-end-of-block))
           (point))))
    (cons (if (looking-at " ")
              (1+ (point))
            (point))
          (save-excursion
            (end-of-line)
            (let (bnd)
              (when (setq bnd (lispy--bounds-string))
                (goto-char (cdr bnd))))
            (end-of-line)
            (while (member (char-before) '(?\\ ?\( ?\, ?\[ ?\{))
              (if (member (char-before) '(?\( ?\[ ?\{))
                  (progn
                    (up-list)
                    (end-of-line))
                (end-of-line 2)))
            (point)))))

(defvar-local lispy-python-proc nil)

(declare-function mash-make-shell "ext:mash")

(defun lispy-set-python-process-action (x)
  (when (and current-prefix-arg (consp x))
    (let* ((process (cdr x))
           (buffer (process-buffer process)))
      (kill-process process)
      (sit-for 0.01)
      (kill-buffer buffer)
      (setq x (car x))))
  (setq lispy-python-proc
        (cond ((consp x)
               (cdr x))
              ((require 'mash-python nil t)
               (save-window-excursion
                 (get-buffer-process
                  (mash-make-shell x 'mash-new-lispy-python))))
              (t
               (lispy--python-proc (concat "lispy-python-" x)))))
  (unless (lispy--eval-python "lp")
    (lispy-python-middleware-reload)))

(defvar lispy-python-process-regexes
  '("^lispy-python-\\(.*\\)" "\\`\\(Python\\)\\'" "\\`\\(comint\\)\\'")
  "List of regexes for process buffers that run Python.")

(defun lispy-short-process-name (x)
  (let ((pname (process-name x)))
    (car
     (delq nil
           (mapcar
            (lambda (re)
              (when (string-match re pname)
                (match-string 1 pname)))
            lispy-python-process-regexes)))))

(defvar lispy-override-python-binary nil
  "When non-nil, override what `lispy--python-proc' uses.")

(defun lispy-set-python-process (&optional arg)
  "Associate a (possibly new) Python process to the current buffer.

Each buffer can have only a single Python process associated with
it at one time."
  (interactive "P")
  (let* ((process-names
          (delq nil
                (mapcar
                 (lambda (x)
                   (let ((name (lispy-short-process-name x)))
                     (when name
                       (cons name x))))
                 (process-list))))
         (lispy-override-python-binary
          (when (equal arg '(16))
            (read-string "python binary: "))))
    (ivy-read (if arg "Restart process: " "Process: ") process-names
              :action #'lispy-set-python-process-action
              :preselect (when (process-live-p lispy-python-proc)
                           (lispy-short-process-name lispy-python-proc))
              :caller 'lispy-set-python-process)))

(defvar lispy--python-middleware-loaded-p nil
  "Nil if the Python middleware in \"lispy-python.py\" wasn't loaded yet.")

(defun lispy--python-proc (&optional name)
  (let* ((proc-name (or name
                        (and (process-live-p lispy-python-proc)
                             lispy-python-proc)
                        "lispy-python-default"))
         (process (get-process proc-name)))
    (if (process-live-p process)
        process
      (let* ((python-shell-font-lock-enable nil)
             (inferior-python-mode-hook nil)
             (python-shell-interpreter
              (cond
                ((save-excursion
                   (goto-char (point-min))
                   (looking-at "#!\\(?:/usr/bin/env \\)\\(.*\\)$"))
                 (match-string-no-properties 1))
                ((file-exists-p python-shell-interpreter)
                 (expand-file-name python-shell-interpreter))
                (t
                 python-shell-interpreter)))
             (python-binary-name
              (or lispy-override-python-binary
                  (concat
                   (string-trim-right
                    (shell-command-to-string
                     (concat "which " python-shell-interpreter)))
                   " "
                   python-shell-interpreter-args)))
             (buffer
              (let ((python-shell-completion-native-enable nil))
                (python-shell-make-comint
                 python-binary-name proc-name nil nil))))
        (setq process (get-buffer-process buffer))
        (with-current-buffer buffer
          (python-shell-completion-native-turn-on)
          (setq lispy-python-proc process)
          (lispy-python-middleware-reload)))
      process)))

(defun lispy--python-print (str)
  (format
   (if (and (memq this-command '(pspecial-lispy-eval lispy-eval))
            (null current-prefix-arg))
       "lp.pprint(%s)"
     "print(repr(%s))")
   str))

(defun lispy--python-nth-element (str single-line-p)
  "Check if STR is of the form \"ITEM in ARRAY_LIKE\".
If so, return an equivalent of ITEM = ARRAY_LIKE[IDX]; ITEM."
  (when (and single-line-p
             (string-match "\\`\\([A-Z_a-z0-9]+\\|\\(?:([^\n]+)\\)\\) in \\(.*\\)\\'" str)
             (not (save-excursion (beginning-of-line) (looking-at " *if"))))
    (let* ((vars (match-string 1 str))
           (val (match-string 2 str))
           (len (read (lispy--eval-python (format "len(list(%s))" val) t)))
           (repr (ignore-errors (read (lispy--eval-python (format "lp.print_elisp(%s)" val) t))))
           (idx
            (read
             (ivy-read
              "idx: "
              (if repr
                  (cl-mapcar (lambda (x i)
                               (concat (number-to-string i)
                                       " "
                                       (cond ((listp x)
                                              (mapconcat
                                               (lambda (y) (if (stringp y) y (prin1-to-string y)))
                                               x " "))
                                             ((keywordp x)
                                              (prin1-to-string x))
                                             (t
                                              x))))
                             repr
                             (number-sequence 0 (1- len)))
                (mapcar #'number-to-string (number-sequence 0 (1- len))))))))
      (format "%s = list (%s)[%s]\nprint ((%s))"
              vars val idx vars))))

(defun lispy--python-eval-string-dwim (str)
  (setq str (string-trim str))
  (when (string-match-p "\\`super()" str)
    (let* ((cls (car (split-string (python-info-current-defun) "\\."))))
      (setq str (replace-regexp-in-string
                 "super()" (format "super(%s, self)" cls) str))))
  (let ((single-line-p (= (cl-count ?\n str) 0)))
    (cond
      ((string-match "^\\[" str)
       (format "__last__ = %s\n%s"
               str (lispy--python-print "__last__")))
      ((string-match "\\`\\(\\(?:\\sw\\|\\s_\\)+\\)\\'" str)
       (lispy--python-print (match-string 1 str)))
      ((and (or (string-match "\\`\\(\\(?:[., ]\\|\\sw\\|\\s_\\|[][]\\)+\\) += " str)
                (string-match "\\`\\(([^)]+)\\) *=[^=]" str)
                (string-match "\\`\\(\\(?:\\sw\\|\\s_\\)+ *\\[[^]]+\\]\\) *=[^=]" str))
            (save-match-data
              (or single-line-p
                  (and (not (string-match-p "lp\\." str))
                       (equal (lispy--eval-python
                               (format "x=lp.is_assignment(\"\"\"%s\"\"\")\nprint (x)" str)
                               t)
                              "True")))))
       (concat str "\n" (lispy--python-print (match-string 1 str))))
      ((lispy--python-nth-element str single-line-p))
      ((string-match "\\`def \\([a-zA-Z_0-9]+\\)\\s-*(\\s-*self" str)
       (let ((qual-name (python-info-current-defun)))
         (concat str
                 "\n"
                 (format "lp.rebind(%s, fname='%s', line=%d)"
                         qual-name
                         (buffer-file-name)
                         (line-number-at-pos)))))
      (t
       str))))

(declare-function lpy-switch-to-shell "ext:lpy")

(defun lispy--eval-python (str &optional plain)
  "Eval STR as Python code."
  (let ((single-line-p (= (cl-count ?\n str) 0)))
    (unless plain
      (setq str (lispy--python-eval-string-dwim str))
      (when (string-match "__file__" str)
        (lispy--eval-python (format "__file__ = '%s'\n" (buffer-file-name)) t))
      (when (and single-line-p (string-match "\\`return \\(.*\\)\\'" str))
        (setq str (match-string 1 str))))
    (let ((res
           (cond ((or single-line-p
                      (string-match "\n .*\\'" str)
                      (string-match "\"\"\"" str))
                  (python-shell-send-string-no-output
                   str (lispy--python-proc)))
                 ((string-match "\\`\\([\0-\377[:nonascii:]]*\\)\n\\([^\n]*\\)\\'" str)
                  (let* ((p1 (match-string 1 str))
                         (p2 (match-string 2 str))
                         (p1-output (python-shell-send-string-no-output
                                     p1 (lispy--python-proc)))
                         p2-output)
                    (cond
                      ((string-match-p "SyntaxError:\\|error:" p1-output)
                       (python-shell-send-string-no-output
                        str (lispy--python-proc)))
                      ((null p1-output)
                       (signal 'eval-error ""))
                      ((null (setq p2-output (lispy--eval-python p2)))
                       (signal 'eval-error ""))
                      (t
                       (concat
                        (if (string= p1-output "")
                            ""
                          (concat p1-output "\n"))
                        p2-output)))))
                 (t
                  (error "unexpected")))))
      (cond
        ((string-match "SyntaxError: 'return' outside function\\'" res)
         (lispy--eval-python
          (concat "__return__ = None\n"
                  (replace-regexp-in-string
                   "\\(^ *\\)return\\(.*\\)"
                   (lambda (x)
                     (concat
                      (match-string 1 x)
                      "__return__ ="
                      (if (= 0 (length (match-string 2 x)))
                          " None"
                        (match-string 2 x))))
                   str)
                  "\n"
                  (lispy--python-print "__return__"))
          t))
        ((string-match "^RuntimeError: break$" res)
         (lpy-switch-to-shell)
         (goto-char (point-max))
         (insert "lp.pm()")
         (comint-send-input)
         "breakpoint")
        ((string-match "^Traceback.*:" res)
         (set-text-properties
          (match-beginning 0)
          (match-end 0)
          '(face error)
          res)
         (signal 'eval-error res))
        ((equal res "")
         (setq lispy-eval-output "(ok)")
         "")
        ((string-match-p "^<\\(?:map\\|filter\\|generator\\|enumerate\\|zip\\) object" res)
         (let ((last (car (last (split-string str "\n")))))
           (cond ((string-match "\\`lp.pprint(\\(.*\\))\\'" last)
                  (setq str (match-string 1 last)))
                 ((string-match "\\`print(repr(\\(.*\\)))\\'" last)
                  (setq str (match-string 1 last)))))
         (lispy--eval-python (format "list(%s)" str) t))
        ((string-match-p "SyntaxError:" res)
         (signal 'eval-error res))
        (t
         (replace-regexp-in-string "\\\\n" "\n" res))))))

(defun lispy--python-array-to-elisp (array-str)
  "Transform a Python string ARRAY-STR to an Elisp string array."
  (when (and (stringp array-str)
             (not (string= array-str "")))
    (let ((parts (with-temp-buffer
                   (python-mode)
                   (insert (substring array-str 1 -1))
                   (goto-char (point-min))
                   (let (beg res)
                     (while (< (point) (point-max))
                       (setq beg (point))
                       (forward-sexp)
                       (push (buffer-substring-no-properties beg (point)) res)
                       (skip-chars-forward ", "))
                     (nreverse res)))))
      (mapcar (lambda (s)
                (if (string-match "\\`\"" s)
                    (read s)
                  (if (string-match "\\`'\\(.*\\)'\\'" s)
                      (match-string 1 s)
                    s)))
              parts))))

(defun lispy-python-symbol-bnd ()
  (let ((bnd (or (bounds-of-thing-at-point 'symbol)
                 (cons (point) (point)))))
    (save-excursion
      (goto-char (car bnd))
      (while (progn
               (skip-chars-backward " ")
               (lispy-after-string-p "."))
        (backward-char 1)
        (when (lispy-after-string-p "]")
          (backward-list 1))
        (skip-chars-backward " ")
        (if (lispy-after-string-p ")")
            (backward-sexp 2)
          (backward-sexp)))
      (skip-chars-forward " ")
      (setcar bnd (point)))
    bnd))

(defun lispy--python-beginning-of-object ()
  (save-excursion
    (backward-sexp)
    (while (not (or
                 (bolp)
                 (lispy-looking-back "[[ \t(]")))
      (backward-sexp))
    (point)))

(defun lispy-python-completion-at-point ()
  (cond ((looking-back "^\\(import\\|from\\) .*" (line-beginning-position))
         (let* ((line (buffer-substring-no-properties
                       (line-beginning-position)
                       (point)))
                (str
                 (format "lp.jedi_completions('%s')" line))
                (cands
                 (lispy--python-array-to-elisp
                  (lispy--eval-python str)))
                (bnd (bounds-of-thing-at-point 'symbol))
                (beg (if bnd (car bnd) (point)))
                (end (if bnd (cdr bnd) (point))))
           (list beg end cands)))
        ((looking-back "\\(?:\\sw\\|\\s_\\|)\\|\\]\\)\\[\\(\\(?:'\\|\"\\)?\\)" (line-beginning-position))
         (let* ((quote_str (match-string-no-properties 1))
                (bnd (save-excursion
                       (goto-char (1- (match-beginning 1)))
                       (cons (point)
                             (lispy--python-beginning-of-object))))
                (str (lispy--string-dwim bnd))
                (keys (read (lispy--eval-python
                             (format "lp.print_elisp(%s.keys())" str)))))
           (list (point) (point)
                 (if (> (length quote_str) 0)
                     keys
                   (mapcar (lambda (s) (concat "\"" s "\"")) keys)))))
        ((lispy-complete-fname-at-point))
        (t
         (let* ((bnd (lispy-python-symbol-bnd))
                (str (buffer-substring-no-properties
                      (car bnd) (cdr bnd))))
           (when (string-match "\\`\\(.*\\)\\.[^.]*\\'" str)
             (let ((expr (format "__t__ = %s" (substring str 0 (match-end 1)))))
               (setq str (concat "__t__" (substring str (match-end 1))))
               (cl-incf (car bnd) (match-end 1))
               (lispy--eval-python expr t)))
           (list (car bnd)
                 (cdr bnd)
                 (mapcar (lambda (s)
                           (replace-regexp-in-string
                            "__t__" ""
                            (if (string-match "(\\'" s)
                                (substring s 0 (match-beginning 0))
                              s)))
                         (python-shell-completion-get-completions
                          (lispy--python-proc)
                          nil str)))))))

(defvar lispy--python-arg-key-re "\\`\\(\\(?:\\sw\\|\\s_\\)+\\)=\\([^=].*\\)\\'"
  "Constant regexp for matching function keyword spec.")

(defun lispy--python-args (beg end)
  (let (res)
    (save-excursion
      (goto-char beg)
      (skip-chars-forward "\n\t ")
      (setq beg (point))
      (while (< (point) end)
        (forward-sexp)
        (while (and (< (point) end)
                    (not (looking-at ",")))
          (forward-sexp))
        (push (buffer-substring-no-properties
               beg (point))
              res)
        (skip-chars-forward ", \n")
        (setq beg (point))))
    (nreverse res)))

(defun lispy--python-step-in-loop ()
  (when (looking-at " ?for \\([A-Z_a-z,0-9 ()]+\\) in \\(.*\\):")
    (let* ((vars (match-string-no-properties 1))
           (val (match-string-no-properties 2))
           (res (lispy--eval-python
                 (format "lp.list_step(\"%s\",%s)" vars val)
                 t)))
      (lispy-message res))))

(defun lispy--python-debug-step-in ()
  (let ((orig-point (point)))
    (unless (lispy--python-step-in-loop)
      (when (looking-at " *(")
        ;; tuple assignment
        (forward-list 1))
      (re-search-forward "(" (line-end-position))
      (backward-char)
      (let* ((p-ar-beg (point))
             (p-ar-end (save-excursion
                         (forward-list)
                         (point)))
             (p-fn-end (progn
                         (skip-chars-backward " ")
                         (point)))
             (method-p nil)
             (p-fn-beg (progn
                         (backward-sexp)
                         (while (or (eq (char-before) ?.) (looking-at "\\["))
                           (setq method-p t)
                           (backward-sexp))
                         (point)))
             (fn (buffer-substring-no-properties
                  p-fn-beg p-fn-end))
             (args
              (lispy--python-args (1+ p-ar-beg) (1- p-ar-end)))
             (args (cl-mapcan (lambda (arg)
                                (if (string-match "\\`\\*\\(.*\\)\\'" arg)
                                    (read
                                     (lispy--eval-python
                                      (format "lp.print_elisp(%s)" (match-string 1 arg))))
                                  (list arg)))
                              args))
             (args (if (and method-p
                            (string-match "\\`\\(.*?\\)\\.\\([^.]+\\)\\'" fn))
                       (cons (match-string 1 fn)
                             args)
                     args))
             (args-key (cl-remove-if-not
                        (lambda (s)
                          (string-match lispy--python-arg-key-re s))
                        args))
             (args-normal (cl-set-difference args args-key))
             (fn-data
              (read (lispy--eval-python
                     (format "lp.argspec(%s)" fn))))
             (fn-args
              (plist-get fn-data :args))
             (fn-varargs
              (plist-get fn-data :varargs))
             (fn-keywords
              (plist-get fn-data :keywords))
             (fn-defaults
              (mapcar (lambda (x)
                        (cond ((stringp x)
                               (prin1-to-string x))
                              (x x)
                              (t
                               "None")))
                      (plist-get fn-data :defaults)))
             (fn-alist
              (cl-mapcar #'cons
                         fn-args
                         (append (make-list (- (length fn-args)
                                               (length fn-defaults))
                                            nil)
                                 fn-defaults)))
             fn-alist-x dbg-cmd extra-keywords extra-varargs)
        (cond (method-p
               (unless (member '("self") fn-alist)
                 (push '("self") fn-alist)))
              ((member '("self") fn-alist)
               (push (format "object.__new__(%s)" fn)
                     args-normal)))
        (setq fn-alist-x fn-alist)
        (dolist (arg args-normal)
          (let ((var-name (pop fn-alist-x)))
            (if var-name
                (setcdr var-name arg)
              (push arg extra-varargs))))
        (setq extra-varargs (nreverse extra-varargs))
        (dolist (arg args-key)
          (if (string-match lispy--python-arg-key-re arg)
              (let ((arg-name (match-string 1 arg))
                    (arg-val (match-string 2 arg))
                    arg-cell)
                (if (setq arg-cell (assoc arg-name fn-alist))
                    (setcdr arg-cell arg-val)
                  (if fn-keywords
                      (push (concat arg-name "=" arg-val) extra-keywords)
                    (error "\"%s\" is not in %s" arg-name fn-alist))))
            (error "\"%s\" does not match the regex spec" arg)))
        (when (memq nil (mapcar #'cdr fn-alist))
          (error "Not all args were provided: %s" fn-alist))
        (when fn-varargs
          (cl-pushnew
           (cons fn-varargs
                 (concat "(" (mapconcat (lambda (s) (concat s ",")) extra-varargs " ") ")"))
           fn-alist))
        (when fn-keywords
          (cl-pushnew
           (cons fn-keywords
                 (concat "dict(" (mapconcat #'identity extra-keywords ", ") ")"))
           fn-alist))
        (setq dbg-cmd
              (mapconcat (lambda (x)
                           (format "%s = %s" (car x) (cdr x)))
                         fn-alist
                         "; "))
        (condition-case nil
            (lispy--eval-python dbg-cmd t)
          (error
           (lispy--eval-python
            (format "lp.step_in(%s,%s)" fn (buffer-substring-no-properties
                                            (1+ p-ar-beg) (1- p-ar-end))))))
        (goto-char orig-point)
        (when fn-data
          (set-text-properties
           0 1
           `(
             filename ,(plist-get fn-data :filename)
             line ,(plist-get fn-data :line))
           fn))
        (lispy-goto-symbol fn)))))

(declare-function deferred:sync! "ext:deferred")
(declare-function jedi:goto-definition "ext:jedi-core")
(declare-function jedi:call-deferred "ext:jedi-core")

(defun lispy--goto-symbol-python (file line)
  (find-file file)
  (goto-char (point-min))
  (forward-line (1- line))
  (back-to-indentation)
  (unless (bolp)
    (backward-char)))

(defun lispy-goto-symbol-python (symbol)
  (save-restriction
    (widen)
    (let ((file (get-text-property 0 'filename symbol))
          (line (get-text-property 0 'line symbol)))
      (if file
          (lispy--goto-symbol-python file line)
        (let ((res (ignore-errors
                     (or
                      (deferred:sync!
                          (jedi:goto-definition))
                      t))))
          (if (member res '(nil "Definition not found."))
              (let* ((symbol (python-info-current-symbol))
                     (symbol-re (concat "^\\(?:def\\|class\\).*" (car (last (split-string symbol "\\." t)))))
                     (r (lispy--eval-python
                         (format "lp.argspec(%s)" symbol)))
                     (plist (and r (read r))))
                (cond (plist
                       (lispy--goto-symbol-python
                        (plist-get plist :filename)
                        (plist-get plist :line)))
                      ((and (equal file "None")
                            (re-search-backward symbol-re nil t)))
                      (t
                       (error "Both jedi and inspect failed"))))
            (unless (looking-back "def " (line-beginning-position))
              (jedi:goto-definition))))))))

(defun lispy--python-docstring (symbol)
  "Look up the docstring for SYMBOL.

First, try to see if SYMBOL.__doc__ returns a string in the
current REPL session (dynamic).

Otherwise, fall back to Jedi (static)."
  (let ((dynamic-result (lispy--eval-python (concat symbol ".__doc__"))))
    (if (> (length dynamic-result) 0)
        (mapconcat #'string-trim-left
                   (split-string (substring dynamic-result 1 -1) "\\\\n")
                   "\n")
      (require 'jedi)
      (plist-get (car (deferred:sync!
                          (jedi:call-deferred 'get_definition)))
                 :doc))))

(defun lispy-python-middleware-reload ()
  (interactive)
  (setq lispy--python-middleware-loaded-p nil)
  (lispy--python-middleware-load))

(defvar lispy-python-init-file "~/git/site-python/init.py")

(defvar lispy-python-init-file-remote "/opt/lispy-python.py")

(defun lispy--python-middleware-load ()
  "Load the custom Python code in \"lispy-python.py\"."
  (unless lispy--python-middleware-loaded-p
    (let* ((lispy-python-py
            (if (file-remote-p default-directory)
                lispy-python-init-file-remote
              (expand-file-name "lispy-python.py" lispy-site-directory)))
           (module-path (format "'lispy-python','%s'" lispy-python-py)))
      (lispy--eval-python
       (format
        (concat
         "try:\n"
         "    from importlib.machinery import SourceFileLoader\n"
         "    lp=SourceFileLoader(%s).load_module()\n"
         "except:\n"
         "    import imp;lp=imp.load_source(%s)\n"
         "__name__='__repl__';"
         "pm=lp.Autocall(lp.pm);")
        module-path module-path))
      (when (file-exists-p lispy-python-init-file)
        (lispy--eval-python
         (format "exec (open ('%s').read(), globals ())"
                 (expand-file-name lispy-python-init-file))))
      (setq lispy--python-middleware-loaded-p t))))

(defun lispy--python-arglist (symbol filename line column)
  (lispy--python-middleware-load)
  (let* ((boundp (lispy--eval-python symbol))
         (code (if boundp
                   (format "lp.arglist(%s)" symbol)
                 (format "lp.arglist_jedi(%d, %d, '%s')" line column filename)))
         (args (lispy--python-array-to-elisp
                (lispy--eval-python
                 code))))
    (format "%s (%s)"
            symbol
            (mapconcat #'identity
                       (delete "self" args)
                       ", "))))

(defun lispy-python-set-breakpoint ()
  (let* ((beg (save-excursion
                (unless (bolp)
                  (python-nav-beginning-of-defun))
                (if (not (looking-at "def \\(\\(?:\\sw\\|\\s_\\)+\\).*:$"))
                    (user-error "Not on a def statement")
                  (point))))
         (name (match-string 1))
         (start (buffer-substring-no-properties
                 beg
                 (if (bolp)
                     (line-end-position)
                   (point)))))
    (lispy--eval-python
     (concat
      start
      "\n    raise(RuntimeError(\"break\"))"
      (format "\nlp.Stack.line_numbers[('%s', '%s')] = %d"
              (buffer-file-name)
              name
              (line-number-at-pos))))
    (message "Break: %s" name)))

(provide 'le-python)

;;; le-python.el ends here
