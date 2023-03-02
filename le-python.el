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
            ((and (looking-at "@") (bolp))
             (setq bnd (cons (point)
                             (save-excursion
                               (re-search-forward "^def" nil t)
                               (goto-char (match-beginning 0))
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
  "This function should strip python comments.
So that we can make multi-line expression into a single-line one.
It didn't work great."
  (buffer-substring-no-properties (car bnd) (cdr bnd))
  ;; (let ((lp (lispy--count-regex "(" bnd))
  ;;       (rp (lispy--count-regex ")" bnd)))
  ;;   (save-excursion
  ;;     (goto-char (cdr bnd))
  ;;     (while (< rp lp)
  ;;       (re-search-forward "[()]" nil t)
  ;;       (cond ((string= (match-string 0) "(")
  ;;              (cl-incf lp))
  ;;             ((string= (match-string 0) ")")
  ;;              (cl-incf rp))
  ;;             (t
  ;;              (error "Unexpected"))))
  ;;     (if (lispy-after-string-p ")")
  ;;         (let ((end (point)))
  ;;           (save-excursion
  ;;             (forward-sexp -1)
  ;;             (concat (buffer-substring-no-properties
  ;;                      (car bnd) (point))
  ;;                     (replace-regexp-in-string
  ;;                      "[\\]*\n[\t ]*" " "
  ;;                      (replace-regexp-in-string
  ;;                       " *#.*[^\"]$" ""
  ;;                       (buffer-substring-no-properties
  ;;                        (point) end))))))
  ;;       (buffer-substring-no-properties (car bnd) (point)))))
  )

(defun lispy-eval-python-str (&optional bnd)
  (let* ((bnd (or bnd (lispy-eval-python-bnd)))
         (str1 (lispy-trim-python
                (lispy-extended-eval-str bnd)))
         (str1.4 (if (string-match-p "\\`\\(\\w\\|\\s_\\)+ :=" str1)
                     (replace-regexp-in-string ":=" "=" str1)
                   str1))
         (str1.5 (replace-regexp-in-string "^ *#[^\n]+\n" "" str1.4))
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

(defvar-local lispy-python-buf nil)

(declare-function mash-make-shell "ext:mash")

(defvar-local python-shell--interpreter nil)
(defvar-local python-shell--interpreter-args nil)

(define-minor-mode lispy-python-interaction-mode
  "Minor mode for eval-ing Python code."
  :group 'lispy
  (when lispy-python-interaction-mode
    (when python-shell--parent-buffer
      (python-util-clone-local-variables python-shell--parent-buffer))
    (setq-local indent-tabs-mode nil)
    (setq-local python-shell--prompt-calculated-input-regexp nil)
    (setq-local python-shell--block-prompt nil)
    (setq-local python-shell--prompt-calculated-output-regexp nil)
    (python-shell-prompt-set-calculated-regexps)
    (setq comint-prompt-regexp python-shell--prompt-calculated-input-regexp)
    (setq-local comint-prompt-read-only t)
    (setq mode-line-process '(":%s"))
    (setq-local comint-output-filter-functions
                '(ansi-color-process-output
                  python-shell-comint-watch-for-first-prompt-output-filter
                  python-comint-postoutput-scroll-to-bottom
                  comint-watch-for-password-prompt))
    (setq-local compilation-error-regexp-alist python-shell-compilation-regexp-alist)
    (add-hook 'completion-at-point-functions
              #'python-shell-completion-at-point nil 'local)
    (define-key inferior-python-mode-map "\t"
      'python-shell-completion-complete-or-indent)
    (make-local-variable 'python-shell-internal-last-output)
    (compilation-shell-minor-mode 1)
    (python-pdbtrack-setup-tracking)))

(defun lispy-set-python-process-action (x)
  (when (and current-prefix-arg (consp x))
    (let* ((process (cdr x))
           (buffer (process-buffer process)))
      (kill-process process)
      (sit-for 0.01)
      (kill-buffer buffer)
      (setq x (car x))))
  (let ((buf (cond ((consp x)
                    (process-buffer (cdr x)))
                   ((require 'mash-python nil t)
                    (save-window-excursion
                      (mash-make-shell x 'mash-new-lispy-python)))
                   (t
                    (process-buffer
                     (lispy--python-proc (concat "lispy-python-" x)))))))

    (setq lispy-python-buf buf)
    (with-current-buffer lispy-python-buf
      (lispy-python-interaction-mode)
      (setq lispy--python-middleware-file
            (if (file-name-absolute-p lispy-python-middleware-file)
                lispy-python-middleware-file
              (expand-file-name "lispy-python.py" lispy-site-directory)))
      (setq lispy-python-buf buf)))
  (let ((lp (ignore-errors (lispy--eval-python-plain "lp"))))
    (unless (and lp (string-match-p "module 'lispy-python'" lp))
      (lispy-python-middleware-reload))))

(defvar lispy-python-process-regexes
  '("^lispy-python-\\(.*\\)" "\\`\\(Python\\)\\'"
    "\\`\\(comint.*\\)\\'"
    "\\`\\(shell.*\\)\\'"
    "\\`\\(gud-\\(?:pdb\\|python\\)\\)\\'")
  "List of regexes for process buffers that run Python.")

(defun lispy-short-process-name (x)
  (let ((pname (process-name x)))
    (car
     (delq nil
           (mapcar
            (lambda (re)
              (when (string-match re pname)
                (let ((m (match-string 1 pname)))
                  (if (string-match-p "comint\\|shell" m)
                      (buffer-name (process-buffer x))
                    m))))
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
              :preselect (when (process-live-p (get-buffer-process lispy-python-buf))
                           (lispy-short-process-name (get-buffer-process lispy-python-buf)))
              :caller 'lispy-set-python-process)))

(defvar lispy--python-middleware-loaded-p nil
  "Nil if the Python middleware in \"lispy-python.py\" wasn't loaded yet.")

(defvar lispy-python-middleware-file "lispy-python.py")

(defvar lispy--python-middleware-file "lispy-python.py")

(defvar lispy-python-init-file (expand-file-name "~/git/site-python/init.py"))

(defvar lispy--python-init-file nil)

(defun lispy--python-poetry-name ()
  (let ((pyproject (expand-file-name "pyproject.toml" (counsel-locate-git-root))))
    (and (file-exists-p pyproject)
         (not (equal python-shell-interpreter "python"))
         (with-current-buffer (find-file-noselect pyproject)
           (goto-char (point-min))
           (when (re-search-forward "\\[tool.poetry\\]\nname *= *\"\\([^\"]+\\)\"" nil t)
             (match-string-no-properties 1))))))

(defun lispy--python-proc-name ()
  (or (and (process-live-p (get-buffer-process lispy-python-buf))
           (process-name (get-buffer-process lispy-python-buf)))
      (let ((name
             (or (lispy--python-poetry-name)
                 (if (string-match "\\(?::python \\|python_\\)\\(.*\\)\\'" python-shell-interpreter-args)
                     (match-string 1 python-shell-interpreter-args)
                   "default"))))
        (concat "lispy-python-" name))))

(defun lispy--python-proc (&optional name)
  (let* ((proc-name (or name
                        (lispy--python-proc-name)))
         (process (get-process proc-name)))
    (if (process-live-p process)
        process
      (let* ((python-shell-font-lock-enable nil)
             (inferior-python-mode-hook nil)
             (poetry-name (lispy--python-poetry-name))
             (python-shell-interpreter
              (if poetry-name
                  "poetry"
                python-shell-interpreter))
             (python-shell-interpreter-args
              (if poetry-name
                  "run python"
                python-shell-interpreter-args))
             ;; (python-shell-interpreter
             ;;  (cond
             ;;   ((and (file-exists-p python-shell-interpreter)
             ;;         (not (file-directory-p python-shell-interpreter)))
             ;;    (expand-file-name python-shell-interpreter))
             ;;   ((save-excursion
             ;;      (goto-char (point-min))
             ;;      (looking-at "#!\\(?:/usr/bin/env \\)\\(.*\\)$"))
             ;;    (match-string-no-properties 1))
             ;;   (t
             ;;    python-shell-interpreter)))
             (python-binary-name
              (or lispy-override-python-binary
                  (concat
                   (string-trim-right
                    (shell-command-to-string
                     (concat "which " python-shell-interpreter)))
                   " "
                   python-shell-interpreter-args)))
             (buffer
              (let ((python-shell-completion-native-enable nil)
                    (default-directory (if poetry-name
                                           (counsel-locate-git-root)
                                         default-directory)))
                (python-shell-make-comint
                 python-binary-name proc-name nil nil))))
        (setq lispy--python-middleware-file
              (if (file-name-absolute-p lispy-python-middleware-file)
                  lispy-python-middleware-file
                (expand-file-name "lispy-python.py" lispy-site-directory)))
        (setq lispy--python-init-file lispy-python-init-file)
        (setq process (get-buffer-process buffer))
        (sit-for 0.1)
        (unless (process-live-p process)
          (pop-to-buffer buffer)
          (user-error "Could not start %s" python-binary-name))
        (with-current-buffer buffer
          (python-shell-completion-native-turn-on)
          (setq lispy-python-buf buffer)
          (lispy-python-middleware-reload)))
      process)))

(defun lispy--python-print (str)
  (format
   (if (and (memq this-command '(pspecial-lispy-eval lispy-eval))
            (memq current-prefix-arg '(nil)))
       "lp.pprint((%s))"
     "print(repr((%s)))")
   str))

(defun lispy--py-to-el (py)
  (read (lispy--eval-python-plain (format "lp.print_elisp(%s)" py))))

(defun lispy--python-nth (py-lst)
  (let (;; (repr (condition-case nil
        ;;           (read (lispy--eval-python-plain (format "print_elisp(%s)" py-expr)))
        ;;         (error
        ;;          (lispy-message "Eval-error: %s" py-expr))))

        (len (lispy--py-to-el (format "len(list(%s))" py-lst)))
        (repr (ignore-errors (lispy--py-to-el (format "list(%s)" py-lst)))))
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
                                     ((stringp x)
                                      x)
                                     (t
                                      (prin1-to-string x)))))
                     repr
                     (number-sequence 0 (1- len)))
        (mapcar #'number-to-string (number-sequence 0 (1- len))))))))


(defun lispy--python-nth-element (str single-line-p)
  "Check if STR is of the form \"ITEM in ARRAY_LIKE\".
If so, return an equivalent of ITEM = ARRAY_LIKE[IDX]; ITEM."
  (when (and single-line-p
             (string-match "\\`\\([A-Z_a-z0-9, ]+\\|\\(?:([^\n]+)\\)\\) in \\(.*\\)\\'" str)
             (not (save-excursion (beginning-of-line) (looking-at " *if"))))
    (let* ((vars (match-string 1 str))
           (val (match-string 2 str))
           (idx (lispy--python-nth val)))
      (format "%s = list (%s)[%s]\nprint ((%s))"
              vars val idx vars))))

(defun lispy--python-eval-string-dwim (str)
  (setq str (string-trim str))
  (let ((single-line-p (= (cl-count ?\n str) 0)))
    (cond
     ((string-match "\\`@pytest.mark.parametrize([^\"]+\"\\([^\"]+\\)\",\\(.*?\\)[, ]*)\\'" str)
      (let* ((vars (match-string 1 str))
             (vals (match-string 2 str))
             (idx (lispy--python-nth vals)))
        (format "%s = %s[%s]\nprint((%s))" vars vals idx vars)))
     ((string-match-p "\"\"\"" str)
      str)
     ((string-match "^\\[" str)
      (format "__last__ = %s\n%s"
              str (lispy--python-print "__last__")))
     ((string-match "\\`\\(\\(?:\\sw\\|\\s_\\)+\\)\\'" str)
      (lispy--python-print (match-string 1 str)))
     ((and (or (string-match "\\`\\(\\(?:[.,* ]\\|\\sw\\|\\s_\\|[][]\\)+\\) += " str)
               (string-match "\\`\\(([^)]+)\\) *=[^=]" str)
               (string-match "\\`\\(\\(?:\\sw\\|\\s_\\)+ *\\[[^]]+\\]\\) *=[^=]" str)
               (string-match "\\`\\(\\(?:\\sw\\|\\s_\\)+\\)\\(:[^:=]+\\) *=" str))
           (save-match-data
             (or single-line-p
                 (and (not (string-match-p "lp\\." str))
                      (equal (ignore-errors
                               (lispy--eval-python
                                (format "x=lp.is_assignment(\"\"\"%s\"\"\")\nprint (x)" str)
                                t))
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
     ((string-match "\\`\\([^if].*\\) as \\(\\(?:\\sw\\|\\s_\\)+\\)\\'" str)
      (let ((val (match-string 1 str))
            (var (match-string 2 str)))
        (format "%s = %s.__enter__()" var val)))
     ((eq current-prefix-arg 2)
      (lispy--python-print str))
     (t
      str))))

(declare-function lpy-switch-to-shell "ext:lpy")

(defun lispy--eval-python-old (str &optional plain)
  "Eval STR as Python code."
  (let ((single-line-p (= (cl-count ?\n str) 0)))
    (unless plain
      (setq str (lispy--python-eval-string-dwim str))
      ;; bind __file__
      (when (string-match "__file__" str)
        (lispy--eval-python-plain (format "__file__ = '%s'\n" (buffer-file-name))))
      ;; eliminate return
      (when (and single-line-p (string-match "\\`return \\(.*\\)\\'" str))
        (setq str (match-string 1 str))))
    (let ((res
           (cond ((or single-line-p
                      (string-match "\n .*\\'" str)
                      (string-match "\"\"\"" str))
                  (replace-regexp-in-string
                   "" ""
                   (python-shell-send-string-no-output
                    str (lispy--python-proc))))
                 ;; split last line
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
        (sit-for 0.1)
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
          (if (cond ((string-match "\\`lp.pprint(\\(.*\\))\\'" last)
                     (setq str (match-string 1 last)))
                    ((string-match "\\`print(repr(\\(.*\\)))\\'" last)
                     (setq str (match-string 1 last))))
              (lispy--eval-python-plain (format "%s = list(%s)\nlp.pprint(%s)" str str str))
            (lispy--eval-python-plain (format "dbg = list(%s)\nlp.pprint(dbg)" str str str)))))
       ((string-match-p "SyntaxError:" res)
        (signal 'eval-error res))
       (t
        (replace-regexp-in-string "\\\\n" "\n" res))))))

(defun lispy--dict (&rest plist)
  (let (k v r)
    (while (setq k (pop plist))
      (setq v (pop plist))
      (push (format
             "'%s': %s"
             (cond ((keywordp k)
                    (substring (symbol-name k) 1))
                   ((stringp k)
                    k
                    k)
                   (t
                    (error "Unexpected")))
             (cond ((eq v 't)
                    "True")
                   ((eq v nil)
                    "None")
                   ((stringp v)
                    (format "'%s'" v))
                   (t
                    (prin1-to-string v)))) r))
    (concat "{"
            (mapconcat #'identity (nreverse r) ",")
            "}")))

(defun lispy--python-nth-1 (cands)
  (let ((len (length cands)))
    (read
     (ivy-read
      "idx: "
      (cl-mapcar
       (lambda (x i)
         (concat
          (number-to-string i)
          " "
          (cond ((listp x)
                 (mapconcat
                  (lambda (y) (if (stringp y) y (prin1-to-string y)))
                  x " "))
                ((stringp x)
                 x)
                (t
                 (prin1-to-string x)))))
       cands
       (number-sequence 0 (1- len)))))))

(defun lispy--eval-python-plain (str)
  (python-shell-send-string-no-output
   str (lispy--python-proc)))

(defun lispy--eval-python (str &optional use-in-expr)
  (setq lispy-eval-output nil)
  (let* ((echo (if (eq current-prefix-arg 2) nil t))
         (fstr
          (cond
           ((eq current-prefix-arg 3)
            (format "lp.eval_to_json(\"\"\"lp.select_item(\"%s\", 0)\"\"\")" str))
           ((or (string-match-p ".\n+." str) (string-match-p "\"\"\"" str))
            (let ((temp-file-name (python-shell--save-temp-file str)))
              (format "lp.eval_to_json('', %s)"
                      (lispy--dict
                       :code temp-file-name
                       :fname (buffer-file-name)
                       :echo echo))))
           (t
            (format "lp.eval_to_json(\"\"\"%s \"\"\", %s)"
                    (replace-regexp-in-string "\\\\n" "\\\\n" str nil t)
                    (lispy--dict :fname (buffer-file-name)
                                 :echo echo
                                 :use-in-expr use-in-expr)))))
         (rs (python-shell-send-string-no-output
              fstr
              (lispy--python-proc)))
         (extra-out (and (string-match "^{" rs) (substring rs 0 (match-beginning 0))))
         (res (json-parse-string
               (substring rs (match-beginning 0)) :object-type 'plist :null-object nil))
         (val (plist-get res :res))
         (binds (plist-get res :binds))
         (out (concat extra-out (plist-get res :out)))
         (err (plist-get res :err)))
    (when (eq current-prefix-arg 3)
      (kill-new fstr))
    (if err
        (signal 'eval-error (concat out err))
      (unless (equal out "")
        (setq lispy-eval-output
              (concat (propertize out 'face 'font-lock-string-face) "\n")))
      (cond
       ((string= val "'select'")
        (setq lispy-eval-output nil)
        (let* ((cands (read out))
               (idx (lispy--python-nth-1 cands)))
          (lispy--eval-python-plain
           (format "lp.select_item(\"\"\"%s\"\"\", %d)" str idx))))
       ((and val (not (string= val "'unset'")))
        (if (string-prefix-p "\"" val)
            (read val)
          val))
       (binds
        (mapconcat
         (lambda (x)
           (concat (substring (symbol-name (car x)) 1) " = " (cadr x)))
         (seq-partition binds 2)
         "\n"))
       (t
        "(ok)")))))

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
  (cond ((save-excursion
           (back-to-indentation)
           (looking-at "\\(import\\|from\\) .*"))
         (let* ((line (buffer-substring-no-properties
                       (line-beginning-position)
                       (point)))
                (str
                 (format "lp.jedi_completions('%s')" line))
                (cands
                 (lispy--python-array-to-elisp
                  (lispy--eval-python-plain str)))
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
                (keys (read (lispy--eval-python-plain
                             (format "lp.print_elisp(%s.keys())" str)))))
           (list (point) (point)
                 (if (> (length quote_str) 0)
                     keys
                   (mapcar
                    (lambda (s)
                      (if (stringp s)
                          (concat "\"" s "\"")
                        (prin1-to-string s)))
                    keys)))))
        ((lispy-complete-fname-at-point))
        (t
         (let* ((bnd (lispy-python-symbol-bnd))
                (str (buffer-substring-no-properties
                      (car bnd) (cdr bnd)))
                (str-com str))
           (when (string-match "\\`\\(.*\\)\\.[^.]*\\'" str)
             (let ((expr (format "__t__ = %s" (substring str 0 (match-end 1)))))
               (setq str-com (concat "__t__" (substring str (match-end 1))))
               (cl-incf (car bnd) (1+ (- (match-end 1) (match-beginning 0))))
               (lispy--eval-python-plain expr)))
           (list (car bnd)
                 (cdr bnd)
                 (let ((out (lispy--eval-python-plain
                             (format "lp.print_elisp(lp.get_completions('%s'))" str-com))))
                   (with-temp-buffer
                     (insert out)
                     (goto-char (point-min))
                     (when (re-search-forward "^(" nil t)
                       (goto-char (match-beginning 0)))
                     (read (current-buffer)))))))))

(defvar lispy--python-arg-key-re "\\`\\(\\(?:\\sw\\|\\s_\\)+\\)=\\([^\\0]+\\)\\'"
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
                    (not (looking-at ","))
                    (ignore-errors
                      (forward-sexp)
                      t)))
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
              (read (lispy--eval-python-plain
                     (format "lp.argspec(%s)" fn))))
             (fn-args
              (plist-get fn-data :args))
             (fn-varkw
              (plist-get fn-data :varkw))
             (fn-varargs
              (plist-get fn-data :varargs))
             (fn-keywords
              (plist-get fn-data :keywords))
             (fn-defaults
              (mapcar (lambda (x)
                        (cond ((stringp x)
                               x)
                              (x (prin1-to-string x))
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
                (cond ((setq arg-cell (assoc arg-name fn-alist))
                       (setcdr arg-cell arg-val))
                      (fn-keywords
                       (push (concat arg-name "=" arg-val) extra-keywords))
                      (fn-varkw
                       (push (cons arg-name arg-val) fn-alist))
                      (t
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
              (concat
               (format "lp.step_into_module_maybe(%s); "
                       (if method-p
                           (cdar fn-alist)
                         fn))
               "("
               (mapconcat #'car fn-alist ", ")
               ")=("
               (mapconcat #'cdr fn-alist ", ")

               ")"))
        (condition-case nil
            (lispy--eval-python-plain dbg-cmd)
          (error
           (lispy--eval-python
            (format "lp.step_in(%s,%s)" fn (buffer-substring-no-properties
                                            (1+ p-ar-beg) (1- p-ar-end))))))
        (let ((line (plist-get fn-data :line)))
          (unless (eq line 1)
            (goto-char orig-point)
            (when fn-data
              (set-text-properties
               0 1
               `(
                 filename ,(plist-get fn-data :filename)
                 line ,line)
               fn))))
        (let ((buf lispy-python-buf))
          (lispy-goto-symbol fn)
          (setq lispy-python-buf buf))))))

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

(defun lispy--python-goto-definition ()
  (save-buffer)
  (let ((definition (lispy--eval-python-plain
                     (format "lp.goto_definition('%s',%d,%d)"
                             (buffer-file-name)
                             (line-number-at-pos)
                             (current-column)))))
    (unless (string= definition "")
      (cl-destructuring-bind (fname line column) (read definition)
        (lispy--goto-symbol-python fname line)
        (forward-char column))
      t)))

(defun lispy-goto-symbol-python (symbol)
  (save-restriction
    (widen)
    (let ((file (get-text-property 0 'filename symbol))
          (line (get-text-property 0 'line symbol)))
      (if file
          (lispy--goto-symbol-python file line)
        (or (lispy--python-goto-definition)
            (let* ((symbol (or (python-info-current-symbol) symbol))
                   (r (ignore-errors
                        (lispy--eval-python-plain
                         (format "lp.argspec(%s)" symbol))))
                   (plist (and r (read r))))
              (cond (plist
                     (lispy--goto-symbol-python
                      (plist-get plist :filename)
                      (plist-get plist :line)))
                    ((and (equal file "None")
                          (let ((symbol-re
                                 (concat "^\\(?:def\\|class\\).*"
                                         (car (last (split-string symbol "\\." t))))))
                            (re-search-backward symbol-re nil t))))
                    (t
                     (error "Both jedi and inspect failed")))))))))

(defun lispy--python-docstring (symbol)
  "Look up the docstring for SYMBOL.

First, try to see if SYMBOL.__doc__ returns a string in the
current REPL session (dynamic).

Otherwise, fall back to Jedi (static)."
  (let ((dynamic-result (lispy--eval-python-plain (concat symbol ".__doc__"))))
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

(defun lispy--python-slurp (f)
  (with-temp-buffer
    (insert-file-contents f)
    (buffer-string)))

(defun lispy--python-setup-cmd ()
  (concat
   "from importlib.machinery import SourceFileLoader;"
   (format "lp=SourceFileLoader('lispy-python', '%s').load_module();"
           lispy--python-middleware-file)
   (format "lp.setup('%s')" lispy--python-init-file)))

(defun lispy--python-middleware-load ()
  "Load the custom Python code in \"lispy-python.py\"."
  (unless lispy--python-middleware-loaded-p
    (let* ((default-directory (or (locate-dominating-file default-directory ".git")
                                  default-directory))
           out)
      ;; send single line so that python.el does no /tmp/*.py magic, which does not work in Docker
      (setq out (lispy--eval-python-plain (lispy--python-setup-cmd)))
      (when (string-match "FileNotFoundError" out)
        (let* ((text (lispy--python-slurp lispy--python-middleware-file))
               (ben (replace-regexp-in-string "\n" "" (base64-encode-string text)))
               (setup-cmd (let ((lispy--python-middleware-file "/tmp/lispy.py"))
                            (lispy--python-setup-cmd))))
          (lispy--eval-python-plain
           (format "import base64; open('/tmp/lispy.py','w').write(base64.b64decode('%s').decode()); %s"
                   ben setup-cmd))))
      (setq lispy--python-middleware-loaded-p t))))

(defun lispy--python-arglist (symbol filename line column)
  (lispy--python-middleware-load)
  (let* ((boundp (ignore-errors (lispy--eval-python-plain symbol) t))
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
