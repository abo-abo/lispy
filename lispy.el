;;; lispy.el --- vi-like Paredit.

;; Copyright (C) 2014 Oleh Krehel

;; Author: Oleh Krehel <ohwoeowho@gmail.com>
;; URL: https://github.com/abo-abo/lispy
;; Version: 0.1
;; Keywords: lisp

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
;; Due to the structure of Lisp syntax it's very rare for the
;; programmer to want to insert characters right before "(" or right
;; after ")".  Thus unprefixed printable characters can be used to call
;; commands when the point is at one of these locations, which are
;; further referred to as special.
;;
;; Conveniently, when located at special position it's very clear to
;; which sexp the list-manipulating command will be applied to, what
;; the result be and where the point should end up afterwards.  You
;; can enhance this effect with `show-paren-mode' or similar.
;;
;; Here's an illustration to this effect, with `lispy-clone' ("*"
;; represents the point):
;; |--------------------+-----+--------------------|
;; | before             | key | after              |
;; |--------------------+-----+--------------------|
;; |  (looking-at "(")* |  c  |  (looking-at "(")  |
;; |                    |     |  (looking-at "(")* |
;; |--------------------+-----+--------------------|
;; | *(looking-at "(")  |  c  | *(looking-at "(")  |
;; |                    |     |  (looking-at "(")  |
;; |--------------------+-----+--------------------|
;;
;; When special, the digit keys call `digit-argument' which is very
;; convenient since most Lispy commands accept a numeric argument.
;; For instance, "3c" is equivalent to "ccc" (clone sexp 3 times), and
;; "4j" is equivalent to "jjjj" (move point 4 sexps down).  Some
;; useful applications are "9l" and "9a" - they exit list forwards and
;; backwards respectively at most 9 times which makes them effectively
;; equivalent to `end-of-defun' and `beginning-of-defun'.
;;
;; To move the point into a special position, use:
;; "]" - calls `lispy-forward'
;; "[" - calls `lispy-backward'
;; ")" - calls `lispy-out-forward' (exit current list forwards)
;;
;; These are the few Lispy commands that don't care whether the point
;; is special or not.  Other such bindings are `DEL', `C-d', `C-k'.
;;
;; To get out of the special position, you can use any of the good-old
;; navigational commands such as `C-f' or `C-n'.
;; Additionally `SPC' will break out of special to get around the
;; situation when you have the point between open parens like this
;; "(|(" and want to start inserting.  `SPC' will change the code to
;; this: "(| (".
;;
;; A lot of Lispy commands come in pairs: one reverses the other.
;; Some examples are:
;; |-----+--------------------------+------------+-------------------|
;; | key | command                  | key        | command           |
;; |-----+--------------------------+------------+-------------------|
;; | j   | `lispy-down'             | k          | `lispy-up'        |
;; | s   | `lispy-move-down'        | w          | `lispy-move-up'   |
;; | o   | `lispy-counterclockwise' | p          | `lispy-clockwise' |
;; | >   | `lispy-slurp'            | <          | `lispy-barf'      |
;; | c   | `lispy-clone'            | C-d or DEL |                   |
;; | C   | `lispy-convolute'        | C          | reverses itself   |
;; | d   | `lispy-different'        | d          | reverses itself   |
;; | M-j | `lispy-split'            | +          | `lispy-join'      |
;; | O   | `lispy-oneline'          | M          | `lispy-multiline' |
;; |-----+--------------------------+------------+-------------------|
;;
;; Among other cool commands are:
;; |-----+------------------------------------|
;; | key | command                            |
;; |-----+------------------------------------|
;; | f   | `lispy-flow'                       |
;; | u   | `undo'                             |
;; | e   | `lispy-eval'                       |
;; | m   | `lispy-mark'                       |
;; | ;   | `lispy-comment'                    |
;; | l   | `lispy-out-forward'                |
;; | a   | `lispy-out-backward'               |
;; | E   | `lispy-eval-and-insert'            |
;; | /   | `lispy-splice'                     |
;; | i   | `indent-sexp'                      |
;; | r   | `lispy-raise'                      |
;; | R   | `lispy-raise-some'                 |
;; | J   | `outline-next-visible-heading'     |
;; | K   | `outline-previous-visible-heading' |
;; | g   | `lispy-goto'                       |
;; | q   | `lispy-ace-paren'                  |
;; |-----+------------------------------------|
;;
;; Most special commands will leave the point special after they're
;; done.  This allows to chain them as well as apply them
;; continuously by holding the key.  Some useful holdable keys are
;; "jkopf<>cws;".
;; Not so useful, but fun is "/": start it from "|(" position and hold
;; until all your Lisp code is turned into Python :).
;;
;;; Code:
(eval-when-compile
  (require 'cl))
(require 'help-fns)
(require 'edebug)
(require 'outline)

;; ——— Customization ———————————————————————————————————————————————————————————
(defgroup lispy nil
  "List navigation and editing for the Lisp family."
  :group 'bindings
  :prefix "lispy-")

(defvar lispy-no-space nil
  "If t, don't insert a space before parens/brackets/braces/colons.")
(make-variable-buffer-local 'lispy-no-space)

(defvar lispy-mode-map (make-sparse-keymap))

;;;###autoload
(define-minor-mode lispy-mode
    "Minor mode for navigating and editing Lisp.

When `lispy-mode' is on, brackets move forward and backward through
lists.

Commands are provided to insert (), [], {}, \"\".

:, ^, RET, SPC are re-defined to provide appropriate whitespace,
besides inserting.

C-k, C-y, C-j, C-d, DEL, C-e and ; are re-defined as well.

Various unprefixed keys are special when positioned at \"(\" or behind
\")\". Otherwise they insert themselves.
For instance, > inserts itself, or grows current list, depending on
the context.

\\{lispy-mode-map}"
  :keymap lispy-mode-map
  :group 'lispy
  :lighter " LY")

;; ——— Macros ——————————————————————————————————————————————————————————————————
(defmacro dotimes-protect (n &rest bodyform)
  "Execute N times the BODYFORM unless an error is signaled.
Return nil couldn't execute BODYFORM at least once.
Otherwise return t."
  `(let ((i 0)
         out)
     (ignore-errors
       (while (<= (incf i) ,n)
         ,@bodyform
         (setq out t)))
     out))

(defmacro lispy-save-excursion (&rest body)
  "More intuitive (`save-excursion' BODY)."
  `(let ((out (save-excursion
                ,@body)))
     (when (bolp)
       (back-to-indentation))
     out))

;; ——— Globals: navigation —————————————————————————————————————————————————————
(defun lispy-forward (arg)
  "Move forward list ARG times or until error.
Return t if moved at least once,
otherwise call `lispy-out-forward' and return nil."
  (interactive "p")
  (when (= arg 0)
    (setq arg 2000))
  (lispy--exit-string)
  (let ((pt (point))
        (r (dotimes-protect arg
             (forward-list))))
    ;; `forward-list' returns true at and of buffer
    (if (or (null r)
            (= pt (point))
            (and (not (looking-back ")"))
                 (progn
                   (backward-list)
                   (forward-list)
                   (= pt (point)))))
        (prog1 nil
          (lispy-out-forward 1))
      t)))

(defun lispy-backward (arg)
  "Move backward list ARG times or until error.
If couldn't move backward at least once, move up backward and return nil."
  (interactive "p")
  (when (= arg 0)
    (setq arg 2000))
  (lispy--exit-string)
  (let ((pt (point))
        (r (dotimes-protect arg
             (backward-list))))
    ;; `backward-list' returns true at beginning of buffer
    (if (or (null r)
            (= pt (point))
            (and (not (looking-at "("))
                 (progn
                   (forward-list)
                   (backward-list)
                   (= pt (point)))))
        (prog1 nil
          (lispy-out-forward 1)
          (backward-list))
      t)))

(defun lispy-out-forward (arg)
  "Move outside list forwards ARG times.
Return nil on failure, t otherwise."
  (interactive "p")
  (lispy--exit-string)
  (catch 'break
    (dotimes (i arg)
      (if (ignore-errors (up-list) t)
          (progn
            (lispy--remove-gaps)
            (indent-for-tab-command))
        (when (looking-at "(")
          (forward-list))
        (throw 'break nil)))
    t))

(defun lispy-out-backward (arg)
  "Move outside list forwards ARG times.
Return nil on failure, t otherwise."
  (interactive "p")
  (lispy-out-forward arg)
  (if (looking-back ")")
      (lispy-backward 1)
    (error "Unexpected")))

(defun lispy-out-forward-newline (arg)
  "Call `lispy-out-forward', then ARG times `newline-and-indent'."
  (interactive "p")
  (lispy-out-forward 1)
  (dotimes-protect arg
    (newline-and-indent)))

;; ——— Locals:  navigation —————————————————————————————————————————————————————
(defun lispy-flow (arg)
  "Move inside list ARG times.
Don't enter strings or comments.
Return nil if can't move."
  (interactive "p")
  (let ((pt (point))
        success)
    (dotimes-protect arg
      (cond ((looking-at "(")
             (forward-char)
             (re-search-forward "(" nil t)
             (while (and (lispy--in-string-or-comment-p)
                         (re-search-forward "(" nil t)))
             (unless (lispy--in-string-or-comment-p)
               (setq success t))
             (backward-char))

            ((looking-back ")")
             (backward-char)
             (re-search-backward ")" nil t)
             (while (and (lispy--in-string-or-comment-p)
                         (re-search-backward ")" nil t)))
             (unless (lispy--in-string-or-comment-p)
               (setq success t))
             (forward-char))))
    (and (not (= (point) pt))
         (or success
             (prog1 nil
               (goto-char pt))))))

(defun lispy-counterclockwise ()
  "Move counterclockwise inside current list."
  (interactive)
  (let ((pt (point)))
    (cond ((looking-at "(")
           (lispy-forward 2)
           (lispy-backward 1))

          ((looking-back ")")
           (lispy-backward 2)
           (lispy-forward 1)))
    (when (= pt (point))
      (if (looking-at "(")
          (lispy-forward 1)
        (lispy-backward 1)))))

(defun lispy-clockwise ()
  "Move clockwise inside current list."
  (interactive)
  (let ((pt (point)))
    (cond ((looking-at "(")
           (or (lispy-backward 1)
               (progn
                 (goto-char pt)
                 (forward-list))))

          ((looking-back ")")
           (if (looking-at ")")
               (lispy-backward 1)
             (unless (lispy-forward 1)
               (goto-char pt)
               (lispy-backward 1)))))))

(defun lispy-down (arg)
  "Move down ARG times inside current list."
  (interactive "p")
  (cond ((looking-at "(")
         (lispy-forward arg)
         (let ((pt (point)))
           (if (lispy-forward 1)
               (lispy-backward 1)
             (goto-char pt))))

        ((looking-back ")")
         (let ((pt (point)))
           (unless (lispy-forward arg)
             (goto-char pt)
             (lispy-backward 1))))

        (t
         (lispy-forward 1)
         (lispy-backward 1))))

(defun lispy-up (arg)
  "Move up ARG times inside current list."
  (interactive "p")
  (cond ((looking-at "(")
         (let ((pt (point)))
           (unless (lispy-backward arg)
             (goto-char pt)
             (lispy-forward 1))))

        ((looking-back ")")
         (lispy-backward arg)
         (let ((pt (point)))
           (if (lispy-backward 1)
               (lispy-forward 1)
             (goto-char pt))))
        (t
         (lispy-backward 1)
         (lispy-forward 1))))

(defun lispy-different ()
  "Switch to the different side of current sexp."
  (interactive)
  (cond ((looking-at "(")
         (forward-list))
        ((looking-back ")")
         (backward-list))
        (t (error "Unexpected"))))

;; ——— Globals: kill, yank, delete, mark, copy —————————————————————————————————
(defun lispy-kill ()
  "Kill keeping parens consistent."
  (interactive)
  (cond ((lispy--in-string-p)
         (let ((end (cdr (lispy--bounds-string))))
           (if (> end (line-end-position))
               (kill-line)
             (delete-region (point)
                            (1- end)))))
        ((looking-at " *\n")
         (progn
           (delete-region
            (match-beginning 0)
            (match-end 0))
           (indent-for-tab-command)))
        (t (let* ((beg (point))
                  (str (buffer-substring-no-properties
                        beg
                        (line-end-position))))
             (if (= (cl-count ?\( str)
                    (cl-count ?\) str))
                 (kill-line)
               (if (lispy-out-forward 1)
                   (progn
                     (backward-char 1)
                     (kill-region beg (point)))
                 (goto-char beg)
                 (lispy-delete 1)))))))

(defun lispy-yank ()
  "Like regular `yank', but quotes body when called from \"|\"."
  (interactive)
  (if (and (eq (char-after) ?\")
           (eq (char-before) ?\"))
      (insert (replace-regexp-in-string "\"" "\\\\\"" (current-kill 0)))
    (yank)))

(defun lispy-delete (arg)
  "Delete ARG sexps."
  (interactive "p")
  (if (and (looking-at "(") (not (lispy--in-string-or-comment-p)))
      (progn
        (dotimes-protect arg
          (lispy--delete))
        (unless (looking-at "(")
          (when (looking-at " +")
            (delete-region
             (match-beginning 0)
             (match-end 0)))
          (lispy-out-forward 1)
          (backward-list)))
    (delete-char arg)))

(defun lispy-delete-backward (arg)
  "From \")|\", delete ARG sexps backwards.
Otherwise (`backward-delete-char-untabify' ARG)."
  (interactive "p")
  (if (and (looking-back ")") (not (lispy--in-string-or-comment-p)))
      (let ((pt (point)))
        (lispy-backward arg)
        (delete-region pt (point))
        (lispy--remove-gaps)
        (unless (looking-back ")")
          (lispy-backward 1)
          (forward-list)))
    (backward-delete-char-untabify arg)))

(defun lispy-mark ()
  "Mark the quoted string or the list that includes the point.
Extend region when it's aleardy active."
  (interactive)
  (let ((bounds (or (lispy--bounds-string)
                    (lispy--bounds-list))))
    (when bounds
      (set-mark (car bounds))
      (goto-char (cdr bounds)))))

(defun lispy-mark-symbol ()
  "Mark current symbol."
  (interactive)
  (cond ((or (looking-at "[ ]*[()]")
             (and (region-active-p)
                  (looking-at "[ \n]*[()]")))
         (skip-chars-forward "() \n")
         (set-mark-command nil)
         (re-search-forward "[() \n]")
         (while (lispy--in-string-or-comment-p)
           (re-search-forward "[() \n]"))
         (backward-char 1))

        ((looking-back ")")
         (skip-chars-backward "() \n")
         (set-mark-command nil)
         (re-search-backward "[() \n]")
         (while (lispy--in-string-or-comment-p)
           (re-search-backward "[() \n]"))
         (forward-char 1))

        ((region-active-p)
         (ignore-errors
           (forward-sexp)))
        (t
         (destructuring-bind (beg . end) (lispy--bounds-dwim)
           (goto-char beg)
           (set-mark-command nil)
           (goto-char end)))))

(defun lispy-pop-copy (arg)
  "Pop to mark and copy current list there.
With ARG not nil, cut instead of copying."
  (interactive "P")
  (lispy-out-forward 1)
  (let* ((beg (point))
         (end (progn
                (lispy-backward 1)
                (point)))
         (sexp
          (buffer-substring-no-properties
           beg end)))
    (when arg
      (delete-region beg end)
      (lispy--remove-gaps))
    (set-mark-command 4)
    (insert sexp)))

(defun lispy-kill-at-point ()
  "Kill the quoted string or the list that includes the point."
  (interactive)
  (if (region-active-p)
      (progn
        (kill-new (buffer-substring-no-properties
                   (region-beginning)
                   (region-end)))
        (newline-and-indent)
        (insert (current-kill 0)))
    (let ((bounds (or (lispy--bounds-string)
                      (lispy--bounds-list))))
      (kill-region (car bounds) (cdr bounds)))))

;; ——— Globals: pairs ——————————————————————————————————————————————————————————
(defun lispy-pair (left right space-unless)
  "Return (lambda (arg)(interactive \"p\")...) using LEFT RIGHT SPACE-UNLESS.
When this function is called:
- with region active:
  Wrap region with LEFT RIGHT.
- with arg 1:
  Insert LEFT RIGHT.
- else:
  Wrap current sexp with LEFT RIGHT."
  `(lambda (arg)
     (interactive "p")
     (cond
       ((region-active-p)
        (lispy--surround-region ,left ,right))
       ((and (lispy--in-string-or-comment-p)
             (progn
               (self-insert-command 1)
               (or (looking-back ,(regexp-quote left))
                   (looking-back ,(regexp-quote right))
                   (looking-back "}")
                   (prog1 nil
                     (backward-delete-char 1))))))
       ((= arg 1)
        (indent-for-tab-command)
        (lispy--space-unless ,space-unless)
        (insert ,left ,right)
        (unless (or (lispy--in-string-p)
                    (looking-at "\n\\|)\\|}\\|\\]"))
          (just-one-space)
          (backward-char 1))
        (when (looking-at ,(regexp-quote left))
          (insert " ")
          (backward-char))
        (backward-char))
       (t
        (insert ,left " ")
        (forward-sexp)
        (insert ,right)
        (backward-sexp)
        (indent-sexp)
        (forward-char 1)))))

(defalias 'lispy-parens
    (lispy-pair "(" ")" "^\\|\\s-\\|lambda\\|\\[\\|[(`'#@~_%,]")
  "`lispy-pair' with ().")

(defalias 'lispy-brackets
    (lispy-pair "[" "]" "\\s-\\|\\s(\\|[']")
  "`lispy-pair' with [].")

(defalias 'lispy-braces
    (lispy-pair "{" "}" "\\s-\\|\\s(\\|[{#^']")
  "`lispy-pair' with {}.")

(defun lispy-quotes (arg)
  "Insert a pair of quotes around the point.

Quotes are quoted when inside a string, unless ARG isn't nil.
When the region is active, wrap it in quotes instead."
  (interactive "P")
  (cond ((region-active-p)
         (lispy--surround-region "\"" "\""))

        ((and (lispy--in-string-p) (not arg))
         (insert "\\\"\\\"")
         (backward-char 2))

        (t
         (lispy--space-unless "\\s-\\|\\s(\\|[#]")
         (insert "\"\"")
         (backward-char))))

(defun lispy-parens-down ()
  "Exit the current sexp, and start a new sexp below."
  (interactive)
  (condition-case nil
      (progn
        (lispy-out-forward 1)
        (if (looking-at "\n *\\()\\)")
            (progn
              (goto-char (match-beginning 1))
              (insert "()")
              (indent-for-tab-command)
              (backward-char))

          (insert "\n()")
          (indent-for-tab-command)
          (backward-char)))
    (error (indent-new-comment-line))))

;; ——— Globals: insertion ——————————————————————————————————————————————————————
(defun lispy-space ()
  "Insert one space.
Special case is (|( -> ( |(."
  (interactive)
  (if (and (featurep 'edebug) edebug-active)
      (edebug-step-mode)
    (insert " ")
    (when (and (looking-at "(")
               (looking-back "( "))
      (backward-char))))

(defun lispy-colon ()
  "Insert :."
  (interactive)
  (lispy--space-unless "\\s-\\|\\s(\\|[:^?]")
  (insert ":"))

(defun lispy-hat ()
  "Insert ^."
  (interactive)
  (lispy--space-unless "\\s-\\|\\s(\\|[:?]")
  (insert "^"))

(defun lispy-newline-and-indent ()
  "Insert newline."
  (interactive)
  (if (eq major-mode 'lisp-interaction-mode)
      (ignore-errors
        (setq this-command 'eval-last-sexp)
        (eval-print-last-sexp))
    (newline-and-indent)
    (when (looking-at "(")
      (indent-sexp))))

;; ——— Locals:  Paredit transformations ————————————————————————————————————————
(defun lispy-slurp (arg)
  "Grow current sexp by ARG sexps."
  (interactive "p")
  (cond ((or (looking-at "()")
             (and (not (looking-back "()")) (looking-at "(")))
         (dotimes-protect arg
           (lispy--slurp-backward))
         (lispy--reindent))

        ((looking-back ")")
         (dotimes-protect arg
           (lispy--slurp-forward))
         (lispy--reindent))))

(defun lispy-barf (arg)
  "Shrink current sexp by ARG sexps."
  (interactive "p")
  (cond ((looking-at "()"))

        ((looking-back ")")
         (dotimes-protect arg
           (lispy--barf-backward))
         (save-excursion
           (lispy-out-forward 1)
           (lispy--reindent)))

        ((looking-at "(")
         (dotimes-protect arg
           (lispy--barf-forward)))))

(defun lispy-splice ()
  "Splice sexp into containing list."
  (interactive)
  (let ((bnd (lispy--bounds-dwim)))
    (cond ((looking-at "(")
           (save-excursion
             (goto-char (cdr bnd))
             (lispy--remove-gaps)
             (backward-delete-char 1))
           (delete-char 1)
           (if (lispy-forward 1)
               (lispy-backward 1)
             (lispy-backward 1)
             (lispy-flow 1)))

          ((looking-back ")")
           (lispy--remove-gaps)
           (backward-delete-char 1)
           (goto-char (car bnd))
           (delete-char 1)
           (if (lispy-backward 1)
               (lispy-forward 1)
             (lispy-forward 1)
             (lispy-flow 1))))))

(defun lispy-raise ()
  "Use current sexp or region as replacement for its parent."
  (interactive)
  (let* ((regionp (region-active-p))
         (at-end (and regionp
                      (= (point) (region-end))))
         bnd1 bnd2)
    (setq bnd1 (lispy--bounds-dwim))
    (lispy-out-forward 1)
    (setq bnd2 (lispy--bounds-dwim))
    (unless regionp
      (goto-char (car bnd2)))
    (delete-region (cdr bnd2) (cdr bnd1))
    (delete-region (car bnd2) (car bnd1))
    (if regionp
        (progn
          (indent-region (car bnd2) (point))
          (unless at-end
            (goto-char (car bnd2)))
          (lispy--reindent 1)
          (or (looking-at "(") (looking-back ")") (lispy-out-forward 1)))
      (indent-sexp))))

(defun lispy-raise-some ()
  "Use current sexps as replacement for their parent."
  (interactive)
  (let ((pt (point)))
    (cond ((region-active-p))

          ((looking-at "(")
           (lispy-out-forward 1)
           (backward-char 1)
           (set-mark (point))
           (goto-char pt))

          ((looking-back ")")
           (lispy-out-forward 1)
           (backward-list)
           (forward-char 1)
           (set-mark (point))
           (goto-char pt))

          (t (error "Unexpected")))
    (lispy-raise)))

(defun lispy-convolute ()
  "Convolute sexp."
  (interactive)
  (if (save-excursion
        (lispy-out-forward 2))
      (lispy-save-excursion
       (set-mark (point))
       (lispy-out-forward 1)
       (backward-list)
       (call-interactively 'kill-region)
       (lispy-out-forward 1)
       (backward-list)
       (yank)
       (lispy-out-forward 1)
       (lispy--reindent))
    (error "Not enough depth to convolute")))

(defun lispy-join ()
  "Join sexps."
  (interactive)
  (let ((pt (point)))
    (cond ((looking-back ")")
           (when (lispy-forward 1)
             (backward-list)
             (delete-char 1)
             (goto-char pt)
             (backward-delete-char 1)
             (lispy-out-forward 1)
             (lispy--reindent 1)))
          ((looking-at "(")
           (when (lispy-backward 1)
             (forward-list)
             (backward-delete-char 1)
             (goto-char (1- pt))
             (delete-char 1)
             (lispy-out-forward 1)
             (backward-list)
             (indent-sexp))))))

(defun lispy-split ()
  "Split sexps."
  (interactive)
  (cond ((lispy--in-comment-p)
         (indent-new-comment-line))
        ((lispy--in-string-p)
         (insert "\"\"")
         (backward-char)
         (newline-and-indent))
        (t
         (when (looking-back " +")
           (delete-region (match-beginning 0)
                          (match-end 0)))
         (insert ")(")
         (backward-char)
         (newline-and-indent))))

;; ——— Locals:  more transformations ———————————————————————————————————————————
(defun lispy-move-up ()
  "Move current expression up.  Don't exit parent list."
  (interactive)
  (if (and (looking-at "(")
           (save-excursion
             (lispy-backward 1)))
      (let (b1 b2 s1 s2)
        (setq s1 (lispy--string-dwim (setq b1 (lispy--bounds-dwim))))
        (lispy-backward 1)
        (setq s2 (lispy--string-dwim (setq b2 (lispy--bounds-dwim))))
        (delete-region (car b1) (cdr b1))
        (save-excursion
          (goto-char (car b1))
          (insert s2))
        (delete-region (car b2) (cdr b2))
        (insert s1)
        (backward-list))))

(defun lispy-move-down ()
  "Move current expression down.  Don't exit parent list."
  (interactive)
  (if (and (looking-at "(")
           (save-excursion
             (and (lispy-forward 1)
                  (lispy-forward 1))))
      (let (b1 b2 s1 s2)
        (setq s1 (lispy--string-dwim (setq b1 (lispy--bounds-dwim))))
        (lispy-counterclockwise)
        (setq s2 (lispy--string-dwim (setq b2 (lispy--bounds-dwim))))
        (delete-region (car b2) (cdr b2))
        (insert s1)
        (goto-char (car b1))
        (delete-region (car b1) (cdr b1))
        (insert s2)
        (lispy-forward 1)
        (backward-list))))

(defun lispy-clone (arg)
  "Clone sexp ARG times."
  (interactive "p")
  (let ((str (lispy--string-dwim)))
    (cond ((looking-at "(")
           (save-excursion
             (dotimes-protect arg
               (insert str)
               (newline-and-indent))))
          ((looking-back ")")
           (dotimes-protect arg
             (newline-and-indent)
             (insert str)))
          (t (error "Unexpected")))))

(defun lispy-oneline ()
  "Squeeze current sexp into one line.
Comments will be moved ahead of sexp."
  (interactive)
  (let (str bnd)
    (setq str (lispy--string-dwim (setq bnd (lispy--bounds-dwim))))
    (save-excursion
      (delete-region (car bnd) (cdr bnd))
      (let ((no-comment "")
            comments)
        (loop for s in (split-string str "\n" t)
           do (if (string-match "^ *\\(;\\)" s)
                  (push (substring s (match-beginning 1)) comments)
                (setq no-comment (concat no-comment "\n" s))))
        (when comments
          (insert (mapconcat #'identity comments "\n") "\n"))
        (insert (substring (replace-regexp-in-string "\n *" " " no-comment) 1))))
    ;; work around `( and '(
    (lispy-forward 1)
    (lispy-backward 1)))

(defun lispy-multiline ()
  "Spread current sexp over multiple lines."
  (interactive)
  (let ((pt (point)))
    (lispy-forward 1)
    (while (and (> (point) pt) (lispy-flow 1))
      (unless (looking-at ")\\|\n")
        (when (looking-at " *")
          (replace-match "\n")
          (backward-char 1))))
    (goto-char pt)
    (indent-sexp)))

(defun lispy-comment (&optional arg)
  "Comment ARG sexps."
  (interactive "p")
  (setq arg (or arg 1))
  (dotimes-protect arg
    (let (bnd)
      (cond ((lispy--in-string-or-comment-p)
             (self-insert-command 1))
            ((region-active-p)
             (comment-dwim nil)
             (when (lispy--in-string-or-comment-p)
               (lispy-out-backward 1)))
            ((looking-at "(")
             (setq bnd (lispy--bounds-dwim))
             (lispy-counterclockwise)
             (comment-region (car bnd) (cdr bnd))
             (when (lispy--in-string-or-comment-p)
               (lispy-out-backward 1)))
            ((looking-back ")")
             (comment-dwim nil)
             (insert " "))
            ((setq bnd (save-excursion
                         (and (lispy-out-forward 1)
                              (point))))
             (comment-region (point) (1- bnd))
             (lispy-out-backward 1))
            (t (self-insert-command 1))))))

(defvar lispy-meol-point 1
  "Point where `lispy-move-end-of-line' should go when already at eol.")

(defun lispy-move-end-of-line ()
  "Forward to `move-end-of-line' unless already at end of line.
Then return to the point where it was called last.
If this point is inside string, move outside string."
  (interactive)
  (let ((pt (point)))
    (if (eq pt (line-end-position))
        (if (lispy--in-string-p)
            (goto-char (cdr (lispy--bounds-string)))
          (when (and (< lispy-meol-point pt)
                     (>= lispy-meol-point (line-beginning-position)))
            (goto-char lispy-meol-point)
            (when (lispy--in-string-p)
              (goto-char (cdr (lispy--bounds-string))))))
      (setq lispy-meol-point (point))
      (move-end-of-line 1))))

(defun lispy-string-oneline ()
  "Convert current string to one line."
  (interactive)
  (when (looking-back "\"")
    (backward-char 1))
  (let (bnd str)
    (setq str (lispy--string-dwim (setq bnd (lispy--bounds-string))))
    (delete-region (car bnd) (cdr bnd))
    (insert (replace-regexp-in-string "\n" "\\\\n" str))))

(defun lispy-stringify (&optional arg)
  "Transform current sexp into a string.
Quote newlines if ARG isn't 1."
  (interactive "p")
  (setq arg (or arg 1))
  (let (str bnd)
    (setq str
          (replace-regexp-in-string
           "\"" "\\\\\""
           (replace-regexp-in-string
            "\\\\" "\\\\\\\\"
            (lispy--string-dwim (setq bnd (lispy--bounds-dwim))))))
    (unless (= arg 1)
      (setq str (replace-regexp-in-string "\n" "\\\\n" str)))
    (save-excursion
      (delete-region (car bnd) (cdr bnd))
      (insert "\""
              str
              "\""))
    ;; work around `( and '(
    (lispy-forward 1)
    (lispy-backward 1)))

;; ——— Locals:  miscellanea ————————————————————————————————————————————————————
(defun lispy-eval ()
  "Eval last sexp."
  (interactive)
  (call-interactively 'eval-last-sexp))

(defun lispy-eval-and-insert ()
  "Eval last sexp and insert the result."
  (interactive)
  (when (> (current-column) 40)
    (newline-and-indent))
  (insert (prin1-to-string (call-interactively 'eval-last-sexp))))

(defun lispy-goto ()
  "Jump to symbol entry point."
  (interactive)
  (cond ((and (featurep 'function-args)
              (moo-jump-local)))
        ((and (featurep 'helm-semantic)
              (helm-semantic)))
        (t (error "No known jumping features provided"))))

(defun lispy-ace-char ()
  "Call `ace-jump-char-mode' on current defun."
  (interactive)
  (let ((bnd (save-excursion
               (lispy-out-backward 50)
               (lispy--bounds-dwim))))
    (narrow-to-region (car bnd) (cdr bnd))
    (call-interactively 'ace-jump-char-mode)
    (widen)))

(defun lispy-ace-paren ()
  "Use `ace-jump-char-mode' to jump to ( within current defun."
  (interactive)
  (let ((bnd (save-excursion
               (lispy-out-backward 50)
               (lispy--bounds-dwim))))
    (unless (and (> (car bnd) (window-start))
                 (< (cdr bnd) (window-end)))
      (recenter-top-bottom))
    (narrow-to-region (car bnd) (cdr bnd))
    (ace-jump-char-mode ?\()
    (widen)))

(defun lispy-ert ()
  "Call (`ert' t)"
  (interactive)
  (ert t))

;; ——— Predicates ——————————————————————————————————————————————————————————————
(defun lispy--in-string-p ()
  "Test if point is inside a string."
  (let ((beg (nth 8 (syntax-ppss))))
    (and beg
         (eq (char-after beg) ?\"))))

(defun lispy--in-comment-p ()
  "Test if point is inside a comment."
  (let ((beg (nth 8 (syntax-ppss))))
    (and beg
         (comment-only-p beg (point)))))

(defun lispy--in-string-or-comment-p ()
  "Test if point is inside a string or a comment."
  (let ((beg (nth 8 (syntax-ppss))))
    (and beg
         (or (eq (char-after beg) ?\")
             (comment-only-p beg (point))))))

;; ——— Pure ————————————————————————————————————————————————————————————————————
(defun lispy--bounds-dwim ()
  "Return a cons of region bounds if it's active.
Otherwise return cons of current string, symbol or list bounds."
  (cond ((region-active-p)
         (cons (region-beginning)
               (region-end)))
        ((looking-at "(\\|\"")
         (bounds-of-thing-at-point 'sexp))
        ((looking-back ")")
         (backward-list)
         (prog1 (bounds-of-thing-at-point 'sexp)
           (forward-list)))
        ((looking-back "\"")
         (backward-sexp)
         (prog1 (bounds-of-thing-at-point 'sexp)
           (forward-sexp)))
        (t
         (or
          (ignore-errors
            (bounds-of-thing-at-point 'symbol))
          (ignore-errors
            (bounds-of-thing-at-point 'sentence))))))

(defun lispy--bounds-list ()
  "Return the bounds of smallest list that includes the point.
First, try to return `lispy--bounds-string'."
  (save-excursion
    (when (memq (char-after) '(?\( ?\[ ?\{))
      (forward-char))
    (ignore-errors
      (let (beg end)
        (up-list)
        (setq end (point))
        (backward-list)
        (setq beg (point))
        (cons beg end)))))

(defun lispy--bounds-string ()
  "Return bounds of current string."
  (let ((beg (or (nth 8 (syntax-ppss))
                 (and (memq (char-after (point))
                            '(?\" ?\'))
                      (point)))))
    (when beg
      (cons beg (save-excursion
                  (goto-char beg)
                  (forward-sexp)
                  (point))))))

(defun lispy--string-dwim (&optional bounds)
  "Return the string that corresponds to BOUNDS.
`lispy--bounds-dwim' is used if BOUNDS is nil."
  (destructuring-bind (beg . end) (or bounds (lispy--bounds-dwim))
    (buffer-substring-no-properties beg end)))

;; ——— Utilities ———————————————————————————————————————————————————————————————
(defun lispy--exit-string ()
  "When in string, go to its beginning."
  (let ((s (syntax-ppss)))
    (when (nth 3 s)
      (goto-char (nth 8 s)))))

(defun lispy--remove-gaps ()
  "Remove dangling `\\s)'."
  (when (or (looking-back "[^ \t\n]\\([ \t\n]+\\)\\s)")
            (looking-back "\\s)\\([ \t\n]+\\)")
            (looking-at "\\([\t\n ]*\\))"))
    (unless (save-excursion
              (save-match-data
                (goto-char (match-beginning 1))
                (lispy--in-string-or-comment-p)))
      (delete-region (match-beginning 1)
                     (match-end 1)))))

(defun lispy--surround-region (alpha omega)
  "Surround active region with ALPHA and OMEGA and re-indent."
  (let ((beg (region-beginning))
        (end (region-end)))
    (goto-char end)
    (insert omega)
    (goto-char beg)
    (insert alpha)
    (indent-region beg (+ 2 end))))

(defun lispy--space-unless (context)
  "Insert one space.
Unless inside string or comment, or `looking-back' at CONTEXT."
  (unless (or lispy-no-space (lispy--in-string-or-comment-p)
              (looking-back context))
    (insert " ")))

(defun lispy--slurp-forward ()
  "Grow current sexp forward by one sexp."
  (unless (looking-back "[()])")
    (just-one-space)
    (backward-char 1))
  (let ((pt (point))
        (beg (save-excursion (backward-list) (point))))
    (when (ignore-errors
            (forward-sexp) t)
      (delete-region (1- pt) pt)
      (insert ")"))))

(defun lispy--slurp-backward ()
  "Grow current sexp backward by one sexp."
  (let ((pt (point)))
    (backward-sexp)
    (delete-region pt (1+ pt))
    (insert "(")
    (backward-char)))

(defun lispy--barf-forward ()
  "Shrink current sexp forward by one sexp."
  (let ((pt (point)))
    (unless (looking-at "()")
      (forward-char)
      (forward-sexp)
      (delete-region pt (1+ pt))
      (skip-chars-forward " \n	")
      (insert "(")
      (backward-char)
      (indent-region pt (point))
      (lispy--reindent 1))))

(defun lispy--barf-backward ()
  "Shrink current sexp backward by one sexp."
  (let ((pt (point)))
    (unless (looking-back "()")
      (backward-char)
      (backward-sexp)
      (skip-chars-backward " \n	")
      (while (lispy--in-comment-p)
        (goto-char (comment-beginning))
        (skip-chars-backward " \n	"))
      (delete-region (1- pt) pt)
      (insert ")")
      (indent-region (point) pt))))

(defun lispy--reindent (&optional arg)
  "Reindent current sexp.  Up-list ARG times before that."
  (cond (arg
         (lispy-save-excursion
          (lispy-out-forward arg)
          (backward-list)
          (indent-sexp)))

        ((looking-back ")")
         (save-excursion
           (backward-list)
           (indent-sexp)))

        ((looking-at "(")
         (indent-sexp))

        (t
         (save-excursion
           (lispy-out-forward 1)
           (backward-list)
           (indent-sexp)))))

(defun lispy--delete ()
  "Delete one sexp."
  (unless (looking-at "(")
    (error "Bad position"))
  (let ((bnd (lispy--bounds-list)))
    (delete-region (car bnd) (cdr bnd))
    (if (looking-at "[\n ]+")
        (delete-region (match-beginning 0)
                       (match-end 0))
      (just-one-space)
      (when (looking-back "( ")
        (backward-delete-char 1)))
    (lispy-forward 1)
    (backward-list)))

(defun lispy--insert-or-call (def &optional from-start)
  "Return a lambda to call DEF if position is special.
Otherwise call `self-insert-command'.
When FROM-START is t, call DEF as if it was invoked from beginning of
list."
  `(lambda ,(help-function-arglist def)
     ,(format "Call `%s' when special, self-insert otherwise.\n\n%s"
              (symbol-name def) (documentation def))
     ,(interactive-form def)
     (cond ((region-active-p)
            (call-interactively ',def))

           ((lispy--in-string-or-comment-p)
            (self-insert-command 1))

           ((looking-at "(")
            (call-interactively ',def))

           ((looking-back ")")
            ,@(and from-start '((backward-list)))
            (unwind-protect
                 (call-interactively ',def)
              ,@(and from-start '((forward-list)))))

           ((or (and (looking-back "^ *") (looking-at ";"))
                (and (= (point) (point-max))
                     (memq ',def `(outline-previous-visible-heading))))
            (call-interactively ',def))

           (t
            (self-insert-command 1)))))

(defvar ac-trigger-commands '(self-insert-command))

(defun lispy-define-key (keymap key def &optional from-start)
  "Forward to (`define-key' KEYMAP KEY (`lispy-defun' DEF FROM-START))."
  (let ((func (defalias (intern (concat "special-" (symbol-name def)))
                  (lispy--insert-or-call def from-start))))
    (unless (member func ac-trigger-commands)
      (push func ac-trigger-commands))
    (define-key keymap (kbd key) func)))

(let ((map lispy-mode-map))
  ;; ——— globals: navigation ——————————————————
  (define-key map (kbd "]")   'lispy-forward)
  (define-key map (kbd "[")   'lispy-backward)
  (define-key map (kbd ")")   'lispy-out-forward)
  (define-key map (kbd "C-9") 'lispy-out-forward-newline)
  ;; ——— locals: navigation ———————————————————
  (lispy-define-key map "n" 'lispy-forward)
  (lispy-define-key map "h" 'lispy-backward)
  (lispy-define-key map "l" 'lispy-out-forward)
  (lispy-define-key map "a" 'lispy-out-backward)
  (lispy-define-key map "f" 'lispy-flow)
  (lispy-define-key map "j" 'lispy-down)
  (lispy-define-key map "k" 'lispy-up)
  (lispy-define-key map "d" 'lispy-different)
  (lispy-define-key map "o" 'lispy-counterclockwise)
  (lispy-define-key map "p" 'lispy-clockwise)
  (lispy-define-key map "J" 'outline-next-visible-heading)
  (lispy-define-key map "K" 'outline-previous-visible-heading)
  ;; ——— globals: kill-related ————————————————
  (define-key map (kbd "C-k") 'lispy-kill)
  (define-key map (kbd "C-y") 'lispy-yank)
  (define-key map (kbd "C-d") 'lispy-delete)
  (define-key map (kbd "DEL") 'lispy-delete-backward)
  (define-key map (kbd "M-m") 'lispy-mark-symbol)
  (define-key map (kbd "M-o") 'lispy-string-oneline)
  (define-key map (kbd "C-c p") 'lispy-pop-copy)
  (define-key map (kbd "C-,") 'lispy-kill-at-point)
  (define-key map (kbd "C-M-,") 'lispy-mark)
  ;; ——— globals: pairs ———————————————————————
  (define-key map (kbd "(")   'lispy-parens)
  (define-key map (kbd "{")   'lispy-braces)
  (define-key map (kbd "}")   'lispy-brackets)
  (define-key map (kbd "\"")  'lispy-quotes)
  ;; ——— globals: insertion ———————————————————
  (define-key map (kbd "C-8") 'lispy-parens-down)
  (define-key map (kbd "SPC") 'lispy-space)
  (define-key map (kbd ":")   'lispy-colon)
  (define-key map (kbd "^")   'lispy-hat)
  (define-key map (kbd "C-j") 'lispy-newline-and-indent)
  (define-key map (kbd "M-j") 'lispy-split)
  (define-key map (kbd "RET") 'newline-and-indent)
  (define-key map (kbd ";")   'lispy-comment)
  (define-key map (kbd "C-e") 'lispy-move-end-of-line)
  ;; ——— locals: Paredit transformations ——————
  (lispy-define-key map ">" 'lispy-slurp)
  (lispy-define-key map "<" 'lispy-barf)
  (lispy-define-key map "/" 'lispy-splice)
  (lispy-define-key map "r" 'lispy-raise t)
  (lispy-define-key map "R" 'lispy-raise-some)
  (lispy-define-key map "+" 'lispy-join)
  ;; ——— locals: more transformations —————————
  (lispy-define-key map "C" 'lispy-convolute t)
  (lispy-define-key map "w" 'lispy-move-up t)
  (lispy-define-key map "s" 'lispy-move-down t)
  (lispy-define-key map "O" 'lispy-oneline t)
  (lispy-define-key map "M" 'lispy-multiline t)
  (lispy-define-key map "S" 'lispy-stringify)
  ;; ——— locals: miscellanea ——————————————————
  (lispy-define-key map "i" 'indent-sexp t)
  (lispy-define-key map "c" 'lispy-clone)
  (lispy-define-key map "e" 'lispy-eval)
  (lispy-define-key map "E" 'lispy-eval-and-insert)
  (lispy-define-key map "m" 'lispy-mark)
  (lispy-define-key map "u" 'undo)
  (lispy-define-key map "g" 'lispy-goto)
  (lispy-define-key map "Q" 'lispy-ace-char)
  (lispy-define-key map "q" 'lispy-ace-paren)
  (lispy-define-key map "T" 'lispy-ert)
  ;; ——— locals: digit argument ———————————————
  (mapc (lambda (x) (lispy-define-key map (format "%d" x) 'digit-argument))
        (number-sequence 0 9)))

(declare-function moo-jump-local "ext:function-args")
(declare-function helm-semantic "ext:helm-semantic")
(declare-function ace-jump-char-mode "ext:ace-jump-mode")

(provide 'lispy)

;;; Local Variables:
;;; outline-regexp: ";; ———"
;;; End:

;;; lispy.el ends here
