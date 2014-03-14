;;; lispy.el --- vi-like Paredit.

;; Copyright (C) 2014 Oleh Krehel

;; Author: Oleh Krehel <ohwoeowho@gmail.com>
;; URL: https://github.com/abo-abo/lispy
;; Version: 0.9.0
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
;; "C-3" - calls `lispy-out-forward' (exit current list forwards)
;; ")" - calls `lispy-out-forward-nostring' (exit current list
;;       forwards, but self-insert in strings and comments)
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
;; | S   | `lispy-stringify'        | C-u "      | `lispy-quotes'    |
;; | ;   | `lispy-comment'          | C-u ;      | `lispy-comment'   |
;; | xi  | `lispy-to-ifs'           | xc         | `lispy-to-cond'   |
;; |-----+--------------------------+------------+-------------------|
;;
;; Here's a list of commands for inserting pairs:
;; |-----+------------------------------------|
;; | key | command                            |
;; |-----+------------------------------------|
;; |  (  | `lispy-parens'                     |
;; |  {  | `lispy-braces'                     |
;; |  }  | `lispy-brackets'                   |
;; |  "  | `lispy-quotes'                     |
;; |-----+------------------------------------|
;;
;; Here's a list of modified insertion commands that handle whitespace
;; in addition to self-inserting:
;; |-----+------------------------------------|
;; | key | command                            |
;; |-----+------------------------------------|
;; | SPC | `lispy-space'                      |
;; |  :  | `lispy-colon'                      |
;; |  ^  | `lispy-hat'                        |
;; | C-m | `lispy-newline-and-indent'         |
;; |-----+------------------------------------|
;;
;; Among other cool commands are:
;; |-------+----------------------------------|
;; | key   | command                          |
;; |-------+----------------------------------|
;; | f     | `lispy-flow'                     |
;; | u     | `lispy-undo'                     |
;; | e     | `lispy-eval'                     |
;; | m     | `lispy-mark-list'                |
;; | l     | `lispy-out-forward'              |
;; | a     | `lispy-out-backward'             |
;; | E     | `lispy-eval-and-insert'          |
;; | /     | `lispy-splice'                   |
;; | i     | `indent-sexp'                    |
;; | r     | `lispy-raise'                    |
;; | R     | `lispy-raise-some'               |
;; | J     | `lispy-outline-next'             |
;; | K     | `lispy-outline-prev'             |
;; | g     | `lispy-goto'                     |
;; | G     | `lispy-goto-local'               |
;; | q     | `lispy-ace-paren'                |
;; | h     | `lispy-ace-symbol'               |
;; | Q     | `lispy-ace-char'                 |
;; | D     | `lispy-describe'                 |
;; | F     | `lispy-follow'                   |
;; | N     | `lispy-normalize'                |
;; | C-1   | `lispy-describe-inline'          |
;; | C-2   | `lispy-arglist-inline'           |
;; | C-7   | `lispy-cursor-down'              |
;; | C-8   | `lispy-parens-down'              |
;; | C-e   | `lispy-move-end-of-line'         |
;; | C-k   | `lispy-kill'                     |
;; | C-y   | `lispy-yank'                     |
;; | C-d   | `lispy-delete'                   |
;; | DEL   | `lispy-delete-backward'          |
;; | C-,   | `lispy-kill-at-point'            |
;; | C-M-, | `lispy-mark'                     |
;; | M-m   | `lispy-mark-symbol'              |
;; | v     | `lispy-view'                     |
;; | n     | `lispy-new-copy'                 |
;; | xd    | `lispy-to-defun'                 |
;; | xl    | `lispy-to-lambda'                |
;; | xm    | `lispy-cursor-ace'               |
;; | xe    | `edebug-defun'                   |
;; |-------+----------------------------------|
;;
;; Most special commands will leave the point special after they're
;; done.  This allows to chain them as well as apply them
;; continuously by holding the key.  Some useful holdable keys are
;; "jkopf<>cws;".
;; Not so useful, but fun is "/": start it from "|(" position and hold
;; until all your Lisp code is turned into Python :).
;;
;; Some Clojure support depends on `cider'.
;; Some Scheme support depends on `geiser'.
;; Some Common Lisp support depends on `slime'.
;; You can get them from MELPA.
;;
;; See http://abo-abo.github.io/lispy/ for a detailed documentation.
;;
;;; Code:

;; ——— Requires ————————————————————————————————————————————————————————————————
(eval-when-compile
  (require 'cl)
  (require 'noflet)
  (require 'org))
(require 'help-fns)
(require 'edebug)
(require 'ediff)
(require 'ediff-util)
(require 'eldoc)
(require 'outline)
(require 'semantic)
(require 'semantic/db)
(require 'ace-jump-mode)
(require 'newcomment)
(require 'lispy-inline)

(require 'iedit)

;; ——— Customization ———————————————————————————————————————————————————————————
(defgroup lispy nil
  "List navigation and editing for the Lisp family."
  :group 'bindings
  :prefix "lispy-")

(defvar lispy-left "[([{]"
  "Opening delimiter.")

(defvar lispy-right "[])}]"
  "Closing delimiter.")

(defvar lispy-no-space nil
  "If t, don't insert a space before parens/brackets/braces/colons.")
(make-variable-buffer-local 'lispy-no-space)

(defcustom lispy-lax-eval t
  "If t, fix \"unbound variable\" error by setting the unbound variable to nil.
This is useful when hacking functions with &optional arguments.
So evaling (setq mode (or mode major-mode)) will set mode to nil on
the first eval, and to major-mode on the second eval."
  :type 'boolean
  :group 'lispy)

(defcustom lispy-use-ctrl-digits t
  "If t, lispy will bind C-1 .. C-9."
  :type 'boolean
  :group 'lispy)

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
  (declare (indent 1))
  `(let ((i 0)
         out)
     (ignore-errors
       (while (<= (incf i) ,n)
         ,@bodyform
         (setq out t)))
     out))

(defmacro lispy-save-excursion (&rest body)
  "More intuitive (`save-excursion' BODY)."
  (declare (indent 0))
  `(let ((out (save-excursion
                ,@body)))
     (when (bolp)
       (back-to-indentation))
     out))

(defmacro lispy-from-left (&rest body)
  "Ensure that BODY is executed from start of list."
  (let ((at-start (cl-gensym "at-start")))
    `(let ((,at-start (looking-at lispy-left)))
       (unless ,at-start
         (lispy-backward 1))
       (unwind-protect
            (save-excursion
              ,@body)
         (unless ,at-start
           (forward-list 1))))))

;; ——— Globals: navigation —————————————————————————————————————————————————————
(defun lispy-forward (arg)
  "Move forward list ARG times or until error.
Return t if moved at least once,
otherwise call `lispy-out-forward' and return nil."
  (interactive "p")
  (when (= arg 0)
    (setq arg 2000))
  (lispy--exit-string)
  (let ((bnd (lispy--bounds-comment)))
    (when bnd
      (goto-char (1+ (cdr bnd)))))
  (let ((pt (point))
        (r (dotimes-protect arg
             (forward-list))))
    ;; `forward-list' returns true at and of buffer
    (if (or (null r)
            (= pt (point))
            (and (not (looking-back lispy-right))
                 (progn
                   (backward-list)
                   (forward-list)
                   (= pt (point)))))
        (prog1 nil
          (lispy--out-forward 1))
      t)))

(defun lispy-backward (arg)
  "Move backward list ARG times or until error.
If couldn't move backward at least once, move up backward and return nil."
  (interactive "p")
  (when (= arg 0)
    (setq arg 2000))
  (lispy--exit-string)
  (let ((bnd (lispy--bounds-comment)))
    (when bnd
      (goto-char (car bnd))))
  (let ((pt (point))
        (r (dotimes-protect arg
             (backward-list))))
    ;; `backward-list' returns true at beginning of buffer
    (if (or (null r)
            (= pt (point))
            (and (not (looking-at lispy-left))
                 (progn
                   (forward-list)
                   (backward-list)
                   (= pt (point)))))
        (prog1 nil
          (lispy--out-forward 1)
          (backward-list))
      t)))

(defun lispy-out-forward (arg)
  "Move outside list forwards ARG times.
Return nil on failure, t otherwise."
  (interactive "p")
  (if (region-active-p)
      (dotimes-protect arg
        (when (save-excursion (ignore-errors (up-list) t))
          (let* ((at-start (= (point) (region-beginning)))
                 (bnd1 (lispy--bounds-dwim))
                 (str1 (lispy--string-dwim bnd1))
                 pt)
            (delete-region (car bnd1) (cdr bnd1))
            (cond ((looking-at " *;"))
                  ((and (looking-at "\n")
                        (looking-back "^ *"))
                   (delete-blank-lines))
                  ((looking-at "\\([\n ]+\\)[^\n ;]")
                   (delete-region (match-beginning 1)
                                  (match-end 1))))
            (up-list)
            (lispy--remove-gaps)
            (if (looking-at " *;")
                (progn
                  (forward-line 1)
                  (newline-and-indent)
                  (forward-line -1))
              (newline-and-indent))
            (setq pt (point))
            (insert str1)
            (indent-region pt (point))
            (setq deactivate-mark)
            (set-mark pt)
            (when at-start
              (exchange-point-and-mark)))))
    (lispy--out-forward arg)))

(defun lispy-out-forward-nostring (arg)
  "Call `lispy--out-forward' with ARG unless in string or comment.
Self-insert otherwise."
  (interactive "p")
  (if (or (lispy--in-string-or-comment-p)
          (looking-back "?\\\\"))
      (self-insert-command arg)
    (lispy--out-forward arg)))

(defun lispy-out-backward (arg)
  "Move outside list forwards ARG times.
Return nil on failure, t otherwise."
  (interactive "p")
  (if (region-active-p)
      (dotimes-protect arg
        (when (save-excursion (ignore-errors (up-list) t))
          (let* ((at-start (= (point) (region-beginning)))
                 (bnd1 (lispy--bounds-dwim))
                 (str1 (lispy--string-dwim bnd1))
                 pt)
            (delete-region (car bnd1) (cdr bnd1))
            (cond ((looking-at " *;"))
                  ((and (looking-at "\n")
                        (looking-back "^ *"))
                   (delete-blank-lines))
                  ((looking-at "\\([\n ]+\\)[^\n ;]")
                   (delete-region (match-beginning 1)
                                  (match-end 1))))
            (up-list)
            (backward-list)
            (lispy--normalize 0)
            (lispy--remove-gaps)
            (setq pt (point))
            (insert str1)
            (newline-and-indent)
            (skip-chars-backward " \n")
            (indent-region pt (point))
            (setq deactivate-mark)
            (set-mark pt)
            (when at-start
              (exchange-point-and-mark)))))
    (lispy--out-backward arg)))

(defun lispy-out-forward-newline (arg)
  "Call `lispy--out-forward', then ARG times `newline-and-indent'."
  (interactive "p")
  (lispy--out-forward 1)
  (dotimes-protect arg
    (newline-and-indent)))

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

;; ——— Locals:  navigation —————————————————————————————————————————————————————
(defun lispy-flow (arg)
  "Move inside list ARG times.
Don't enter strings or comments.
Return nil if can't move."
  (interactive "p")
  (let ((pt (point))
        success)
    (dotimes-protect arg
      (cond ((looking-at lispy-left)
             (forward-char)
             (re-search-forward lispy-left nil t)
             (while (and (lispy--in-string-or-comment-p)
                         (re-search-forward lispy-left nil t)))
             (unless (lispy--in-string-or-comment-p)
               (setq success t))
             (backward-char))

            ((looking-back lispy-right)
             (backward-char)
             (re-search-backward lispy-right nil t)
             (while (and (lispy--in-string-or-comment-p)
                         (re-search-backward lispy-right nil t)))
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
    (cond ((looking-at lispy-left)
           (lispy-forward 2)
           (lispy-backward 1))

          ((looking-back lispy-right)
           (lispy-backward 2)
           (lispy-forward 1)))
    (when (= pt (point))
      (if (looking-at lispy-left)
          (lispy-forward 1)
        (lispy-backward 1)))))

(defun lispy-clockwise ()
  "Move clockwise inside current list."
  (interactive)
  (let ((pt (point)))
    (cond ((looking-at lispy-left)
           (or (lispy-backward 1)
               (progn
                 (goto-char pt)
                 (forward-list))))

          ((looking-back lispy-right)
           (if (looking-at lispy-right)
               (lispy-backward 1)
             (unless (lispy-forward 1)
               (goto-char pt)
               (lispy-backward 1)))))))

(defun lispy-down (arg)
  "Move down ARG times inside current list."
  (interactive "p")
  (lispy--ensure-visible)
  (cond ((region-active-p)
         (dotimes-protect arg
           (if (= (point) (region-beginning))
               (progn
                 (forward-sexp 1)
                 (skip-chars-forward " \n"))
             (forward-sexp 1))))

        ((looking-at lispy-left)
         (lispy-forward arg)
         (let ((pt (point)))
           (if (lispy-forward 1)
               (lispy-backward 1)
             (goto-char pt))))

        ((looking-back lispy-right)
         (let ((pt (point)))
           (unless (lispy-forward arg)
             (goto-char pt)
             (lispy-backward 1))))

        (t
         (lispy-forward 1)
         (lispy-backward 1)))
  (lispy--ensure-visible))

(defun lispy-up (arg)
  "Move up ARG times inside current list."
  (interactive "p")
  (lispy--ensure-visible)
  (cond ((region-active-p)
         (dotimes-protect arg
           (if (= (point) (region-beginning))
               (backward-sexp 1)
             (progn
               (backward-sexp 1)
               (skip-chars-backward " \n")))))

        ((looking-at lispy-left)
         (let ((pt (point)))
           (unless (lispy-backward arg)
             (goto-char pt)
             (lispy-forward 1))))

        ((looking-back lispy-right)
         (lispy-backward arg)
         (let ((pt (point)))
           (if (lispy-backward 1)
               (lispy-forward 1)
             (goto-char pt))))

        (t
         (lispy-backward 1)
         (lispy-forward 1)))
  (lispy--ensure-visible))

(defun lispy-different ()
  "Switch to the different side of current sexp."
  (interactive)
  (cond ((and (region-active-p)
              (not (= (region-beginning) (region-end))))
         (exchange-point-and-mark))
        ((looking-at lispy-left)
         (forward-list))
        ((looking-back lispy-right)
         (backward-list))
        (t
         (error "Unexpected"))))

(defun lispy-outline-next (arg)
  "Call `outline-next-visible-heading' ARG times."
  (interactive "p")
  (dotimes-protect arg
    (let ((pt (point)))
      (outline-next-visible-heading 1)
      (unless (looking-at outline-regexp)
        (goto-char pt)
        (error "Past last outline")))))

(defun lispy-outline-prev (arg)
  "Call `outline-previous-visible-heading' ARG times."
  (interactive "p")
  (dotimes-protect arg
    (let ((pt (point)))
      (outline-previous-visible-heading 1)
      (unless (looking-at outline-regexp)
        (goto-char pt)
        (error "Past first outline")))))

;; ——— Globals: kill, yank, delete, mark, copy —————————————————————————————————
(defun lispy-kill ()
  "Kill keeping parens consistent."
  (interactive)
  (cond ((or (lispy--in-comment-p) (looking-at ";"))
         (kill-line))

        ((lispy--in-string-p)
         (let ((end (cdr (lispy--bounds-string))))
           (if (> end (line-end-position))
               (kill-line)
             (delete-region (point)
                            (1- end)))))
        ((looking-at " *\n")
         (delete-region
          (match-beginning 0)
          (match-end 0))
         (lispy--indent-for-tab))
        ((and (looking-at lispy-right) (looking-back lispy-left))
         (delete-char 1)
         (backward-delete-char 1))
        (t
         (let* ((beg (point))
                (str (buffer-substring-no-properties
                      beg
                      (line-end-position))))
           (if (= (cl-count ?\( str)
                  (cl-count ?\) str))
               (kill-line)
             (if (lispy--out-forward 1)
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
  (cond ((region-active-p)
         (delete-region
          (region-beginning)
          (region-end)))

        ((lispy--in-string-p)
         (cond ((looking-at "\\\\\"")
                (delete-char 2))
               ((lispy--delete-pair-in-string "\\\\\\\\(" "\\\\\\\\)"))
               ((save-excursion
                  (forward-char 1)
                  (lispy--in-string-or-comment-p))
                (delete-char arg))
               (t
                (lispy--exit-string))))

        ((lispy--in-comment-p)
         (delete-char arg))

        ((looking-at lispy-left)
         (dotimes-protect arg (lispy--delete))
         (unless (looking-at lispy-left)
           (when (looking-at " +")
             (delete-region (match-beginning 0)
                            (match-end 0)))
           (lispy--out-forward 1)
           (backward-list)))

        ((looking-at "\"")
         (kill-sexp)
         (lispy--remove-gaps)
         (when (lispy-forward 1)
           (backward-list)))

        ((looking-at lispy-right)
         (forward-char 1)
         (lispy-delete-backward 1))

        (t
         (delete-char arg))))

(defun lispy-delete-backward (arg)
  "From \")|\", delete ARG sexps backwards.
Otherwise (`backward-delete-char-untabify' ARG)."
  (interactive "p")
  (let (pos)
    (cond ((region-active-p)
           (delete-region (region-beginning)
                          (region-end)))

          ((lispy--in-string-p)
           (cond ((save-excursion
                    (backward-char 1)
                    (not (lispy--in-string-p)))
                  (lispy--exit-string)
                  (forward-sexp))
                 ((or (looking-back "\\\\\\\\(")
                      (looking-back "\\\\\\\\)"))
                  (let ((pt (point)))
                    (goto-char (match-beginning 0))
                    (unless (lispy--delete-pair-in-string
                             "\\\\\\\\(" "\\\\\\\\)")
                      (goto-char pt)
                      (backward-delete-char-untabify arg))))
                 (t
                  (backward-delete-char-untabify arg))))

          ((lispy--in-comment-p)
           (backward-delete-char-untabify arg))

          ((and (looking-back lispy-right) (not (looking-back "\\\\.")))
           (let ((pt (point)))
             (lispy-backward arg)
             (delete-region pt (point))
             (unless (looking-back "^ *")
               (just-one-space))
             (lispy--remove-gaps)
             (unless (looking-back lispy-right)
               (lispy-backward 1)
               (forward-list))))

          ((and (looking-back lispy-left) (not (looking-back "\\\\.")))
           (lispy--out-forward 1)
           (lispy-delete-backward 1))

          ((looking-back "\"")
           (backward-sexp 1)
           (cl-destructuring-bind (beg . end)
               (bounds-of-thing-at-point 'sexp)
             (delete-region beg end))
           (just-one-space)
           (lispy--remove-gaps)
           (unless (looking-back lispy-right)
             (when (lispy-forward 1)
               (backward-list))))

          (t
           (backward-delete-char-untabify arg)))))

(defun lispy-mark ()
  "Mark the quoted string or the list that includes the point.
Extend region when it's aleardy active."
  (interactive)
  (let ((bounds (or (lispy--bounds-comment)
                    (lispy--bounds-string)
                    (lispy--bounds-list))))
    (when bounds
      (set-mark (car bounds))
      (goto-char (cdr bounds)))))

(defun lispy-mark-list ()
  "Mark list from special position."
  (interactive)
  (cond ((region-active-p)
         (deactivate-mark))
        ((looking-at lispy-left)
         (set-mark (point))
         (forward-list))
        ((looking-back lispy-right)
         (set-mark (point))
         (backward-list))))

(defun lispy-mark-symbol ()
  "Mark current symbol."
  (interactive)
  (cond ((lispy--in-comment-p)
         (lispy--mark (lispy--bounds-comment)))
        ((or (looking-at "[ ]*[()]")
             (and (region-active-p)
                  (looking-at "[ \n]*[()]")))
         (skip-chars-forward "() \n")
         (set-mark-command nil)
         (re-search-forward "[() \n]")
         (while (lispy--in-string-or-comment-p)
           (re-search-forward "[() \n]"))
         (backward-char 1))

        ((looking-back lispy-right)
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
         (lispy--mark (lispy--bounds-dwim)))))

(defun lispy-pop-copy (arg)
  "Pop to mark and copy current list there.
With ARG not nil, cut instead of copying."
  (interactive "P")
  (lispy--out-forward 1)
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
    (let ((bounds (or (lispy--bounds-comment)
                      (lispy--bounds-string)
                      (lispy--bounds-list))))
      (kill-region (car bounds) (cdr bounds)))))

(defun lispy-new-copy ()
  "Copy marked region or sexp to kill ring."
  (interactive)
  (let ((str (lispy--string-dwim)))
    (unless (string= str (current-kill 0))
      (kill-new str))))

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
     (cond ((region-active-p)
            (lispy--surround-region ,left ,right))
           ((and (lispy--in-string-p)
                 (looking-back "\\\\\\\\"))
            (insert ,left "\\\\" ,right)
            (backward-char 3))
           ((lispy--in-string-or-comment-p)
            (if (and (string= ,left "(")
                     (= ?\( (aref (this-command-keys-vector) 0)))
                (insert "(")
              (insert ,left ,right)
              (backward-char 1)))
           ((looking-back "?\\\\")
            (self-insert-command 1))
           ((= arg 1)
            (lispy--indent-for-tab)
            (lispy--space-unless ,space-unless)
            (insert ,left ,right)
            (unless (or (eolp)
                        (lispy--in-string-p)
                        (looking-at "\n\\|)\\|}\\|\\]"))
              (just-one-space)
              (backward-char 1))
            (when (looking-at ,(regexp-quote left))
              (insert " ")
              (backward-char))
            (backward-char))
           (t
            (insert ,left)
            (unless (looking-at " ")
              (insert " "))
            (forward-sexp)
            (insert ,right)
            (backward-sexp)
            (skip-chars-forward "'`#")
            (indent-sexp)
            (forward-char 1)))))

(defalias 'lispy-parens
    (lispy-pair "(" ")" "^\\|\\s-\\|\\[\\|[(`'#@~_%,]")
  "`lispy-pair' with ().")

(defalias 'lispy-brackets
    (lispy-pair "[" "]" "^\\|\\s-\\|\\s(\\|[']")
  "`lispy-pair' with [].")

(defalias 'lispy-braces
    (lispy-pair "{" "}" "^\\|\\s-\\|\\s(\\|[{#^']")
  "`lispy-pair' with {}.")

(defun lispy-quotes (arg)
  "Insert a pair of quotes around the point.

When the region is active, wrap it in quotes instead.
When inside string, if ARG is nil quotes are quoted,
otherwise the whole string is unquoted."
  (interactive "P")
  (cond ((region-active-p)
         (lispy--surround-region "\"" "\""))

        ((lispy--in-string-p)
         (if arg
             (let* ((bnd (lispy--bounds-string))
                    (str (lispy--string-dwim bnd)))
               (delete-region (car bnd) (cdr bnd))
               (insert (read str)))
           (insert "\\\"\\\"")
           (backward-char 2)))

        (arg
         (lispy-stringify))

        (t
         (lispy--space-unless "^\\|\\s-\\|\\s(\\|[#]")
         (insert "\"\"")
         (unless (or (lispy--in-string-p)
                     (looking-at "\n\\|)\\|}\\|\\]"))
           (just-one-space)
           (backward-char 1))
         (backward-char))))

(defun lispy-parens-down ()
  "Exit the current sexp, and start a new sexp below."
  (interactive)
  (condition-case nil
      (progn
        (lispy--out-forward 1)
        (if (looking-at "\n *\\()\\)")
            (progn
              (goto-char (match-beginning 1))
              (insert "()")
              (lispy--indent-for-tab)
              (backward-char))

          (insert "\n()")
          (lispy--indent-for-tab)
          (backward-char)))
    (error (indent-new-comment-line))))

;; ——— Globals: insertion ——————————————————————————————————————————————————————
(defun lispy-space ()
  "Insert one space.
Special case is (|( -> ( |(."
  (interactive)
  (if (region-active-p)
      (progn
        (goto-char (region-end))
        (deactivate-mark)
        (insert " "))
    (insert " ")
    (when (and (looking-at lispy-left)
               (looking-back "( "))
      (backward-char))))

(defvar lispy-colon-no-space-regex
  '((lisp-mode . "\\s-\\|[:^?#]\\|\\(?:\\s([[:word:]-]+\\)"))
  "Overrides REGEX that `lispy-colon' will consider for `major-mode'.
`lispy-colon' will insert \" :\" instead of \" \" unless
`lispy-no-space' is t or `looking-back' REGEX.")

(defun lispy-colon ()
  "Insert :."
  (interactive)
  (lispy--space-unless
   (or (cdr (assoc major-mode lispy-colon-no-space-regex))
       "\\s-\\|\\s(\\|[#:^?]"))
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
      (progn
        (setq this-command 'eval-last-sexp)
        (eval-print-last-sexp))
    (newline-and-indent)
    (when (looking-at lispy-left)
      (indent-sexp))))

;; ——— Globals: miscellanea ————————————————————————————————————————————————————
(defun lispy-string-oneline ()
  "Convert current string to one line."
  (interactive)
  (when (looking-back "\"")
    (backward-char 1))
  (let (bnd str)
    (setq str (lispy--string-dwim (setq bnd (lispy--bounds-string))))
    (delete-region (car bnd) (cdr bnd))
    (insert (replace-regexp-in-string "\n" "\\\\n" str))))

(defun lispy-iedit ()
  "Wrap around `iedit'"
  (interactive)
  (iedit-mode 0))

;; ——— Locals:  Paredit transformations ————————————————————————————————————————
(defun lispy-slurp (arg)
  "Grow current sexp by ARG sexps."
  (interactive "p")
  (cond ((region-active-p)
         (let ((bnd (lispy--bounds-dwim))
               (endp (= (point) (region-end)))
               deactivate-mark)
           (let ((pt (save-excursion
                       (when (lispy-forward 1)
                         (lispy-backward 1)
                         (point)))))
             (when pt
               (goto-char pt)
               (lispy--teleport (car bnd) (cdr bnd) endp t)))))
        ((or (looking-at "()")
             (and (looking-at lispy-left) (not (looking-back "()"))))
         (dotimes-protect arg
           (lispy--slurp-backward)))
        ((looking-back lispy-right)
         (dotimes-protect arg
           (lispy--slurp-forward))))
  (unless (region-active-p)
    (lispy--reindent)))

(defun lispy-barf (arg)
  "Shrink current sexp by ARG sexps."
  (interactive "p")
  (cond ((region-active-p)
         (let ((bnd (lispy--bounds-dwim))
               (endp (= (point) (region-end)))
               (bsize (buffer-size))
               deactivate-mark)
           (let ((pt (save-excursion
                       (goto-char (car bnd))
                       (when (lispy-backward 1)
                         (lispy-forward 1)
                         (backward-char 1)
                         (setq deactivate-mark nil)
                         (newline-and-indent)
                         (point)))))
             (when pt
               (goto-char pt)
               (decf bsize (buffer-size))
               (backward-char 1)
               (lispy--teleport (- (car bnd) bsize)
                                (- (cdr bnd) bsize)
                                endp t)))))

        ((looking-at "()"))

        ((looking-back lispy-right)
         (dotimes-protect arg
           (lispy--barf-backward))
         (save-excursion
           (lispy--out-forward 1)
           (lispy--reindent)))

        ((looking-at lispy-left)
         (dotimes-protect arg
           (lispy--barf-forward)))))

(defun lispy-splice (arg)
  "Splice ARG sexps into containing list."
  (interactive "p")
  (dotimes-protect arg
    (let ((bnd (lispy--bounds-dwim)))
      (cond ((looking-at lispy-left)
             (save-excursion
               (goto-char (cdr bnd))
               (lispy--remove-gaps)
               (backward-delete-char 1))
             (delete-char 1)
             (if (lispy-forward 1)
                 (lispy-backward 1)
               (lispy-backward 1)
               (lispy-flow 1)))

            ((looking-back lispy-right)
             (lispy--remove-gaps)
             (backward-delete-char 1)
             (goto-char (car bnd))
             (delete-char 1)
             (if (lispy-backward 1)
                 (lispy-forward 1)
               (lispy-forward 1)
               (lispy-flow 1)))))))

(defun lispy-raise ()
  "Use current sexp or region as replacement for its parent."
  (interactive)
  (let* ((regionp (region-active-p))
         (at-end (and regionp
                      (= (point) (region-end))))
         bnd1 bnd2)
    (if regionp
        (let ((deactivate-mark nil))
          (when at-end (lispy-different))
          (save-excursion
            (lispy--reindent 1)))
      (lispy--reindent 1))
    (setq bnd1 (lispy--bounds-dwim))
    (lispy--out-forward 1)
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
          (or (looking-at lispy-left)
              (looking-back lispy-right)
              (lispy--out-forward 1)))
      (indent-sexp))))

(defun lispy-raise-some ()
  "Use current sexps as replacement for their parent.
The outcome when ahead of sexps is different from when behind."
  (interactive)
  (let ((pt (point)))
    (cond ((region-active-p))

          ((looking-at lispy-left)
           (lispy--out-forward 1)
           (backward-char 1)
           (set-mark (point))
           (goto-char pt))

          ((looking-back lispy-right)
           (lispy--out-forward 1)
           (backward-list)
           (forward-char 1)
           (set-mark (point))
           (goto-char pt))

          (t
           (error "Unexpected")))
    (lispy-raise)))

;; TODO add numeric arg: 1 is equivalent to prev behavior 2 will raise containing list twice.
(defun lispy-convolute ()
  "Replace (...(,,,|( with (,,,(...|( where ... and ,,, is arbitrary code."
  (interactive)
  (if (save-excursion
        (lispy--out-forward 2))
      (let ((endp (and (region-active-p)
                       (= (point) (region-end))))
            beg end
            deactivate-mark)
        (when endp (exchange-point-and-mark))
        (lispy-save-excursion
          (setq beg (point))
          (lispy--out-forward 1)
          (setq end (backward-list))
          (lispy--out-forward 1)
          (backward-list)
          (lispy--swap-regions (cons beg end)
                               (cons (point) (point)))
          (lispy--out-forward 1)
          (lispy--reindent))
        (when endp (exchange-point-and-mark)))
    (error "Not enough depth to convolute")))

(defun lispy-join ()
  "Join sexps."
  (interactive)
  (let ((pt (point)))
    (cond ((looking-back lispy-right)
           (when (lispy-forward 1)
             (backward-list)
             (delete-char 1)
             (goto-char pt)
             (backward-delete-char 1)
             (lispy--out-forward 1)
             (lispy--reindent 1)))
          ((looking-at lispy-left)
           (when (lispy-backward 1)
             (forward-list)
             (backward-delete-char 1)
             (goto-char (1- pt))
             (delete-char 1)
             (lispy--out-forward 1)
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
  (cond ((region-active-p)
         (let ((pt (point))
               (at-start (= (point) (region-beginning)))
               (bnd0 (save-excursion
                       (deactivate-mark)
                       (if (ignore-errors (up-list) t)
                           (lispy--bounds-dwim)
                         (cons (point-min) (point-max)))))
               (bnd1 (lispy--bounds-dwim))
               bnd2)
           (goto-char (car bnd1))
           (if (re-search-backward "[^ \n`'#(]" (car bnd0) t)
               (progn
                 (deactivate-mark)
                 (if (lispy--in-comment-p)
                     (setq bnd2 (lispy--bounds-comment))
                   (when (eq (char-after) ?\")
                     (forward-char)
                     (backward-sexp))
                   (when (memq (char-after) '(?\) ?\] ?}))
                     (forward-char))
                   (setq bnd2 (lispy--bounds-dwim)))
                 (lispy--swap-regions bnd1 bnd2)
                 (setq deactivate-mark nil)
                 (goto-char (car bnd2))
                 (set-mark (point))
                 (forward-char (- (cdr bnd1) (car bnd1)))
                 (when at-start
                   (exchange-point-and-mark)))
             (goto-char pt))))
        ((and (looking-at lispy-left)
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
           (backward-list)))))

(defun lispy-move-down ()
  "Move current expression down.  Don't exit parent list."
  (interactive)
  (cond ((region-active-p)
         (let ((pt (point))
               (at-start (= (point) (region-beginning)))
               (bnd0 (save-excursion
                       (deactivate-mark)
                       (if (ignore-errors (up-list) t)
                           (lispy--bounds-dwim)
                         (cons (point-min) (point-max)))))
               (bnd1 (lispy--bounds-dwim))
               bnd2)
           (goto-char (cdr bnd1))
           (if (re-search-forward "[^ \n]" (1- (cdr bnd0)) t)
               (progn
                 (deactivate-mark)
                 (if (lispy--in-comment-p)
                     (setq bnd2 (lispy--bounds-comment))
                   (when (memq (char-before) '(?\( ?\" ?\[ ?{))
                     (backward-char))
                   (setq bnd2 (lispy--bounds-dwim)))
                 (lispy--swap-regions bnd1 bnd2)
                 (setq deactivate-mark nil)
                 (goto-char (cdr bnd2))
                 (set-mark (point))
                 (backward-char (- (cdr bnd1) (car bnd1)))
                 (unless at-start
                   (exchange-point-and-mark)))
             (goto-char pt))))
        ((and (looking-at lispy-left)
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
           (backward-list)))))

(defun lispy-clone (arg)
  "Clone sexp ARG times."
  (interactive "p")
  (let ((str (lispy--string-dwim)))
    (cond ((region-active-p)
           (let ((str (lispy--string-dwim)))
             (let (deactivate-mark)
               (save-excursion
                 (newline-and-indent)
                 (insert str)))))
          ((looking-at lispy-left)
           (save-excursion
             (dotimes-protect arg
               (insert str)
               (newline-and-indent))))
          ((looking-back lispy-right)
           (dotimes-protect arg
             (newline-and-indent)
             (insert str)))
          (t
           (error "Unexpected")))))

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
        (insert (substring
                 (replace-regexp-in-string "\n *" " " no-comment) 1))))
    ;; work around `( and '(
    (lispy-forward 1)
    (lispy--normalize 0)))

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
  (if (and (> arg 1) (lispy--in-comment-p))
      (let ((bnd (lispy--bounds-comment)))
        (uncomment-region (car bnd) (cdr bnd)))
    (dotimes-protect arg
      (let (bnd)
        (cond ((region-active-p)
               (comment-dwim nil)
               (when (lispy--in-string-or-comment-p)
                 (lispy--out-backward 1)))
              ((lispy--in-string-or-comment-p)
               (self-insert-command 1))
              ((looking-at lispy-left)
               (setq bnd (lispy--bounds-dwim))
               (lispy-counterclockwise)
               (comment-region (car bnd) (cdr bnd))
               (when (lispy--in-string-or-comment-p)
                 (lispy--out-backward 1)))
              ((bolp)
               (insert ";;"))
              ((or (looking-back lispy-right) (eolp))
               (comment-dwim nil)
               (insert " "))
              ((looking-back "^ +")
               (comment-dwim nil))
              ((setq bnd (save-excursion
                           (and (lispy--out-forward 1)
                                (point))))
               (let ((pt (point)))
                 (if (re-search-forward "\n" bnd t)
                     (progn (comment-region pt (point))
                            (lispy-forward 1)
                            (lispy-backward 1))
                   (comment-region (point) (1- bnd))
                   (lispy--out-backward 1))))
              (t
               (self-insert-command 1)))))))

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

(defun lispy-teleport (arg)
  "Move ARG sexps into a sexp determined by ace-jump."
  (interactive "p")
  (let ((beg (point))
        end endp regionp)
    (cond ((region-active-p)
           (setq endp (= (point) (region-end)))
           (setq regionp t)
           (lispy-different))
          ((looking-at lispy-left)
           (unless (dotimes-protect arg
                     (forward-list 1))
             (error "Unexpected")))
          ((looking-back lispy-right)
           (setq endp t)
           (unless (dotimes-protect arg
                     (backward-list arg))
             (error "Unexpected")))
          (t
           (error "Unexpected")))
    (setq end (point))
    (goto-char beg)
    (lispy--ace-do
     lispy-left
     (save-excursion (lispy--out-backward 50) (lispy--bounds-dwim))
     (lambda () (not (lispy--in-string-or-comment-p)))
     `(lambda ()
        (forward-char)
        (ignore-errors
          (unless (looking-at "(")
            (forward-sexp)))
        (backward-char)
        (lispy--teleport ,beg ,end ,endp ,regionp))
     t)))

;; ——— Locals:  tags ———————————————————————————————————————————————————————————
(defun lispy-goto ()
  "Jump to symbol within files in current directory."
  (interactive)
  (deactivate-mark)
  (lispy--goto 'lispy--fetch-tags))

(defun lispy-goto-recursive ()
  "Jump to symbol within files in current directory and its subdiretories."
  (interactive)
  (deactivate-mark)
  (lispy--goto 'lispy--fetch-tags-recursive))

(defun lispy-goto-local ()
  "Jump to symbol within current file."
  (interactive)
  (lispy--goto 'lispy--fetch-this-file-tags))

;; ——— Locals:  dialect-related ————————————————————————————————————————————————
(defun lispy-eval ()
  "Eval last sexp."
  (interactive)
  (save-excursion
    (unless (or (looking-back lispy-right) (region-active-p))
      (lispy-forward 1))
    (message
     (replace-regexp-in-string "%" "%%" (lispy--eval (lispy--string-dwim))))))

(defun lispy-eval-and-insert ()
  "Eval last sexp and insert the result."
  (interactive)
  (cl-labels
      ((doit ()
         (unless (or (looking-back lispy-right) (region-active-p))
           (lispy-forward 1))
         (let ((str (lispy--eval (lispy--string-dwim))))
           (newline-and-indent)
           (insert str))))
    (if (looking-at lispy-left)
        (save-excursion
          (doit))
      (doit))))

(defun lispy-eval-and-replace ()
  "Eval last sexp and replace it with the result."
  (interactive)
  (lispy-from-left
   (let* ((bnd (lispy--bounds-list))
          (str (lispy--string-dwim bnd)))
     (delete-region (car bnd) (cdr bnd))
     (insert (lispy--eval str)))))

(declare-function cider-jump-to-def "ext:cider")
(declare-function slime-edit-definition "ext:slime")
(declare-function lispy--clojure-resolve "ext:lispy")

(defun lispy-follow ()
  "Follow to `lispy--current-function'."
  (interactive)
  (let ((symbol (lispy--current-function))
        rsymbol)
    (cond ((and (memq major-mode '(emacs-lisp-mode lisp-interaction-mode))
                (setq symbol (intern-soft symbol)))
           (cond ((fboundp symbol)
                  (push-mark)
                  (deactivate-mark)
                  (find-function symbol))
                 ((boundp symbol)
                  (push-mark)
                  (deactivate-mark)
                  (find-variable symbol))))
          ((eq major-mode 'clojure-mode)
           (require 'le-clojure)
           (setq rsymbol (lispy--clojure-resolve symbol))
           (cond ((stringp rsymbol)
                  (cider-jump-to-def rsymbol))
                 ((eq rsymbol 'special)
                  (error "Can't jump to '%s because it's special" symbol))
                 ((eq rsymbol 'keyword)
                  (error "Can't jump to keywords"))
                 ((and (listp rsymbol)
                       (eq (car rsymbol) 'variable))
                  (error "Can't jump to Java variables"))
                 (t
                  (error "Could't resolve '%s" symbol)))
           (lispy--back-to-paren))
          ((eq major-mode 'lisp-mode)
           (require 'slime)
           (push-mark)
           (deactivate-mark)
           (slime-edit-definition symbol)))))

(defun lispy-describe ()
  "Display documentation for `lispy--current-function'."
  (interactive)
  (let ((symbol (intern-soft (lispy--current-function))))
    (cond ((fboundp symbol)
           (describe-function symbol)))))

(defun lispy-arglist ()
  "Display arglist for `lispy--current-function'."
  (interactive)
  (let ((sym (intern-soft (lispy--current-function))))
    (cond ((fboundp sym)
           (let ((args (car (help-split-fundoc (documentation sym t) sym))))
             (message "%s" args))))))

;; ——— Locals:  ace-jump-mode  —————————————————————————————————————————————————
(defun lispy-ace-char ()
  "Call `ace-jump-char-mode' on current defun."
  (interactive)
  (let ((bnd (save-excursion
               (lispy--out-backward 50)
               (lispy--bounds-dwim))))
    (narrow-to-region (car bnd) (cdr bnd))
    (let ((ace-jump-mode-scope 'window))
      (call-interactively 'ace-jump-char-mode))
    (widen)))

(defun lispy-ace-paren ()
  "Use `lispy--ace-do' to jump to `lispy-left' within current defun.
When FUNC is not nil, call it after a successful move.
When NO-NARROW is not nil, don't narrow."
  (interactive)
  (lispy--ace-do
   lispy-left
   (save-excursion (lispy--out-backward 50) (lispy--bounds-dwim))
   (lambda () (not (lispy--in-string-or-comment-p)))
   nil
   nil))

(defun lispy-ace-symbol (arg)
  "Use `lispy--ace-do' to mark to a symbol within a sexp.
Sexp is obtained by exiting list ARG times."
  (interactive "p")
  (lispy--out-forward
   (if (region-active-p)
       (progn (deactivate-mark) arg)
     (1- arg)))
  (lispy--ace-do
   "[([{ ]\\(?:\\sw\\|\\s_\\|\\s(\\|[\"'`#]\\)"
   (lispy--bounds-dwim)
   (lambda () (or (not (lispy--in-string-or-comment-p)) (looking-back ".\"")))
   (lambda () (forward-char 1) (lispy-mark-symbol))))

(defun lispy-ace-subword (arg)
  "Mark sub-word within a sexp.
Sexp is obtained by exiting list ARG times."
  (interactive "p")
  (lispy--out-forward
   (if (region-active-p)
       (progn (deactivate-mark) arg)
     (1- arg)))
  (lispy--ace-do
   "[([{ -]\\(?:\\sw\\|\\s_\\|\\s(\\|[\"'`#]\\)"
   (lispy--bounds-dwim)
   (lambda () (or (not (lispy--in-string-or-comment-p)) (looking-back ".\"")))
   (lambda () (skip-chars-forward "-([{ `'#") (mark-word))))

(defun lispy-ace-symbol-replace (arg)
  "Use `ace-jump-char-mode' to jump to a symbol within a sexp and delete it.
Sexp is obtained by exiting list ARG times."
  (interactive "p")
  (lispy--out-forward
   (if (region-active-p)
       (progn (deactivate-mark) arg)
     (1- arg)))
  (lispy--ace-do
   "[([{ ]\\(?:\\sw\\|\\s_\\|\\s(\\|[\"'`#]\\)"
   (lispy--bounds-dwim)
   (lambda () (or (not (lispy--in-string-or-comment-p)) (looking-back ".\"")))
   (lambda () (forward-char 1) (lispy-mark-symbol) (lispy-delete 1))))

;; ——— Locals:  outline ————————————————————————————————————————————————————————
(defun lispy-tab ()
  "Indent code and hide/show outlines."
  (interactive)
  (if (looking-at ";")
      (progn
        (outline-minor-mode 1)
        (condition-case e
            (outline-toggle-children)
          (error
           (if (string= (error-message-string e) "before first heading")
               (outline-next-visible-heading 1)
             (signal (car e) (cdr e))))))
    (indent-sexp)
    (lispy-normalize)))

(defun lispy-shifttab ()
  "Hide/show outline summary."
  (interactive)
  (require 'noflet)
  (outline-minor-mode 1)
  (noflet ((org-unlogged-message (&rest x)))
    (if (get 'lispy-shifttab 'state)
        (progn
          (org-cycle '(64))
          (put 'lispy-shifttab 'state nil))
      (org-overview)
      (put 'lispy-shifttab 'state 1))))

;; ——— Locals:  refactoring ————————————————————————————————————————————————————
(defun lispy-to-lambda ()
  "Turn the current function definition into a lambda."
  (interactive)
  (when (save-excursion (lispy--out-backward 1))
    (beginning-of-defun))
  (forward-char 1)
  (let ((beg (point)))
    (when (re-search-forward "(" (save-excursion (forward-list)) t)
      (delete-region beg (- (point) 2))
      (goto-char beg)
      (insert "lambda")
      (goto-char (1- beg)))))

(defun lispy-to-defun ()
  "Turn the current lambda into a defun."
  (interactive)
  (let ((pt (point)))
    (when (looking-back ")")
      (backward-list))
    (while (and (not (looking-at "(lambda"))
                (lispy--out-backward 1)))
    (if (looking-at "(lambda")
        (let ((name (read-string "Function name: ")))
          (forward-char 1)
          (delete-char 6)
          (insert "defun " name)
          (lispy-kill-at-point)
          (insert "#'" name)
          (message "defun stored to kill ring"))
      (goto-char pt)
      (error "Not in lambda"))))

;; ——— Locals:  multiple cursors ———————————————————————————————————————————————
(declare-function mc/create-fake-cursor-at-point "ext:multiple-cursors")
(declare-function mc/all-fake-cursors "ext:multiple-cursors")
(declare-function mc/maybe-multiple-cursors-mode "ext:multiple-cursors")
(declare-function mc/mark-lines "ext:multiple-cursors")

(defun lispy-cursor-down (arg)
  "Add ARG cursors using `lispy-down'."
  (interactive "p")
  (require 'multiple-cursors)
  (if (looking-at lispy-left)
      (dotimes-protect arg
        (mc/create-fake-cursor-at-point)
        (loop do (lispy-down 1)
           while (mc/all-fake-cursors (point) (1+ (point)))))
    (mc/mark-lines arg 'forwards))
  (mc/maybe-multiple-cursors-mode))

(defun lispy-cursor-ace ()
  "Add a cursor using `lispy--ace-do'.
Currently, only one cursor can be added with local binding.
Any amount can be added with a global binding."
  (interactive)
  (require 'multiple-cursors)
  (mc/create-fake-cursor-at-point)
  (lexical-let ((pt (point)))
    (lispy--ace-do
     "(" (cons (window-start) (window-end))
     (lambda () (not (lispy--in-string-or-comment-p)))
     #'mc/maybe-multiple-cursors-mode)))

;; ——— Locals:  ediff ——————————————————————————————————————————————————————————
(defun lispy-store-region-and-buffer ()
  "Store current buffer and `lispy--bounds-dwim'."
  (interactive)
  (put 'lispy-store-bounds 'buffer (current-buffer))
  (put 'lispy-store-bounds 'region (lispy--bounds-dwim)))

(defun lispy-ediff-regions ()
  "Comparable to `ediff-regions-linewise'.
First region and buffer come from `lispy-store-region-and-buffer'
Second region and buffer are the current ones."
  (interactive)
  (let ((wnd (current-window-configuration)))
    (multiple-value-bind (buf1 beg1 end1)
        (lispy--make-ediff-buffer
         (current-buffer) "-A-"
         (lispy--bounds-dwim))
      (multiple-value-bind (buf2 beg2 end2)
          (lispy--make-ediff-buffer
           (get 'lispy-store-bounds 'buffer) "-B-"
           (get 'lispy-store-bounds 'region))
        (ediff-regions-internal
         buf1 beg1 end1
         buf2 beg2 end2
         nil 'ediff-regions-linewise nil nil)))
    (add-hook 'ediff-after-quit-hook-internal
              `(lambda ()
                 (setq ediff-after-quit-hook-internal)
                 (set-window-configuration ,wnd)))))

;; ——— Locals:  miscellanea ————————————————————————————————————————————————————
(defun lispy-x ()
  "Forward to `lispy-mode-x-map'."
  (interactive)
  (let ((char (read-char))
        fun)
    (if (setq fun (cdr (assoc char lispy-mode-x-map)))
        (call-interactively fun)
      (error "Nothing bound to %c" char))))

(defun lispy-ert ()
  "Call (`ert' t)."
  (interactive)
  (ert t))

(defun lispy-normalize ()
  "Normalize current sexp."
  (interactive)
  (save-excursion
    (cond ((looking-at lispy-left)
           (forward-char 1))
          ((looking-back lispy-right)
           (backward-list)
           (forward-char 1))
          (t
           (error "Unexpected")))
    (lispy--normalize 1)))

(defun lispy-undo ()
  "Deactivate region and `undo'."
  (interactive)
  (when (region-active-p)
    (deactivate-mark t))
  (undo))

(defun lispy-view ()
  "Recenter current sexp to first screen line.
If already there, return it to previous position."
  (interactive)
  (let ((window-line (count-lines (window-start) (point))))
    (if (or (= window-line 0)
            (and (not (bolp)) (= window-line 1)))
        (recenter (or (get 'lispy-recenter :line) 0))
      (put 'lispy-recenter :line window-line)
      (recenter 0))))

(defun lispy-edebug-stop ()
  "Stop edebugging, while saving current function arguments."
  (interactive)
  (if (bound-and-true-p edebug-active)
      (save-excursion
        (lispy-out-backward 99)
        (if (looking-at "(defun\\s-+\\_<[^ ]+\\_>\\s-+(")
            (progn
              (goto-char (match-end 0))
              (backward-char 1)
              (forward-sexp 1)
              (let ((sexps (mapcar
                            (lambda (x!) (cons x! (symbol-value x!)))
                            (delq '&optional (delq '&rest (preceding-sexp)))))
                    (wnd (current-window-configuration)))
                (run-with-timer
                 0 nil
                 `(lambda ()
                    (mapc (lambda (x!) (set (car x!) (cdr x!))) ',sexps)
                    (set-window-configuration ,wnd)))
                (top-level)))))
    (self-insert-command 1)))

(defun lispy-edebug (arg)
  "Start/stop edebug of current thing depending on ARG.
ARG is 1: `edebug-defun' on this function.
ARG is 2: `eval-defun' on this function.
ARG is 3: `edebug-defun' on the function from this sexp.
ARG is 4: `eval-defun' on the function from this sexp."
  (interactive "p")
  (cond ((= arg 1)
         (edebug-defun))
        ((= arg 2)
         (eval-defun nil))
        (t
         (let* ((expr (lispy--read (lispy--string-dwim)))
                (fun (car expr)))
           (if (fboundp fun)
               (cl-destructuring-bind (buf . pt)
                   (find-definition-noselect fun nil)
                 (with-current-buffer buf
                   (goto-char pt)
                   (cond ((= arg 3)
                          (edebug-defun))
                         ((= arg 4)
                          (eval-defun nil))
                         (t
                          (error "arg=%s isn't supported" arg)))))
             (error "%s isn't bound" fun))))))

(defun lispy-flatten (arg)
  "Inline a function at the point of its call.
The function body is obtained from `find-function-noselect'.
With ARG, use the contents of `lispy-store-region-and-buffer' instead."
  (interactive "P")
  (let* ((begp (looking-at lispy-left))
         (bnd (lispy--bounds-list))
         (expr (lispy--read (lispy--string-dwim bnd)))
         (fstr (if arg
                   (with-current-buffer (get 'lispy-store-bounds 'buffer)
                     (lispy--string-dwim (get 'lispy-store-bounds 'region)))
                 (ignore-errors
                   (lispy--function-str (car expr)))))
         f-args body
         (e-args (cl-remove-if #'lispy--whitespacep (cdr expr))))
    (multiple-value-setq (f-args body)
      (lispy--function-parse fstr))
    (when (equal f-args '(ly-raw empty))
      (setq f-args))
    (setq f-args (delq '&optional (delq '&rest f-args)))
    (let ((e-n (length e-args))
          (f-n (length f-args))
          f-arg e-arg)
      (if (> e-n f-n)
          (error "Too many arguments: expected %s, got %s" f-n e-n)
        (setq e-args (append e-args (make-list (- f-n e-n) nil))))
      (while (setq e-arg (pop e-args)
                   f-arg (pop f-args))
        (setq body (lispy--replace body f-arg e-arg)))
      (if (= (length body) 1)
          (setq body (car body))
        (setq body (cons 'progn body)))
      (goto-char (car bnd))
      (delete-region (car bnd) (cdr bnd))
      (lispy--insert body)
      (when begp
        (goto-char (car bnd))))))

(declare-function projectile-find-file "ext:projectile")
(declare-function projectile-find-file-other-window "ext:projectile")

(defun lispy-visit (arg)
  "Forward to find file in project."
  (interactive "p")
  (cond ((= arg 1)
         (projectile-find-file nil))
        ((= arg 2)
         (projectile-find-file-other-window))
        (t
         (projectile-find-file arg))))

(defun lispy-narrow (arg)
  "Narrow ARG sexps or region."
  (interactive "p")
  (cond ((region-active-p)
         (narrow-to-region (region-beginning) (region-end)))
        ((looking-at lispy-left)
         (narrow-to-region (point)
                           (save-excursion
                             (lispy-forward arg)
                             (point))))
        ((looking-back lispy-right)
         (narrow-to-region (point)
                           (save-excursion
                             (lispy-backward arg)
                             (point))))))

(defun lispy-widen ()
  "Forward to `widen'."
  (interactive)
  (widen))

;; ——— Predicates ——————————————————————————————————————————————————————————————
(defun lispy--in-string-p ()
  "Test if point is inside a string."
  (let ((beg (nth 8 (syntax-ppss))))
    (and beg
         (eq (char-after beg) ?\"))))

(defun lispy--in-comment-p ()
  "Test if point is inside a comment."
  (save-excursion
    (unless (eolp)
      (forward-char 1))
    (let ((beg (nth 8 (syntax-ppss))))
      (and beg
           (comment-only-p beg (point))))))

(defun lispy--in-string-or-comment-p ()
  "Test if point is inside a string or a comment."
  (let ((beg (nth 8 (syntax-ppss))))
    (and beg
         (or (eq (char-after beg) ?\")
             (comment-only-p beg (point))))))

(defun lispy--buffer-narrowed-p ()
  "Returns T if the current buffer is narrowed."
  (or (/= (point-min) 1)
      (/= (point-max) (1+ (buffer-size)))))

;; ——— Pure ————————————————————————————————————————————————————————————————————
(defun lispy--bounds-dwim ()
  "Return a cons of region bounds if it's active.
Otherwise return cons of current string, symbol or list bounds."
  (cond ((region-active-p)
         (cons (region-beginning)
               (region-end)))
        ((looking-back lispy-right)
         (backward-list)
         (prog1 (bounds-of-thing-at-point 'sexp)
           (forward-list)))
        ((or (looking-at (format "[`'#]*%s\\|\"" lispy-left))
             (looking-at "[`'#]"))
         (bounds-of-thing-at-point 'sexp))
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

(defun lispy--bounds-comment ()
  "Return bounds of current comment."
  (and (lispy--in-comment-p)
       (save-excursion
         (when (lispy--beginning-of-comment)
           (let ((pt (point)))
             (while (and (lispy--in-comment-p)
                         (forward-comment -1)
                         (= 1 (- (count-lines (point) pt)
                                 (if (bolp) 0 1))))
               (setq pt (point)))
             (goto-char pt))
           (let ((beg (lispy--beginning-of-comment))
                 (pt (point))
                 (col (current-column)))
             (while (and (lispy--in-comment-p)
                         (forward-comment 1)
                         (lispy--beginning-of-comment)
                         (and (= 1 (- (count-lines pt (point))
                                      (if (bolp) 0 1)))
                              ;; count comments starting in different columns
                              ;; as separate
                              (= col (current-column))
                              ;; if there's code in between,
                              ;; count comments as separate
                              (looking-back "^\\s-*")))
               (setq pt (point)))
             (goto-char pt)
             (end-of-line)
             (cons beg (point)))))))

(defun lispy--string-dwim (&optional bounds)
  "Return the string that corresponds to BOUNDS.
`lispy--bounds-dwim' is used if BOUNDS is nil."
  (destructuring-bind (beg . end) (or bounds (lispy--bounds-dwim))
    (buffer-substring-no-properties beg end)))

(defun lispy--current-function ()
  "Return current function as string."
  (if (region-active-p)
      (let ((str (lispy--string-dwim)))
        (if (string-match "^[#'`]*\\(.*\\)$" str)
            (match-string 1 str)
          nil))
    (save-excursion
      (lispy--back-to-paren)
      (when (looking-at "(\\([^ \n)]+\\)[ )\n]")
        (match-string-no-properties 1)))))

;; ——— Utilities: movement —————————————————————————————————————————————————————
(defvar lispy-ignore-whitespace nil
  "When set to t, `lispy-out-forward' will not clean up whitespace.")

(defun lispy--out-forward (arg)
  "Move outside list forwards ARG times.
Return nil on failure, (point) otherwise."
  (lispy--exit-string)
  (catch 'break
    (dotimes (i arg)
      (if (ignore-errors (up-list) t)
          (progn
            (unless lispy-ignore-whitespace
              (lispy--remove-gaps))
            (lispy--indent-for-tab))
        (when (looking-at lispy-left)
          (forward-list))
        (throw 'break nil)))
    (point)))

(defun lispy--out-backward (arg)
  "Move outside list forwards ARG times.
Return nil on failure, t otherwise."
  (let ((pt (point)))
    (lispy--out-forward arg)
    (when (looking-back lispy-right)
      (lispy-backward 1))
    (not (= pt (point)))))

(defun lispy--back-to-paren ()
  "Move to ( going out backwards."
  (let ((lispy-ignore-whitespace t))
    (while (and (not (looking-at "("))
                (lispy--out-backward 1)))))

(defun lispy--exit-string ()
  "When in string, go to its beginning."
  (let ((s (syntax-ppss)))
    (when (nth 3 s)
      (goto-char (nth 8 s)))))

(defun lispy--beginning-of-comment ()
  "Go to beginning of comment on current line."
  (end-of-line)
  (let ((cs (comment-search-backward (line-beginning-position) t)))
    (when cs
      (goto-char cs))))

(defun lispy--comment-search-forward (dir)
  "Search for a first comment in direction DIR.
Move to the end of line."
  (while (not (lispy--in-comment-p))
    (forward-line dir)
    (end-of-line)))

;; ——— Utilities: evaluation ———————————————————————————————————————————————————
(defun lispy--eval (e-str)
  "Eval E-STR according to current `major-mode'."
  (funcall
   (cond ((memq major-mode '(emacs-lisp-mode lisp-interaction-mode))
          'lispy--eval-elisp)
         ((eq major-mode 'clojure-mode)
          (require 'le-clojure)
          'lispy--eval-clojure)
         ((eq major-mode 'scheme-mode)
          (require 'le-scheme)
          'lispy--eval-scheme)
         ((eq major-mode 'lisp-mode)
          (require 'le-lisp)
          'lispy--eval-lisp)
         (t (error "%s isn't supported currently" major-mode)))
   e-str))

(defun lispy--eval-elisp (e-str)
  "Eval E-STR as Elisp code."
  (let ((sexp (read e-str))
        val)
    (when (and (consp sexp)
               (eq (car sexp) 'defvar)
               (consp (cdr sexp))
               (boundp (cadr sexp)))
      (set (cadr sexp) (eval (caddr sexp))))
    (condition-case e
        (prin1-to-string
         (eval sexp lexical-binding))
      (error
       (progn
         (let ((es (error-message-string e)))
           (if (and lispy-lax-eval
                    (string-match
                     "^Symbol's value as variable is void: \\(.*\\)$"
                     es))
               (progn
                 (setq es (match-string 1 es))
                 (set (intern es) nil)
                 (message "Caught unbound variable %s, setting it to nil." es))
             (signal (car e) (cdr e)))))))))

;; ——— Utilities: tags —————————————————————————————————————————————————————————
(defun lispy-build-semanticdb (&optional dir)
  "Build and save semanticdb for DIR."
  (interactive)
  (setq dir (or dir default-directory))
  (let ((default-directory dir))
    (mapc
     (lambda (f)
       (let ((buffer (find-file-noselect f))
             tags)
         (set-buffer buffer)
         (setq tags (ignore-errors (semantic-fetch-tags)))
         ;; modifies tags
         (when (memq major-mode '(lisp-mode emacs-lisp-mode))
           (lexical-let ((arity (cdr (assoc major-mode lispy-tag-arity)))
                         (tag-regex (lispy--tag-regexp)))
             (mapc (lambda (x) (lispy--modify-tag x tag-regex arity)) tags)))
         ;; (kill-buffer buffer)
         ))
     (lispy--file-list)))
  (semanticdb-save-all-db))

(defun lispy--file-list ()
  "Get the list of same type files in current directory."
  (let ((ext (file-name-extension (buffer-file-name))))
    (cl-remove-if
     (lambda (x) (string-match "\\(?:^\\.?#\\|~$\\)" x))
     (file-expand-wildcards (format "*.%s" ext)))))

(defvar lispy-tag-arity
  '((lisp-mode
     (defclass . 1)
     (defconstant . 1)
     (defgeneric . 1)
     (define-condition . 1)
     (define-symbol-macro . 1)
     (defmethod . 2)
     (defpackage . 1)
     (defparameter . 1)
     (defsetf . 1)
     (defstruct . 1)
     (deftype . 1)
     (in-package . 1)
     (load . 1)
     (setq . 2)
     ;; SLIME specific
     (definterface . 1)
     (defimplementation . 1)
     (define-caller-pattern . 1)
     (define-variable-pattern . 1)
     (define-pattern-substitution . 1)
     (defslimefun . 1))
    (emacs-lisp-mode
     (setq . 2)
     (setq-default . 2)
     (add-to-list . 2)
     (add-hook . 2)
     (load . 1)
     (load-file . 1)
     (define-key . 3)
     (ert-deftest . 1)
     (declare-function . 1)
     (defalias . 2)
     (make-variable-buffer-local . 1)
     (define-minor-mode . 1)
     (make-obsolete . 2)
     (put . 3)
     (overlay-put . 3)
     (make-obsolete-variable . 1)
     (define-obsolete-function-alias . 1)
     (define-obsolete-variable-alias . 1)
     (eval-after-load . 1)
     (global-set-key . 2)
     ;; org-mode specific
     (org-defkey . 3)))
  "Alist of tag arities for supported modes.")

(defun lispy--tag-regexp (&optional mode)
  "Return tag regexp based on MODE."
  (setq mode (or mode major-mode))
  (cond ((eq mode 'lisp-mode)
         (concat
          "^([ \t\n]*\\_<\\(?:cl:\\)?"
          "\\("
          (regexp-opt
           (mapcar (lambda (x) (symbol-name (car x)))
                   (cdr (assoc mode lispy-tag-arity))))
          "\\)"
          "\\_>"))
        ((memq major-mode '(emacs-lisp-mode lisp-interaction-mode))
         (concat
          "^([ \t\n]*\\_<"
          "\\("
          (regexp-opt
           (mapcar (lambda (x) (symbol-name (car x)))
                   (cdr (assoc mode lispy-tag-arity))))
          "\\)"
          "\\_>"))
        (t (error "%s isn't supported" mode))))

(defcustom lispy-helm-columns '(1 60 70 80)
  "Start and end positions of columns when completing with `helm'."
  :group 'lispy)

(defun lispy--propertize-tag (kind x &optional face)
  "Concatenate KIND and the name of tag X.
KIND is fontified with `font-lock-keyword-face'.
The name of X fontified according to FACE.
FACE can be :keyword, :function or :type.  It defaults to 'default."
  (concat
   (if kind (concat (propertize kind 'face 'font-lock-keyword-face) " ") "")
   (propertize (car x) 'face
               (cl-case face
                 (:keyword 'font-lock-keyword-face)
                 (:type 'font-lock-type-face)
                 (:function 'font-lock-function-name-face)
                 (t 'default)))))

(defun lispy--modify-tag (x regex arity-alist)
  "Re-parse X and modify it accordingly.
REGEX selects the symbol is 1st place of sexp.
ARITY-ALIST combines strings that REGEX matches and their arities."
  (let ((overlay (nth 4 x))
        buffer start)
    (if (overlayp overlay)
        (setq buffer (overlay-buffer overlay)
              start (overlay-start overlay))
      (if (vectorp overlay)
          (setq buffer (find-file-noselect (aref overlay 2))
                start (aref overlay 0))
        (error "Unexpected")))
    (with-current-buffer buffer
      (save-excursion
        (goto-char start)
        (when (looking-at regex)
          (goto-char (match-end 0))
          (let ((tag-head (match-string 1))
                beg arity str)
            (skip-chars-forward " \n")
            (when (setq arity (cdr (assoc (intern tag-head) arity-alist)))
              (setq beg (point))
              (forward-sexp arity)
              (setq str (replace-regexp-in-string
                         "\n" " " (buffer-substring-no-properties beg (point))))
              (setcar x str)
              (setcar (nthcdr 1 x) (intern tag-head))))))))
  x)

(defun lispy--tag-name-lisp (x)
  "Build tag name for Common Lisp tag X."
  (cond ((not (stringp (car x)))
         "tag with no name")
        ((eq (cadr x) 'function)
         (lispy--propertize-tag nil x :function))
        ((eq (cadr x) 'type)
         (lispy--propertize-tag "defstruct" x :type))
        ((eq (cadr x) 'variable)
         (lispy--propertize-tag "defvar" x))
        ((assq (cadr x) (cdr (assoc 'lisp-mode lispy-tag-arity)))
         (lispy--propertize-tag (symbol-name (cadr x)) x))
        (t (car x))))

(defun lispy--tag-name-elisp (x)
  "Build tag name for Elisp tag X."
  (cond ((not (stringp (car x)))
         "tag with no name")
        ((eq (cadr x) 'include)
         (lispy--propertize-tag "require" x))
        ((eq (cadr x) 'package)
         (lispy--propertize-tag "provide" x))
        ((eq (cadr x) 'customgroup)
         (lispy--propertize-tag "defgroup" x))
        ((eq (cadr x) 'function)
         (lispy--propertize-tag nil x :function))
        ((eq (cadr x) 'variable)
         (lispy--propertize-tag "defvar" x))
        ((assq (cadr x) (cdr (assoc 'emacs-lisp-mode lispy-tag-arity)))
         (lispy--propertize-tag (symbol-name (cadr x)) x))
        (t (car x))))

(defun lispy--tag-name-clojure (x)
  "Build tag name for Clojure tag X."
  (cond ((not (stringp (car x))))
        ((eq (cadr x) 'package)
         (lispy--propertize-tag "ns" x))
        ((eq (cadr x) 'function)
         (lispy--propertize-tag nil x :function))
        ((eq (cadr x) 'variable)
         (lispy--propertize-tag "def" x))
        (t (car x))))

(defun lispy--tag-name (x)
  "Given a semantic tag X, amend it with additional info.
For example, a `setq' statement is amended with variable name that it uses."
  (or
   (catch 'break
     (cons
      (concat
       (lispy--pad-string
        (cond ((memq major-mode '(emacs-lisp-mode lisp-interaction-mode))
               (lispy--tag-name-elisp x))
              ((eq major-mode 'clojure-mode)
               (lispy--tag-name-clojure x))
              ((eq major-mode 'scheme-mode)
               ;; (lispy--tag-name-scheme x)
               (car x))
              ((eq major-mode 'lisp-mode)
               (lispy--tag-name-lisp x))
              (t (throw 'break nil)))
        (nth 1 lispy-helm-columns))
       (make-string (- (nth 2 lispy-helm-columns)
                       (nth 1 lispy-helm-columns))
                    ?\ )
       (let ((v (nth 4 x)))
         (file-name-nondirectory
          (cond ((overlayp v)
                 (buffer-file-name (overlay-buffer v)))
                ((vectorp v)
                 (aref v 2))
                (t (error "Unexpected"))))))
      (cdr x)))
   x))

(defun lispy--pad-string (str n)
  "Make STR at most length N."
  (setq str (replace-regexp-in-string "\t" "    " str))
  (if (< (length str) (- n 3))
      (concat str (make-string (- n (length str)) ?\ ))
    (concat (substring str 0 (- n 3)) "...")))

(defun lispy--fetch-tags (&optional path)
  "Get a list of tags for PATH."
  (lispy--fetch-this-file-tags)
  (setq path (or path default-directory))
  (let* ((this-file (buffer-file-name))
         (default-directory path)
         (db (or (semanticdb-directory-loaded-p path)
                 ;; a trick to make sure semantic loads
                 (let ((file (expand-file-name (car (lispy--file-list)))))
                   (when file
                     (with-current-buffer
                         (find-file-noselect file)
                       (unless (equal this-file (buffer-file-name))
                         (kill-buffer))))
                   (semanticdb-directory-loaded-p path)))))
    (unless (lexical-let ((db-files
                           (mapcar (lambda (x) (aref x 2))
                                   (and db (aref db 6)))))
              (cl-every (lambda (x) (member x db-files))
                        (let ((default-directory path))
                          (lispy--file-list))))
      (lispy-build-semanticdb path)
      (setq db (semanticdb-directory-loaded-p path)))
    (and db
         (setq db (cl-remove-if-not
                   (lambda (x) (eq (aref x 4) major-mode))
                   (aref db 6)))
         (apply
          #'append
          (mapcar
           (lambda (x)
             (cl-remove-if-not
              (lambda (s) (stringp (car s)))
              (lispy--set-file-to-tags
               (expand-file-name (aref x 2) path)
               (aref x 5)))) db)))))

(defun lispy--fetch-tags-recursive ()
  "Fetch all tags in current directory recursively."
  (let ((dirs
         (cl-remove-if
          (lambda (y) (string-match "\\.git/" y))
          (mapcar
           (lambda (x) (concat (expand-file-name x) "/"))
           (split-string
            (shell-command-to-string "find . -type d")
            "\n"
            t)))))
    (apply #'append
           (mapcar #'lispy--fetch-tags dirs))))

(defun lispy--set-file-to-tags (file tags)
  "Put FILE as property of each tag in TAGS."
  (mapcar
   (lambda (y)
     (let ((v (nth 4 y)))
       (cond ((vectorp v)
              (cl-case (length v)
                (2 (setcar (nthcdr 4 y)
                           (vconcat v (vector file))))
                (3 nil)
                (t (error "Unexpected"))))
             ((overlayp v)
              (setcar (nthcdr 4 y)
                      (vector (overlay-start v)
                              (overlay-end v)
                              file)))
             (t (error "Unexpected")))
       y))
   tags))

(defun lispy--fetch-this-file-tags ()
  "Fetch this file tags."
  (let ((tags
         (lispy--set-file-to-tags
          (file-name-nondirectory (buffer-file-name))
          (semantic-fetch-tags))))
    (when (memq major-mode '(lisp-mode emacs-lisp-mode))
      (lexical-let ((arity (cdr (assoc major-mode lispy-tag-arity)))
                    (tag-regex (lispy--tag-regexp)))
        (mapc (lambda (x) (lispy--modify-tag x tag-regex arity)) tags)))
    tags))

(defvar lispy--goto-cache nil "Maps directories to pretty tags.")

(defcustom lispy-no-permanent-semantic nil
  "When t, `lispy' will not enable function `semantic-mode' when it's off."
  :type 'boolean
  :group 'lispy)

(defun lispy--goto (fun)
  "Jump to symbol selected from (FUN)."
  (require 'semantic/bovine/el)
  (let ((semantic-on (bound-and-true-p semantic-mode)))
    (semantic-mode 1)
    (let ((candidates (funcall fun))
          (cache (assoc default-directory lispy--goto-cache))
          cached-cands
          helm-candidate-number-limit)
      (lispy--select-candidate
       (cond ((null cache)
              (setq cached-cands (mapcar #'lispy--tag-name candidates))
              (when (> (length cached-cands) 1000)
                (push (cons default-directory cached-cands)
                      lispy--goto-cache))
              cached-cands)
             ((and (setq cached-cands (cdr cache))
                   (= (length cached-cands)
                      (length candidates)))
              cached-cands)
             (t
              (setcdr cache (mapcar #'lispy--tag-name candidates))))
       #'lispy--action-jump))
    (when (and lispy-no-permanent-semantic
               (not semantic-on))
      (semantic-mode -1))))

;; ——— Utilities: slurping and barfing —————————————————————————————————————————
(defun lispy--slurp-forward ()
  "Grow current sexp forward by one sexp."
  (unless (looking-back "[()])")
    (just-one-space)
    (backward-char 1))
  (let ((pt (point))
        (char (char-before))
        (beg (save-excursion (backward-list) (point))))
    (when (ignore-errors
            (forward-sexp) t)
      (delete-region (1- pt) pt)
      (insert char))))

(defun lispy--slurp-backward ()
  "Grow current sexp backward by one sexp."
  (let ((pt (point))
        (char (char-after)))
    (backward-sexp)
    (delete-region pt (1+ pt))
    (insert char)
    (backward-char)))

(defun lispy--barf-forward ()
  "Shrink current sexp forward by one sexp."
  (let ((pt (point))
        (char (char-after)))
    (unless (looking-at "()")
      (forward-char)
      (forward-sexp)
      (delete-region pt (1+ pt))
      (skip-chars-forward " \n	")
      (insert char)
      (backward-char)
      (indent-region pt (point))
      (lispy--reindent 1))))

(defun lispy--barf-backward ()
  "Shrink current sexp backward by one sexp."
  (let ((pt (point))
        (char (char-before)))
    (unless (looking-back "()")
      (backward-char)
      (backward-sexp)
      (skip-chars-backward " \n	")
      (while (lispy--in-comment-p)
        (goto-char (comment-beginning))
        (skip-chars-backward " \n	"))
      (delete-region (1- pt) pt)
      (insert char)
      (indent-region (point) pt))))

;; ——— Utilities: source transformation ————————————————————————————————————————
(defun lispy--read (str)
  "Read STR including comments and newlines."
  (let* ((mode major-mode)
         cbnd
         (str (with-temp-buffer
                (funcall mode)
                (insert str)
                ;; ——— comments ———————————————
                (goto-char (point-min))
                (while (comment-search-forward (point-max) t)
                  (lispy--beginning-of-comment)
                  (setq cbnd (cons (point) (line-end-position)))
                  (setq str (lispy--string-dwim cbnd))
                  (delete-region (car cbnd) (cdr cbnd))
                  (insert (format "(ly-raw comment %S)" str)))
                ;; ——— strings ————————————————
                (goto-char (point-min))
                (while (re-search-forward "\"" nil t)
                  (setq cbnd (lispy--bounds-string))
                  (when (and cbnd (not (looking-back "ly-raw comment \"")))
                    (setq str (lispy--string-dwim cbnd))
                    (delete-region (car cbnd) (cdr cbnd))
                    (insert (format "(ly-raw string %S)" str))))
                ;; ——— newlines ———————————————
                (goto-char (point-min))
                (while (re-search-forward "\n" nil t)
                  (unless (lispy--in-string-or-comment-p)
                    (replace-match " (ly-raw newline)")))
                ;; ——— () —————————————————————
                (goto-char (point-min))
                (while (re-search-forward "()" nil t)
                  (unless (lispy--in-string-or-comment-p)
                    (replace-match "(ly-raw empty)")))
                ;; ——— ? char syntax ——————————
                (goto-char (point-min))
                (while (re-search-forward "\\?" nil t)
                  (unless (lispy--in-string-or-comment-p)
                    (let ((pt (point))
                          sexp)
                      (forward-sexp)
                      (setq sexp (buffer-substring-no-properties pt (point)))
                      (delete-region (1- pt) (point))
                      (insert (format "(ly-raw char %S)" sexp)))))
                (buffer-substring-no-properties
                 (point-min)
                 (point-max)))))
    (ignore-errors
      (read str))))

(defvar lispy--insert-alist
  '((\` . "`")
    (\, . ",")
    (\,@ . ",@")))

(defun lispy--insert (expr)
  "Insert the EXPR read by `lispy--read."
  (cl-labels ((ensure-space ()
                (unless (and (looking-back "^\\|[ (`',@]")
                             (not (looking-back "\\\\(")))
                  (insert " "))))
    (let (str)
      (cond ((null expr)
             (ensure-space)
             (insert "nil"))
            ((listp expr)
             (if (or (not (listp (cdr expr)))
                     (and (eq (cadr expr) 'ly-raw)
                          (> (length expr) 2))
                     (assoc (cadr expr) lispy--insert-alist))
                 (progn
                   (ensure-space)
                   (insert (format "(%S . )" (car expr)))
                   (backward-char 1)
                   (lispy--insert (cdr expr))
                   (forward-char 1))
               (cond ((eq (car expr) 'ly-raw)
                      (cl-case (cadr expr)
                        ('comment
                         (insert (caddr expr)))
                        ('newline
                         (newline-and-indent))
                        ('string
                         (ensure-space)
                         (insert (caddr expr)))
                        ('empty
                         (ensure-space)
                         (insert "()"))
                        ('char
                         (ensure-space)
                         (insert "?" (caddr expr)))))
                     ((eq (car expr) 'quote)
                      (ensure-space)
                      (insert "'")
                      (lispy--insert (cadr expr)))
                     ((setq str (cdr (assoc (car expr) lispy--insert-alist)))
                      (ensure-space)
                      (insert str)
                      (lispy--insert (cadr expr)))
                     ((eq (car expr) 'function)
                      (ensure-space)
                      (insert (format "#'%S" (cadr expr))))
                     (t
                      (ensure-space)
                      (insert "()")
                      (backward-char 1)
                      (mapc #'lispy--insert expr)
                      (forward-char 1)))))
            (t
             (ensure-space)
             (insert (prin1-to-string expr)))))))

(defun lispy-expr-canonical-p (str)
  "Return t if STR is the same when read and re-inserted."
  (interactive
   (list (lispy--string-dwim (lispy--bounds-list))))
  (let* ((mode major-mode)
         (result (string=
                  str
                  (with-temp-buffer
                    (funcall mode)
                    (lispy--insert (lispy--read str))
                    (buffer-substring-no-properties
                     (point-min)
                     (point-max))))))
    (when (called-interactively-p 'any)
      (message "%s" result))
    result))

(defun lispy--whitespacep (x)
  "Check if X is a whitespace tag."
  (and (consp x)
       (eq (car x) 'ly-raw)
       (or (eq (cadr x) 'newline)
           (eq (cadr x) 'comment))))

(defun lispy--function-str (fun)
  "Return FUN definition as a string."
  (if (fboundp fun)
      (cl-destructuring-bind (buf . pt)
          (save-window-excursion
            (save-excursion
              (find-function-noselect fun)))
        (with-current-buffer buf
          (goto-char pt)
          (lispy--string-dwim)))
    (error "%s isn't bound" fun)))

(defun lispy--function-parse (str)
  "Extract the function body and args from it's expression STR."
  (let ((body (lispy--read str))
        args)
    (if (eq (car body) 'defun)
        (setq body (lispy--whitespace-trim (cdr body)))
      (error "Expected defun, got %s" (car body)))
    (if (symbolp (car body))
        (setq body (lispy--whitespace-trim (cdr body)))
      (error "Expected function name, got %s" (car body)))
    (if (listp (car body))
        (progn
          (setq args (car body))
          (setq body (lispy--whitespace-trim (cdr body))))
      (error "Expected function arguments, got %s" (car body)))
    ;; skip docstring
    (if (and (listp (car body))
             (eq (caar body) 'ly-raw)
             (eq (cadar body) 'string))
        (setq body (lispy--whitespace-trim (cdr body))))
    ;; skip declare
    (if (and (listp (car body))
             (eq (caar body) 'declare))
        (setq body (lispy--whitespace-trim (cdr body))))
    ;; skip interactive
    (if (and (listp (car body))
             (eq (caar body) 'interactive))
        (setq body (lispy--whitespace-trim (cdr body))))
    (list args body)))

(defun lispy-to-ifs ()
  "Transform current `cond' expression to equivalent `if' expressions."
  (interactive)
  (lispy-from-left
   (let* ((bnd (lispy--bounds-dwim))
          (expr (lispy--read (lispy--string-dwim bnd))))
     (unless (eq (car expr) 'cond)
       (error "%s isn't cond" (car expr)))
     (delete-region (car bnd) (cdr bnd))
     (lispy--fast-insert
      (car
       (lispy--whitespace-trim
        (lispy--cases->ifs (cdr expr)))))))
  (lispy-from-left
   (indent-sexp)))

(defun lispy--fast-insert (f-expr)
  "`lispy--insert' EXPR into a temp buffer and return `buffer-string'."
  (insert
   (with-temp-buffer
     (emacs-lisp-mode)
     (lispy--insert f-expr)
     (buffer-string))))

(defun lispy-to-cond ()
  "Reverse of `lispy-to-ifs'."
  (interactive)
  (lispy-from-left
   (let* ((bnd (lispy--bounds-dwim))
          (expr (lispy--read (lispy--string-dwim bnd))))
     (unless (eq (car expr) 'if)
       (error "%s isn't if" (car expr)))
     (delete-region (car bnd) (cdr bnd))
     (lispy--fast-insert
      (cons 'cond (lispy--ifs->cases expr)))))
  (lispy-from-left
   (indent-sexp)))

(defun lispy--case->if (case &optional else)
  "Return an if statement based on  CASE statement and ELSE."
  (append
   `(if ,(car case))
   (cond ((null (cdr case)) `((ly-raw newline) nil ,@else))
         ((= (length (cl-remove-if #'lispy--whitespacep (cdr case))) 1)
          (append (cdr case) else))
         (t
          (let ((p (or (cl-position-if-not
                        #'lispy--whitespacep
                        (cdr case))
                       -1)))
            `(,@(cl-subseq (cdr case) 0 p)
                (progn
                  (ly-raw newline)
                  ,@(cl-subseq (cdr case) p))
                ,@else))))))

(defun lispy--cases->ifs (cases)
  "Return nested if statements that correspond to CASES."
  (cond ((= 1 (length cases))
         (if (eq (caar cases) t)
             (let ((then (cdar cases)))
               (if (equal (car then) '(ly-raw newline))
                   (cdr then)
                 then))
           (list (lispy--case->if (car cases)))))
        ((lispy--whitespacep (car cases))
         (cons (car cases)
               (lispy--cases->ifs (cdr cases))))
        (t
         (list
          (lispy--case->if
           (car cases)
           (lispy--cases->ifs (cdr cases)))))))

(defun lispy--whitespace-trim (x)
  "Trim whitespace from start of X."
  (cl-subseq x (cl-position-if-not #'lispy--whitespacep x)))

(defun lispy--if->case (cnd then)
  "Return a case statement corresponding to if with CND and THEN."
  (cond ((null then)
         (reverse (lispy--whitespace-trim (reverse cnd))))
        ((and (listp then) (eq (car then) 'progn))
         (append cnd (lispy--whitespace-trim (cdr then))))
        (t
         (append cnd (list then)))))

(defun lispy--ifs->cases (ifs)
  "Return a list of cases corresponding to nested IFS."
  (let (result ifs1)
    (if (eq (car ifs) 'if)
        (setq ifs1 (cdr ifs))
      (error "Unexpected"))
    (while ifs1
      (let* ((p1 (cl-position-if-not #'lispy--whitespacep ifs1))
             (whitespace1 (cl-subseq ifs1 0 p1))
             (ifs2 (cl-subseq ifs1 (1+ p1)))
             (p2 (cl-position-if-not #'lispy--whitespacep ifs2))
             (cnd (cl-subseq ifs1 p1 (+ p1 (1+ p2))))
             (then (nth p2 ifs2))
             (ifs3 (cl-subseq ifs2 (1+ p2)))
             (p3 (cl-position-if-not #'lispy--whitespacep ifs3))
             (whitespace2 (cl-subseq ifs3 0 p3))
             (ifs4 (and ifs3 (cl-subseq ifs3 p3))))
        (when whitespace1
          (setq result (append result whitespace1)))
        (setq result (append result (list (lispy--if->case cnd then))))
        (setq result (append result whitespace2))
        (if (and (eq (length ifs4) 1)
                 (listp (car ifs4))
                 (eq (caar ifs4) 'if))
            (setq ifs1 (cdar ifs4))
          (when ifs4
            (setq result (append result
                                 `((t (ly-raw newline) ,@ifs4)))))
          (setq ifs1))))
    result))

(defun lispy--replace (lst from to)
  "Recursively replace elements in LST from FROM to TO."
  (cond ((eq lst from)
         to)
        ((not (consp lst))
         lst)
        (t
         (cons
          (lispy--replace (car lst) from to)
          (lispy--replace (cdr lst) from to)))))

;; ——— Utilities: rest —————————————————————————————————————————————————————————
(defun lispy--indent-for-tab ()
  "Call `indent-for-tab-command'."
  (let ((tab-always-indent t))
    (indent-for-tab-command)))

(defun lispy--remove-gaps ()
  "Remove dangling `\\s)'."
  (when (or (looking-back "[^ \t\n]\\([ \t\n]+\\)\\s)")
            (and (looking-back "\\s)\\([ \t\n]+\\)")
                 (not (looking-at lispy-left)))
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

(defun lispy--mark (bnd)
  "Mark BND.  BND is a cons of beginning and end positions."
  (destructuring-bind (beg . end) bnd
    (goto-char beg)
    (set-mark-command nil)
    (goto-char end)))

(defun lispy--space-unless (context)
  "Insert one space.
Unless inside string or comment, or `looking-back' at CONTEXT."
  (unless (or lispy-no-space (lispy--in-string-or-comment-p)
              (looking-back context))
    (insert " ")))

(defun lispy--reindent (&optional arg)
  "Reindent current sexp.  Up-list ARG times before that."
  (cond ((region-active-p)
         (indent-region (region-beginning)
                        (region-end)))
        (arg
         (lispy-save-excursion
           (lispy--out-forward arg)
           (backward-list)
           (indent-sexp)))

        ((looking-back lispy-right)
         (save-excursion
           (backward-list)
           (indent-sexp)))

        ((looking-at lispy-left)
         (indent-sexp))

        (t
         (save-excursion
           (lispy--out-forward 1)
           (backward-list)
           (indent-sexp)))))

(defun lispy--delete ()
  "Delete one sexp."
  (unless (looking-at lispy-left)
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

(declare-function helm "ext:helm")

(defun lispy--select-candidate (candidates action)
  "Select from CANDIDATES list with `helm'.
ACTION is called for the selected candidate."
  (require 'helm)
  (require 'helm-help)
  ;; allows restriction with space
  (require 'helm-match-plugin)
  (let (helm-update-blacklist-regexps)
    (helm :sources
          `((name . "Candidates")
            (candidates . ,(mapcar
                            (lambda (x)
                              (if (listp x)
                                  (if (stringp (cdr x))
                                      (cons (cdr x) (car x))
                                    (cons (car x) x))
                                x)) candidates))
            (action . ,action)
            (pattern-transformer . regexp-quote)))))

(defun lispy--action-jump (tag)
  "Jump to TAG."
  (when (semantic-tag-p tag)
    (let ((overlay (semantic-tag-overlay tag)))
      (cond ((overlayp overlay))
            ((arrayp overlay)
             (semantic--tag-set-overlay
              tag
              (setq overlay
                    (make-overlay (aref overlay 0)
                                  (aref overlay 1)
                                  (find-file (aref overlay 2))))))
            (t (error "Unexpected")))
      (push-mark)
      (switch-to-buffer (overlay-buffer overlay))
      (goto-char (overlay-start overlay))
      (lispy--ensure-visible)
      ;; (semantic-go-to-tag tag)
      (when (eq major-mode 'clojure-mode)
        (ignore-errors (up-list 1))
        (backward-list 1))
      (switch-to-buffer (current-buffer)))))

(defun lispy--recenter-bounds (bnd)
  "Make sure BND is visible in window.
BND is a cons of start and end points."
  (cond ((> (count-lines (car bnd) (cdr bnd))
            (window-height)))
        ((< (car bnd) (window-start))
         (save-excursion
           (goto-char (car bnd))
           (recenter 0)))
        ((> (cdr bnd) (window-end))
         (save-excursion
           (goto-char (cdr bnd))
           (recenter -1)))))

(defun lispy--ace-do (x bnd &optional filter func no-narrow)
  "Use `ace-jump-do' to X within BND when FILTER return t.
When FUNC is not nil, call it after a successful move.
When NO-NARROW is not nil, don't narrow to BND."
  (require 'ace-jump-mode)
  (lispy--recenter-bounds bnd)
  (let ((already-narrowed (lispy--buffer-narrowed-p)))
    (unless no-narrow
      (narrow-to-region (car bnd) (cdr bnd)))
    (when func
      (setq ace-jump-mode-end-hook
            (list `(lambda ()
                     (setq ace-jump-mode-end-hook)
                     (,func)))))
    (let ((ace-jump-mode-scope 'window)
          (ace-jump-search-filter filter))
      (unwind-protect
           (ace-jump-do x)
        (unless already-narrowed
          (widen))))))

(defun lispy--normalize (arg)
  "Go up ARG times and normalize."
  (lispy--out-backward arg)
  (let ((bnd (lispy--bounds-list)))
    (save-excursion
      (narrow-to-region (car bnd) (cdr bnd))
      (lispy--do-replace "[^ ]\\( \\{2,\\}\\)[^ ]" " ")
      (lispy--do-replace "[^\\\\]\\(([\n ]+\\)" "(")
      (lispy--do-replace "\\([\n ]+)\\)" ")")
      (lispy--do-replace "\\([ ]+\\)\n" "")
      (lispy--do-replace ")\\([^] \n)}]\\)" " \\1")
      (lispy--do-replace "\\(( (\\)" "((")
      (widen))))

(defun lispy--do-replace (from to)
  "Replace first match group of FROM to TO."
  (goto-char (point-min))
  (let (mb me ms)
    (while (and (re-search-forward from nil t)
                (setq mb (match-beginning 1))
                (setq me (match-end 1))
                (setq ms (match-string 1)))
      (goto-char mb)
      (if (or (lispy--in-string-or-comment-p)
              (looking-back "^"))
          (goto-char me)
        (delete-region mb me)
        (when (cl-search "\\1" to)
          (setq to (replace-regexp-in-string "\\\\1" ms to)))
        (insert to)))))

(defun lispy--teleport (beg end endp regionp)
  "Move text from between BEG END to point.
Leave point at the beginning or end of text depending on ENDP.
Make text marked if REGIONP is t."
  (let ((str (buffer-substring-no-properties beg end))
        (beg1 (1+ (point)))
        end1
        (size (buffer-size))
        (deactivate-mark nil))
    (if (and (>= (point) beg)
             (<= (point) end))
        (progn
          (message "Can't teleport expression inside itself")
          (goto-char beg))
      (goto-char beg)
      (delete-region beg end)
      (delete-blank-lines)
      (when (> beg1 beg)
        (decf beg1 (- size (buffer-size))))
      (goto-char beg1)
      (when (looking-at "(")
        (save-excursion
          (newline-and-indent)))
      (unless (looking-back "[ (]")
        (insert " ")
        (incf beg1))
      (insert str)
      (unless (looking-at "[\n)]")
        (insert "\n")
        (backward-char))
      (lispy-save-excursion
        (lispy--reindent 1)
        (goto-char (1- beg1))
        (indent-sexp))
      (if regionp
          (progn
            (setq deactivate-mark nil)
            (set-mark-command nil)
            (goto-char beg1)
            (when endp
              (exchange-point-and-mark)))
        (unless endp
          (goto-char beg1))))))

(defun lispy--swap-regions (bnd1 bnd2)
  "Swap buffer regions BND1 and BND2."
  (when (> (car bnd1) (car bnd2))
    (cl-rotatef bnd1 bnd2))
  (let ((str1 (lispy--string-dwim bnd1))
        (str2 (lispy--string-dwim bnd2)))
    (goto-char (car bnd2))
    (delete-region (car bnd2) (cdr bnd2))
    (insert str1)
    (goto-char (car bnd1))
    (delete-region (car bnd1) (cdr bnd1))
    (insert str2)
    (goto-char (car bnd1))))

(defun lispy--ensure-visible ()
  "Remove overlays hiding point."
  (let ((overlays (overlays-at (point)))
        ov invis-prop expose)
    (while (setq ov (pop overlays))
      (if (and (invisible-p (overlay-get ov 'invisible))
               (setq expose (overlay-get ov 'isearch-open-invisible)))
          (funcall expose ov)))))

(defun lispy--delete-pair-in-string (left right)
  "Delete a pair of LEFT and RIGHT in string."
  (let ((bnd (lispy--bounds-string)))
    (when bnd
      (let ((pos (cond ((looking-at left)
                        (save-excursion
                          (let ((b1 (match-beginning 0))
                                (e1 (match-end 0))
                                b2 e2)
                            (when (re-search-forward right (cdr bnd) t)
                              (setq b2 (match-beginning 0)
                                    e2 (match-end 0))
                              (delete-region b2 e2)
                              (delete-region b1 e1)
                              b1))))
                       ((looking-at right)
                        (save-excursion
                          (let ((b1 (match-beginning 0))
                                (e1 (match-end 0))
                                b2 e2)
                            (when (re-search-backward left (car bnd) t)
                              (setq b2 (match-beginning 0)
                                    e2 (match-end 0))
                              (delete-region b1 e1)
                              (delete-region b2 e2)
                              (+ (point) (- b1 e2)))))))))
        (when pos
          (goto-char pos))))))

(defun lispy--make-ediff-buffer (buffer ext bnd)
  "Create a copy of BUFFER with EXT added to the name.
Use only the part bounded by BND."
  (multiple-value-bind (name mode str)
      (with-current-buffer buffer
        (list (concat (buffer-name) ext) major-mode (lispy--string-dwim bnd)))
    (with-current-buffer (get-buffer-create name)
      (funcall mode)
      (insert str "\n")
      (indent-region (point-min) (point-max))
      (setq ediff-temp-indirect-buffer t)
      (list (current-buffer) (point-min) (point-max)))))

(defun lispy--insert-or-call (def &optional from-start)
  "Return a lambda to call DEF if position is special.
Otherwise call `self-insert-command'.
When FROM-START is t, call DEF as if it was invoked from beginning of
list."
  `(lambda ,(help-function-arglist def)
     ,(format "Call `%s' when special, self-insert otherwise.\n\n%s"
              (symbol-name def) (documentation def))
     ,(interactive-form def)
     (let (cmd)
       (cond ((and (bound-and-true-p edebug-active)
                   (= 1 (length (this-command-keys)))
                   (let ((char (aref (this-command-keys) 0)))
                     (setq cmd (or (assq char edebug-mode-map)
                                   (assq char global-edebug-map)))))
              (call-interactively (cdr cmd)))

             ((region-active-p)
              (call-interactively ',def))

             ((lispy--in-string-or-comment-p)
              (self-insert-command 1))

             ((looking-at lispy-left)
              (call-interactively ',def))

             ((looking-back lispy-right)
              ,@(and from-start '((backward-list)))
              (unwind-protect
                   (call-interactively ',def)
                ,@(and from-start '((forward-list)))))

             ((or (and (looking-back "^ *") (looking-at ";"))
                  (and (= (point) (point-max))
                       (memq ',def `(outline-previous-visible-heading))))
              (call-interactively ',def))

             (t
              (self-insert-command 1))))))

;; ——— Key definitions —————————————————————————————————————————————————————————
(defvar ac-trigger-commands '(self-insert-command))
(defvar company-begin-commands '(self-insert-command))
(defvar company-no-begin-commands '(special-lispy-space))
(defvar mc/cmds-to-run-for-all nil)
(defvar mc/cmds-to-run-once nil)

(defadvice ac-handle-post-command (around ac-post-command-advice activate)
  "Don't `auto-complete' when region is active."
  (unless (region-active-p)
    ad-do-it))

(defvar lispy-mode-x-map (make-sparse-keymap))

(let ((map lispy-mode-x-map))
  (define-key map "d" 'lispy-to-defun)
  (define-key map "l" 'lispy-to-lambda)
  (define-key map "e" 'lispy-edebug)
  (define-key map "m" 'lispy-cursor-ace)
  (define-key map "i" 'lispy-to-ifs)
  (define-key map "c" 'lispy-to-cond)
  (define-key map "f" 'lispy-flatten)
  (define-key map "r" 'lispy-eval-and-replace))

(defun lispy-define-key (keymap key def &optional from-start)
  "Forward to (`define-key' KEYMAP KEY FUNC).
FUNC is obtained from (`lispy--insert-or-call' DEF FROM-START)"
  (let ((func (defalias (intern (concat "special-" (symbol-name def)))
                  (lispy--insert-or-call def from-start))))
    (unless (member func ac-trigger-commands)
      (push func ac-trigger-commands))
    (unless (member func company-begin-commands)
      (unless (member func company-no-begin-commands)
        (push func company-begin-commands)))
    (unless (or (member func mc/cmds-to-run-for-all)
                (member func mc/cmds-to-run-once))
      (push func mc/cmds-to-run-for-all))
    (eldoc-add-command func)
    (define-key keymap (kbd key) func)))

(let ((map lispy-mode-map))
  ;; ——— globals: navigation ——————————————————
  (define-key map (kbd "]") 'lispy-forward)
  (define-key map (kbd "[") 'lispy-backward)
  (define-key map (kbd ")") 'lispy-out-forward-nostring)
  ;; ——— globals: kill-related ————————————————
  (define-key map (kbd "C-k") 'lispy-kill)
  (define-key map (kbd "C-y") 'lispy-yank)
  (define-key map (kbd "C-d") 'lispy-delete)
  (define-key map (kbd "DEL") 'lispy-delete-backward)
  (define-key map (kbd "M-m") 'lispy-mark-symbol)
  (define-key map (kbd "C-c p") 'lispy-pop-copy)
  (define-key map (kbd "C-,") 'lispy-kill-at-point)
  (define-key map (kbd "C-M-,") 'lispy-mark)
  ;; ——— globals: pairs ———————————————————————
  (define-key map (kbd "(") 'lispy-parens)
  (define-key map (kbd "{") 'lispy-braces)
  (define-key map (kbd "}") 'lispy-brackets)
  (define-key map (kbd "\"") 'lispy-quotes)
  ;; ——— globals: insertion ———————————————————
  (define-key map (kbd ":") 'lispy-colon)
  (define-key map (kbd "^") 'lispy-hat)
  (define-key map (kbd "C-j") 'lispy-newline-and-indent)
  (define-key map (kbd "M-j") 'lispy-split)
  (define-key map (kbd "RET") 'newline-and-indent)
  (define-key map (kbd ";") 'lispy-comment)
  (define-key map (kbd "C-e") 'lispy-move-end-of-line)
  ;; ——— globals: C-1 .. C-9 ——————————————————
  (when lispy-use-ctrl-digits
    (define-key map (kbd "C-1") 'lispy-describe-inline)
    (define-key map (kbd "C-2") 'lispy-arglist-inline)
    (define-key map (kbd "C-3") 'lispy-out-forward)
    (define-key map (kbd "C-4") 'lispy-x)
    (define-key map (kbd "C-7") 'lispy-cursor-down)
    (define-key map (kbd "C-8") 'lispy-parens-down)
    (define-key map (kbd "C-9") 'lispy-out-forward-newline))
  ;; ——— globals: miscellanea —————————————————
  (define-key map (kbd "M-o") 'lispy-string-oneline)
  (define-key map (kbd "M-i") 'lispy-iedit)
  ;; ——— locals: navigation ———————————————————
  (lispy-define-key map "l" 'lispy-out-forward)
  (lispy-define-key map "a" 'lispy-out-backward)
  (lispy-define-key map "f" 'lispy-flow)
  (lispy-define-key map "j" 'lispy-down)
  (lispy-define-key map "k" 'lispy-up)
  (lispy-define-key map "d" 'lispy-different)
  (lispy-define-key map "o" 'lispy-counterclockwise)
  (lispy-define-key map "p" 'lispy-clockwise)
  (lispy-define-key map "J" 'lispy-outline-next)
  (lispy-define-key map "K" 'lispy-outline-prev)
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
  ;; ——— locals: marking ——————————————————————
  (lispy-define-key map "h" 'lispy-ace-symbol)
  (lispy-define-key map "H" 'lispy-ace-symbol-replace)
  (lispy-define-key map "m" 'lispy-mark-list)
  ;; ——— locals: dialect-specific —————————————
  (lispy-define-key map "e" 'lispy-eval)
  (lispy-define-key map "E" 'lispy-eval-and-insert)
  (lispy-define-key map "G" 'lispy-goto-local)
  (lispy-define-key map "g" 'lispy-goto)
  (lispy-define-key map "F" 'lispy-follow t)
  (lispy-define-key map "D" 'lispy-describe)
  (lispy-define-key map "A" 'lispy-arglist)
  ;; ——— locals: miscellanea ——————————————————
  (lispy-define-key map "SPC" 'lispy-space)
  (lispy-define-key map "i" 'lispy-tab t)
  (lispy-define-key map "I" 'lispy-shifttab)
  (lispy-define-key map "N" 'lispy-narrow)
  (lispy-define-key map "W" 'lispy-widen)
  (lispy-define-key map "c" 'lispy-clone)
  (lispy-define-key map "u" 'lispy-undo)
  (lispy-define-key map "q" 'lispy-ace-paren)
  (lispy-define-key map "Q" 'lispy-ace-char)
  (lispy-define-key map "v" 'lispy-view t)
  (lispy-define-key map "T" 'lispy-ert)
  (lispy-define-key map "t" 'lispy-teleport)
  (lispy-define-key map "n" 'lispy-new-copy)
  (lispy-define-key map "b" 'lispy-store-region-and-buffer)
  (lispy-define-key map "B" 'lispy-ediff-regions)
  (lispy-define-key map "x" 'lispy-x)
  (lispy-define-key map "X" 'lispy-edebug-stop)
  (lispy-define-key map "V" 'lispy-visit)
  (lispy-define-key map "-" 'lispy-ace-subword)
  ;; ——— locals: digit argument ———————————————
  (mapc (lambda (x) (lispy-define-key map (format "%d" x) 'digit-argument))
        (number-sequence 0 9)))

(provide 'lispy)

;;; Local Variables:
;;; outline-regexp: ";; ———"
;;; End:

;;; lispy.el ends here
