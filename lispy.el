;;; lispy.el --- vi-like Paredit.

;; Copyright (C) 2014 Oleh Krehel

;; Author: Oleh Krehel <ohwoeowho@gmail.com>
;; URL: https://github.com/abo-abo/lispy
;; Version: 0.20.0
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
;; When special, the digit keys call `digit-argument', since most
;; `lispy' commands accept a numeric argument.  For instance, "3c" is
;; equivalent to "ccc" (clone sexp 3 times), and "4j" is equivalent to
;; "jjjj" (move point 4 sexps down).  Some useful applications are
;; "9l" and "9a" - they exit list forwards and backwards respectively
;; at most 9 times which makes them effectively equivalent to
;; `end-of-defun' and `beginning-of-defun'.
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
;; | >   | `lispy-slurp'            | <          | `lispy-barf'      |
;; | c   | `lispy-clone'            | C-d or DEL |                   |
;; | C   | `lispy-convolute'        | C          | reverses itself   |
;; | d   | `lispy-different'        | d          | reverses itself   |
;; | M-j | `lispy-split'            | +          | `lispy-join'      |
;; | O   | `lispy-oneline'          | M          | `lispy-multiline' |
;; | S   | `lispy-stringify'        | C-u "      | `lispy-quotes'    |
;; | ;   | `lispy-comment'          | C-u ;      | `lispy-comment'   |
;; | xi  | `lispy-to-ifs'           | xc         | `lispy-to-cond'   |
;; | F   | `lispy-follow'           | D          | `pop-tag-mark'    |
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
;; |  '  | `lispy-tick'                       |
;; |  `  | `lispy-backtick'                   |
;; | C-m | `lispy-newline-and-indent'         |
;; |-----+------------------------------------|
;;
;; You can see the full list of bound commands with "F1 f lispy-mode".
;;
;; Most special commands will leave the point special after they're
;; done.  This allows to chain them as well as apply them continuously
;; by holding the key.  Some useful holdable keys are "jkf<>cws;".
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
(require 'etags)
(require 'outline)
(require 'semantic)
(require 'semantic/db)
(require 'ace-jump-mode)
(require 'newcomment)
(require 'lispy-inline)
(require 'iedit)
(require 'delsel)
(require 'package)
(require 'highlight)

;; ——— Customization ———————————————————————————————————————————————————————————
(defgroup lispy nil
  "List navigation and editing for the Lisp family."
  :group 'bindings
  :prefix "lispy-")

(defvar lispy-left "[([{]"
  "Opening delimiter.")

(defvar lispy-right "[])}]"
  "Closing delimiter.")

(defcustom lispy-no-space nil
  "If t, don't insert a space before parens/brackets/braces/colons."
  :type 'boolean
  :group 'lispy)
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

(defcustom lispy-verbose t
  "If t, lispy will display some messages on error state.
These messages are similar to \"Beginning of buffer\" error for
`backward-char' and can safely be ignored."
  :type 'boolean
  :group 'lispy)

(defcustom lispy-verbose-verbs t
  "If t, verbs produced by `lispy-defverb' will have a hint in the echo area.
The hint will consist of the possible nouns that apply to the verb."
  :type 'boolean
  :group 'lispy)

(defcustom lispy-fancy-narrow t
  "If t, `lispy-ace-char' will try to use `fancy-narrow-to-region'."
  :type 'boolean
  :group 'lispy)

(defcustom lispy-helm-columns '(1 60 70 80)
  "Start and end positions of columns when completing with `helm'."
  :group 'lispy)

(defcustom lispy-no-permanent-semantic nil
  "When t, `lispy' will not enable function `semantic-mode' when it's off."
  :type 'boolean
  :group 'lispy)

(defface lispy-command-name-face
    '((t (:inherit font-lock-function-name-face)))
  "Face for Elisp commands."
  :group 'lispy-faces)

(defface lispy-occur-face
    '((t (:background "#CECEFF")))
  "Face for `lispy-occur' matches."
  :group 'lispy-faces)

(defvar lispy-mode-map (make-sparse-keymap))

;;;###autoload
(define-minor-mode lispy-mode
    "Minor mode for navigating and editing LISP dialects.

When `lispy-mode' is on, most unprefixed keys,
i.e. [a-zA-Z+-./<>], conditionally call commands instead of
self-inserting. The condition (called special further on) is one
of:

- the point is before \"(\"
- the point is after \")\"
- the region is active

For instance, when special, \"j\" moves down one sexp, otherwise
it inserts itself.

When special, [0-9] call `digit-argument'.

When `lispy-mode' is on, \"[\" and \"]\" move forward and
backward through lists, which is useful to move into special.

\\{lispy-mode-map}"
  :keymap lispy-mode-map
  :group 'lispy
  :lighter " LY"
  (when (and lispy-mode (called-interactively-p 'any))
    (mapc #'lispy-raise-minor-mode
          (cons 'lispy-mode lispy-known-verbs))))

(defun lispy-raise-minor-mode (mode)
  "Make MODE the first on `minor-mode-map-alist'."
  (let ((x (assq mode minor-mode-map-alist)))
      (when x
        (setq minor-mode-map-alist
              (cons x (delq mode minor-mode-map-alist))))))

;; ——— Macros ——————————————————————————————————————————————————————————————————
(defmacro lispy-dotimes (n &rest bodyform)
  "Execute N times the BODYFORM unless an error is signaled.
Return nil if couldn't execute BODYFORM at least once.
Otherwise return the amount of times executed."
  (declare (indent 1))
  `(let ((i 0))
     (catch 'result
       (condition-case e
           (progn
             (while (<= (incf i) ,n)
               ,@bodyform)
             ,n)
         (error
          (when (eq (car e) 'buffer-read-only)
            (message "Buffer is read-only: %s" (current-buffer)))
          (decf i)
          (and (> i 0) i))))))

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
    `(let ((,at-start (lispy--leftp)))
       (unless ,at-start
         (lispy-different))
       (unwind-protect
            (lispy-save-excursion
              ,@body)
         (unless (eq ,at-start (lispy--leftp))
           (lispy-different))))))

;; ——— Verb related ————————————————————————————————————————————————————————————
(defvar lispy-known-verbs nil
  "List of registered verbs.")

(defun lispy-disable-verbs-except (verb)
  "Disable all verbs except VERB."
  (mapc
   (lambda (v) (funcall v -1))
   (remq verb lispy-known-verbs)))

(defun lispy-quit ()
  "Remove modifiers."
  (interactive)
  (lispy-disable-verbs-except nil))

(defmacro lispy-defverb (name grammar)
  "Define the verb NAME.
GRAMMAR is a list of nouns that work with this verb."
  (let* ((sym (intern (format "lispy-%s-mode" name)))
         (keymap (intern (format "lispy-%s-mode-map" name)))
         (doc (format "%s verb.\n\n \\{lispy-%s-mode-map}"
                      (capitalize name) name))
         (lighter (format " [%s]" name))
         (verb (intern (format "lispy-%s-verb" name)))
         (msg (format "[%s]: %s" name
                      (mapconcat #'car grammar " "))))
    `(progn
       (defvar ,sym nil
         ,(format "Non-nil if Lispy-%s mode is enabled.
Use the command `%s' to change this variable."
                  (capitalize name)
                  sym))
       (make-variable-buffer-local ',sym)
       (defvar ,keymap (make-sparse-keymap))
       (defun ,sym (&optional arg)
         ,doc
         (interactive (list (or current-prefix-arg 'toggle)))
         (let ((last-message (current-message)))
           (setq ,sym (if (eq arg 'toggle)
                          (not ,sym)
                        (> (prefix-numeric-value arg)
                           0)))
           (cond (,sym (lispy-disable-verbs-except ',sym))
                 (t nil))
           (if (called-interactively-p 'any)
               (unless (and (current-message)
                            (not (equal last-message (current-message))))
                 (if ,sym
                     (when lispy-verbose-verbs
                       (message ,msg))
                   (message "")))))
         (force-mode-line-update))
       (mapc (lambda (x)
               (lispy-define-key
                   ,keymap
                   (car x) (cadr x)
                 :disable ',sym))
             ',grammar)
       (unless (memq ',sym lispy-known-verbs)
         (push ',sym lispy-known-verbs))
       (defun ,verb ()
         (interactive)
         (if (bound-and-true-p ,sym)
             (,sym -1)
           (,sym 1)))
       (with-no-warnings
         (add-minor-mode ',sym ,lighter ,keymap nil nil)))))

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
        (r (lispy-dotimes arg
             (when (= (point) (point-max))
               (error "Reached end of buffer"))
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
      (point))))

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
        (r (lispy-dotimes arg
             (when (= (point) (point-min))
               (error "Reached beginning of buffer"))
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
      (point))))

(defun lispy-out-forward (arg)
  "Move outside list forwards ARG times.
Return nil on failure, t otherwise."
  (interactive "p")
  (if (region-active-p)
      (lispy-mark-right arg)
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
      (lispy-mark-left arg)
    (lispy--out-backward arg)))

(defun lispy-out-forward-newline (arg)
  "Call `lispy--out-forward', then ARG times `newline-and-indent'."
  (interactive "p")
  (lispy--out-forward 1)
  (lispy-dotimes arg
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
    (lispy-dotimes arg
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
  (save-match-data
    (cond ((region-active-p)
           (if (lispy--symbolp (lispy--string-dwim))
               (if (= (point) (region-end))
                   (lispy-dotimes arg
                     (when (lispy-slurp 1)
                       (lispy-different)
                       (lispy-barf 1)
                       (lispy-different)))
                 (lispy-dotimes arg
                   (lispy-different)
                   (if (lispy-slurp 1)
                       (progn
                         (lispy-different)
                         (lispy-barf 1))
                     (lispy-different))))

             (if (= (point) (region-end))
                 (lispy-dotimes arg
                   (forward-sexp 1)
                   (lispy-different)
                   (if (lispy--in-comment-p)
                       (goto-char (1+ (cdr (lispy--bounds-comment))))
                     (forward-sexp 2)
                     (forward-sexp -1))
                   (lispy-different))
               (lispy-dotimes arg
                 (lispy-different)
                 (if (ignore-errors
                       (forward-sexp 1)
                       t)
                     (progn
                       (lispy-different)
                       (forward-sexp 2)
                       (forward-sexp -1))
                   (lispy-different))))))

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
           (lispy-backward 1))))
  (lispy--ensure-visible))

(defun lispy-up (arg)
  "Move up ARG times inside current list."
  (interactive "p")
  (lispy--ensure-visible)
  (save-match-data
    (cond ((region-active-p)
           (if (lispy--symbolp (lispy--string-dwim))
               (if (= (point) (region-end))
                   (lispy-dotimes arg
                     (lispy-different)
                     (if (lispy-slurp 1)
                         (progn
                           (lispy-different)
                           (lispy-barf 1))
                       (lispy-different)))
                 (lispy-dotimes arg
                   (when (lispy-slurp 1)
                     (lispy-different)
                     (lispy-barf 1)
                     (lispy-different))))
             (if (= (point) (region-end))
                 (lispy-dotimes arg
                   (lispy-different)
                   (if (ignore-errors
                         (backward-sexp 1) t)
                       (progn
                         (lispy-different)
                         (backward-sexp 2)
                         (backward-sexp -1))
                     (lispy-different)
                     (error "Can't move up")))
               (lispy-dotimes arg
                 (backward-sexp 1)
                 (lispy-different)
                 (backward-sexp 2)
                 (backward-sexp -1)
                 (lispy-different)))))

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
           (lispy-forward 1))))
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
         (user-error "Unexpected"))))

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
           (if (= (s-count-matches lispy-left str)
                  (s-count-matches lispy-right str))
               (kill-line)
             (if (let ((lispy-ignore-whitespace t))
                   (lispy--out-forward 1))
                 (progn
                   (backward-char 1)
                   (if (= beg (point))
                       (lispy--out-backward 1)
                     (kill-region beg (point))))
               (kill-region beg (point))))))))

(defun lispy-yank ()
  "Like regular `yank', but quotes body when called from \"|\"."
  (interactive)
  (cond
    ((and (region-active-p)
          (bound-and-true-p delete-selection-mode))
     (delete-active-region)
     (yank))
    ((and (eq (char-after) ?\")
          (eq (char-before) ?\"))
     (insert (replace-regexp-in-string "\"" "\\\\\"" (current-kill 0))))
    (t
     (yank))))

(defun lispy-delete (arg)
  "Delete ARG sexps."
  (interactive "p")
  (cond ((region-active-p)
         (delete-region
          (region-beginning) (region-end)))

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

        ((looking-at lispy-right)
         (lispy-out-backward 1))

        ((looking-at lispy-left)
         (when (looking-back "\\(?:\\s-\\|^\\)[`',@]+")
           (delete-region
            (match-beginning 0)
            (match-end 0))
           (insert " "))

         (lispy-dotimes arg
           (lispy--delete)))

        ((looking-at "\"")
         (let ((bnd (lispy--bounds-string)))
           (delete-region (car bnd)
                          (cdr bnd))
           (let ((pt (point)))
             (skip-chars-forward " ")
             (delete-region pt (point)))))

        ((eolp)
         (delete-char 1)
         (let ((pt (point)))
           (skip-chars-forward " ")
           (delete-region pt (point))
           (unless (or (eolp)
                       (bolp)
                       (looking-back "^ +"))
             (insert " "))))

        (t
         (delete-char arg))))


(defun lispy--delete-whitespace-backward ()
  "Delete spaces backward."
  (let ((pt (point)))
    (skip-chars-backward " ")
    (delete-region (point) pt)))

(defun lispy-delete-backward (arg)
  "From \")|\", delete ARG sexps backwards.
Otherwise (`backward-delete-char-untabify' ARG)."
  (interactive "p")
  (let (pos)
    (cond ((region-active-p)
           (delete-region (region-beginning)
                          (region-end)))
          ((bobp))

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

          ((looking-back "\\\\.")
           (backward-delete-char-untabify arg))

          ((and (looking-back (concat lispy-right " "))
                (looking-at " *$"))
           (backward-delete-char-untabify arg))

          ((or (looking-back lispy-right)
               (and (looking-back (concat lispy-right " "))
                    (looking-at lispy-left)))
           (let ((pt (point)))
             (lispy-backward arg)
             (skip-chars-backward "`',@# \t")
             (delete-region pt (point))
             (unless (or (looking-at " ")
                         (looking-back "^ *")
                         (and (looking-back lispy-right)
                              (not (or (looking-at lispy-left)
                                       (looking-at "\""))))
                         (looking-back lispy-left))
               (just-one-space))
             (setq pt (point))
             (if (and
                  (not (looking-back "^ *"))
                  (not (looking-at lispy-left))
                  (progn
                    (skip-chars-backward " \t\n")
                    (looking-back lispy-right)))
                 (delete-region (point) pt)
               (goto-char pt)
               (indent-for-tab-command))))

          ((and (looking-back lispy-left) (not (looking-back "\\\\.")))
           (lispy--out-forward 1)
           (lispy-delete-backward 1))

          ((looking-back "\"")
           (backward-char 1)
           (let ((bnd (lispy--bounds-string)))
             (delete-region (car bnd)
                            (cdr bnd))
             (lispy--delete-whitespace-backward)
             (unless (looking-at " ")
               (insert " "))
             (indent-for-tab-command)))

          ((looking-back "\" +")
           (let ((pt (point))
                 bnd)
             (goto-char (match-beginning 0))
             (delete-region (car (lispy--bounds-string)) pt))
           (lispy--delete-whitespace-backward)
           (unless (looking-back lispy-left)
             (just-one-space))
           (indent-for-tab-command))

          ((looking-back "^ *")
           (delete-region
            (line-beginning-position)
            (point))
           (unless (bobp)
             (if (save-excursion
                   (backward-char 1)
                   (lispy--in-comment-p))
                 (progn
                   (backward-char 1)
                   (let ((bnd (lispy--bounds-comment)))
                     (delete-region (car bnd) (cdr bnd)))
                   (delete-char 1))
               (backward-delete-char 1)
               (unless (or (eolp)
                           (looking-at lispy-right)
                           (looking-back lispy-left))
                 (just-one-space)))
             (indent-for-tab-command)))

          (t
           (backward-delete-char-untabify arg))))
  (when (and (buffer-file-name)
             (< (- (line-number-at-pos (point))
                   (line-number-at-pos (window-start)))
                5))
    (ignore-errors
      (recenter -20)))
  (when (looking-at lispy-left)
    (indent-sexp)))

(defun lispy-mark ()
  "Mark the quoted string or the list that includes the point.
Extend region when it's aleardy active."
  (interactive)
  (let ((bounds (or (lispy--bounds-comment)
                    (lispy--bounds-string)
                    (lispy--bounds-list))))
    (when bounds
      (lispy--mark bounds))))

(defun lispy-mark-list (arg)
  "Mark list from special position.
When ARG is more than 1, mark ARGth element."
  (interactive "p")
  (cond ((> arg 1)
         (lispy-mark-car)
         (lispy-down (1- arg)))
        ((region-active-p)
         (deactivate-mark)
         (when (lispy--in-comment-p)
           (beginning-of-line)
           (skip-chars-forward " ")))
        ((looking-at lispy-left)
         (lispy--mark
          (lispy--bounds-dwim)))
        ((looking-back lispy-right)
         (lispy--mark
          (lispy--bounds-dwim))
         (lispy-different))
        ((and (looking-back "^ *") (looking-at ";"))
         (lispy--mark (lispy--bounds-comment)))))

(defun lispy-mark-symbol ()
  "Mark current symbol."
  (interactive)
  (let (bnd)
    (cond ((and lispy-bind-var-in-progress iedit-mode)
           (iedit-mode)
           (setq lispy-bind-var-in-progress nil)
           (set-mark (point))
           (search-backward (funcall iedit-current-symbol)))

          ((lispy--in-comment-p)
           (lispy--mark (lispy--bounds-comment)))

          ((and
            (not (region-active-p))
            (lispy--in-string-p)
            (= (1+ (point))
               (cdr (setq bnd (lispy--bounds-string)))))
           (lispy--mark bnd))

          ((or (looking-at "[ ]*[()]")
               (and (region-active-p)
                    (looking-at "[ \n]*[()]")))
           (let ((pt (point)))
             (skip-chars-forward "() \n")
             (set-mark-command nil)
             (condition-case nil
                 (progn
                   (re-search-forward "[() \n]")
                   (while (lispy--in-string-or-comment-p)
                     (re-search-forward "[() \n]"))
                   (backward-char 1))
               (error
                (message "No further symbols found")
                (deactivate-mark)
                (goto-char pt)))))

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
           (lispy--mark (lispy--bounds-dwim))))))

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
            (lispy--surround-region ,left ,right)
            (when (and (looking-at lispy-left)
                       (looking-back lispy-left))
              (insert " ")
              (backward-char 1)))
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
            (if (looking-back lispy-right)
                (backward-list)
              (if (looking-at (concat " *" lispy-right))
                  (backward-sexp)))
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
         (let ((str (lispy--string-dwim)))
           (if (and arg
                    (= (aref str 0) ?\")
                    (= (aref str (1- (length str))) ?\"))
               (let* ((bnd (cons (region-beginning)
                                 (region-end)))
                      (str (lispy--string-dwim bnd)))
                 (deactivate-mark)
                 (delete-region (car bnd) (cdr bnd))
                 (insert (read str)))
             (if (lispy--in-string-p)
                 (lispy--surround-region "\\\"" "\\\"")
               (lispy--surround-region "\"" "\"")))))
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

        ((looking-back "?\\\\")
         (self-insert-command 1))

        (t
         (lispy--space-unless "^\\|\\s-\\|\\s(\\|[#]")
         (insert "\"\"")
         (unless (or (lispy--in-string-p)
                     (looking-at "\n\\|)\\|}\\|\\]\\|$"))
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
  (lispy--space-unless "\\s-\\|\\s(\\|[:?]\\\\?")
  (insert "^"))

(defun lispy-tick ()
  "Insert '."
  (interactive)
  (lispy--space-unless "\\s-\\|\\s(\\|[#:?]\\\\?")
  (insert "'"))

(defun lispy-backtick ()
  "Insert `."
  (interactive)
  (lispy--space-unless "\\s-\\|\\s(\\|[:?]\\\\?")
  (insert "`"))

(defun lispy-newline-and-indent ()
  "Insert newline."
  (interactive)
  (if (eq major-mode 'lisp-interaction-mode)
      (progn
        (setq this-command 'eval-last-sexp)
        (eval-print-last-sexp))
    (if (looking-at lispy-left)
        (progn
          (skip-chars-backward ",@'`#")
          (newline-and-indent)
          (skip-chars-forward ",@'`#")
          (indent-sexp))
      (newline-and-indent))))

(defun lispy-newline-and-indent-plain ()
  "When in minibuffer, exit it.  Otherwise forward to `newline-and-indent'."
  (interactive)
  (cl-case major-mode
    (minibuffer-inactive-mode
     (exit-minibuffer))
    (cider-repl-mode
     (cider-repl-return))
    (slime-repl-mode
     (slime-repl-return))
    (t
     (newline-and-indent))))

(defun lispy-open-line ()
  "Add one line after the current expression."
  (interactive)
  (cond ((looking-at lispy-left)
         (save-excursion
           (forward-list)
           (newline)))
        ((looking-back lispy-right)
         (save-excursion (newline)))
        (t
         (save-excursion
           (lispy--out-forward 1)
           (newline)))))

(defun lispy-alt-line ()
  "Add a newline and move there."
  (interactive)
  (when (bound-and-true-p abbrev-mode)
    (expand-abbrev))
  (cond ((looking-at lispy-left)
         (lispy-different))
        ((re-search-forward lispy-right (line-end-position) t)
         (backward-char 1)
         (lispy-out-forward 1))
        (t
         (move-end-of-line 1)))
  (newline-and-indent))

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
  "Wrap around `iedit'."
  (interactive)
  (if iedit-mode
      (iedit-mode nil)
    (when (looking-at lispy-left)
      (forward-char 1))
    (iedit-mode 0)))

;; ——— Locals:  Navigation —————————————————————————————————————————————————————
(defvar lispy--occur-beg 1
  "Start position of the top level sexp during `lispy-occur'.")
(defvar lispy--occur-end 1
  "End position of the top level sexp during `lispy-occur'.")
(defvar lispy--occur-buffer nil
  "Current buffer for `lispy-occur'.")

(defun lispy--occur-action (x)
  "Goto line X for `lispy-occur'."
  (goto-char lispy--occur-beg)
  (let (str-or-comment)
    (cond ((string= helm-input "")
           (forward-line x)
           (back-to-indentation)
           (when (re-search-forward lispy-left (line-end-position) t)
             (goto-char (match-beginning 0))))

          ((setq str-or-comment
                 (progn
                   (forward-line (1- x))
                   (re-search-forward
                    (lispy--occur-regex) (line-end-position) t)
                   (lispy--in-string-or-comment-p)))
           (goto-char str-or-comment))

          ((re-search-backward lispy-left (line-beginning-position) t)
           (goto-char (match-beginning 0)))

          ((re-search-forward lispy-left (line-end-position) t)
           (goto-char (match-beginning 0)))

          (t
           (back-to-indentation)))))

(defun lispy-occur ()
  "Select a line within current top level sexp with `helm'."
  (interactive)
  (deactivate-mark)
  (unwind-protect
       (helm :sources
             `((name . "this defun")
               (init
                . (lambda ()
                    (setq lispy--occur-buffer (current-buffer))
                    (let ((bnd (save-excursion
                                 (unless (and (looking-at "(")
                                              (looking-back "^"))
                                   (beginning-of-defun))
                                 (lispy--bounds-dwim))))
                      (setq lispy--occur-beg (car bnd))
                      (setq lispy--occur-end (cdr bnd))
                      (helm-init-candidates-in-buffer
                          'global
                        (buffer-substring
                         lispy--occur-beg
                         lispy--occur-end)))
                    (add-hook 'helm-move-selection-after-hook
                              #'lispy--occur-update-sel)
                    (add-hook 'helm-update-hook
                              #'lispy--occur-update-input)))
               (candidates-in-buffer)
               (get-line . lispy--occur-get-line)
               (regexp . (lambda () helm-input))
               (action . lispy--occur-action))
             :preselect
             (let ((start-line
                    (save-excursion
                      (unless (and (looking-at "(")
                                   (looking-back "^"))
                        (beginning-of-defun))
                      (line-number-at-pos (point)))))
               (format "^%d"
                       (-
                        (line-number-at-pos (point))
                        start-line)))
             :buffer "*lispy-occur*")
    ;; cleanup
    (remove-hook 'helm-move-selection-after-hook
                 #'lispy--occur-update-sel)
    (remove-hook 'helm-update-hook
                 #'lispy--occur-update-input)
    (with-current-buffer lispy--occur-buffer
      (hlt-unhighlight-region
       lispy--occur-beg
       lispy--occur-end)
      (isearch-dehighlight))))

(defun lispy--occur-regex ()
  "Re-build regex in case it has a space."
  (mapconcat
   #'identity
   (split-string helm-input " +" t)
   ".*"))

(defun lispy--occur-update-input ()
  "Update selection for `lispy-occur'."
  (with-current-buffer lispy--occur-buffer
    (hlt-unhighlight-region
     lispy--occur-beg
     lispy--occur-end)
    (when (> (length helm-input) 1)
      (hlt-highlight-regexp-region
       lispy--occur-beg
       lispy--occur-end
       (lispy--occur-regex)
       'lispy-occur-face))))

(defun lispy--occur-update-sel ()
  "Update selection for `lispy-occur'."
  (let* ((str (buffer-substring-no-properties
               (point-at-bol)
               (point-at-eol)))
         (num (if (string-match "^[0-9]+" str)
                  (string-to-number (match-string 0 str))
                0))
         pt)
    (with-current-buffer lispy--occur-buffer
      (goto-char lispy--occur-beg)
      (forward-line (1- num))
      (when (re-search-forward (lispy--occur-regex)
                               lispy--occur-end t)
        (isearch-highlight (match-beginning 0)
                           (match-end 0))
        (setq pt (match-beginning 0))))
    (when pt
      (with-selected-window
          (helm-persistent-action-display-window)
        (goto-char pt)))))

(defun lispy--occur-get-line (s e)
  "Highlight between S and E for `lispy-occur'."
  (let ((line (buffer-substring s e)))
    (propertize
     (format "%d %s"
             (1- (line-number-at-pos s))
             line)
     'helm-realvalue
     (1- (line-number-at-pos s)))))

;; ——— Locals:  Paredit transformations ————————————————————————————————————————
(defun lispy--sub-slurp-forward (arg)
  "Grow current marked symbol by ARG words forwards.
Return the amount of successful grow steps, nil instead of zero."
  (when (looking-at "\\s_")
    (let ((end (cdr (bounds-of-thing-at-point 'symbol)))
          prev)
      (lispy-dotimes arg
        (setq prev (point))
        (forward-word 1)
        (when (> (point) end)
          (goto-char prev)
          (throw 'result (1- i)))))))

(defun lispy--sub-slurp-backward (arg)
  "Grow current marked symbol by ARG backwards.
Return the amount of successful grow steps, nil instead of zero."
  (when (looking-back "\\s_")
    (let ((beg (car (bounds-of-thing-at-point 'symbol)))
          prev)
      (lispy-dotimes arg
        (setq prev (point))
        (backward-word 1)
        (when (< (point) beg)
          (goto-char prev)
          (throw 'result (1- i)))))))

(defun lispy-slurp (arg)
  "Grow current sexp by ARG sexps."
  (interactive "p")
  (if (region-active-p)
      (if (= (point) (region-end))
          (if (or (looking-at "\\s_")
                  (save-excursion
                    (goto-char (region-beginning))
                    (looking-back "\\s_")))
              (lispy--sub-slurp-forward arg)
            (lispy-dotimes arg
              (forward-sexp 1)))
        (if (or (looking-back "\\s_")
                (save-excursion
                  (goto-char (region-end))
                  (looking-at "\\s_")))
            (lispy--sub-slurp-backward arg)
          (lispy-dotimes arg
            (forward-sexp -1))))
    (if (or (looking-at "()")
            (and (looking-at lispy-left) (not (looking-back "()"))))
        (lispy-dotimes arg
          (lispy--slurp-backward))
      (if (looking-back lispy-right)
          (lispy-dotimes arg
            (lispy--slurp-forward))))
    (lispy--reindent)))

(defun lispy-down-slurp ()
  "Move current sexp or region into the next sexp."
  (interactive)
  (let ((bnd (lispy--bounds-dwim))
        (leftp (lispy--leftp))
        (regionp (region-active-p))
        deactivate-mark)
    (when (looking-at lispy-left)
      (forward-sexp))
    (let ((pt (save-excursion
                (when (lispy-forward 1)
                  (lispy-backward 1)
                  (point)))))
      (when pt
        (goto-char pt)
        (lispy--teleport (car bnd) (cdr bnd) (not leftp) regionp)))))

(defun lispy-up-slurp ()
  "Move current sexp or region into the previous sexp."
  (interactive)
  (let ((bnd (lispy--bounds-dwim))
        (regionp (region-active-p))
        (endp (or (looking-back lispy-right)
                  (and (region-active-p) (= (point) (region-end)))))
        p-beg p-end
        (pt (point))
        (deactivate-mark nil)
        bsize)
    (deactivate-mark)
    (goto-char (car bnd))
    (if (not (lispy-backward 1))
        (progn
          (lispy-complain "No list above to slurp into")
          (if regionp
              (lispy--mark bnd)
            (goto-char
             (if endp
                 (cdr bnd)
               (car bnd)))))
      (setq p-beg (point))
      (forward-list)
      (setq p-end (point))
      (goto-char (car bnd))
      (setq bsize (buffer-size))
      (lispy-save-excursion
        (goto-char (cdr bnd))
        (insert ")")
        (goto-char p-end)
        (backward-delete-char 1)
        (goto-char p-beg)
        (indent-sexp))
      (setq bnd (cons (point)
                      (+ (point)
                         (- (cdr bnd) (car bnd))
                         (- (buffer-size)
                            bsize
                            (- (point) (car bnd))
                            1))))
      (when regionp
        (lispy--mark bnd))
      (if endp
          (goto-char (cdr bnd))
        (if (region-active-p)
            (lispy-different)
          (goto-char (car bnd)))))))

(defun lispy-barf (arg)
  "Shrink current sexp by ARG sexps."
  (interactive "p")
  (cond ((region-active-p)
         (let* ((str (lispy--string-dwim))
                (one-symbolp (lispy--symbolp str))
                delta)
           (if (= (point) (region-end))
               (if one-symbolp
                   (lispy-dotimes arg
                     (if (re-search-backward "\\sw\\s_+" (region-beginning) t)
                         (forward-char 1)
                       (throw 'result i)))
                 (save-restriction
                   (narrow-to-region (region-beginning)
                                     (point-max))
                   (incf arg)
                   (lispy-dotimes arg
                     (forward-sexp -1))
                   (forward-sexp 1)
                   (widen)))
             (if one-symbolp
                 (lispy-dotimes arg
                   (if (re-search-forward "\\s_+\\sw" (region-end) t)
                       (backward-char 1)
                     (throw 'result i)))
               (save-restriction
                 (narrow-to-region (point-min)
                                   (region-end))
                 (incf arg)
                 (lispy-dotimes arg
                   (forward-sexp 1))
                 (forward-sexp -1)
                 (widen))))))

        ((looking-at "()"))

        ((looking-back lispy-right)
         (lispy-dotimes arg
           (lispy--barf-backward)))

        ((looking-at lispy-left)
         (lispy-dotimes arg
           (lispy--barf-forward)))))

(defun lispy-splice (arg)
  "Splice ARG sexps into containing list."
  (interactive "p")
  (lispy-dotimes arg
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

(defun lispy-raise (arg)
  "Use current sexp or region as replacement for its parent.
Do so ARG times."
  (interactive "p")
  (lispy-dotimes arg
    (let ((regionp (region-active-p))
          (leftp (lispy--leftp))
          (deactivate-mark nil)
          bnd1 bnd2)
      ;; re-indent first
      (lispy-save-excursion (lispy--out-forward 1))
      (unless leftp
        (lispy-different))
      (setq bnd1 (lispy--bounds-dwim))
      (deactivate-mark)
      (lispy--out-forward 1)
      (setq bnd2 (lispy--bounds-dwim))
      (delete-region (cdr bnd2) (cdr bnd1))
      (delete-region (car bnd2) (car bnd1))
      (if regionp
          (progn
            (indent-region (car bnd2) (point))
            (lispy--mark (cons (car bnd2) (point))))
        (lispy-from-left
         (indent-sexp)))
      (unless (eq leftp (lispy--leftp))
        (lispy-different)))))

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
    (lispy-raise 1)
    (deactivate-mark)))

(defun lispy-convolute (arg)
  "Replace (...(,,,|( with (,,,(...|( where ... and ,,, is arbitrary code.
When ARG is more than 1, pull ARGth expression to enclose current sexp."
  (interactive "p")
  (if (and (save-excursion
             (lispy--out-forward (1+ arg)))
           (save-excursion
             (lispy--out-backward (1+ arg))))
      (let (beg end deactivate-mark)
        (lispy-from-left
         (setq beg (point))
         (setq end (lispy--out-backward arg))
         (lispy--out-backward 1)
         (lispy--swap-regions (cons beg end)
                              (cons (point) (point)))
         (lispy--reindent arg))
        (lispy-from-left
         (lispy-different)
         (setq beg (point))
         (setq end (lispy--out-forward arg))
         (lispy-out-forward 1)
         (lispy--swap-regions (cons beg end)
                              (cons (point) (point)))
         (lispy--reindent (1+ arg))))
    (error "Not enough depth to convolute")))

(defvar lispy-repeat--command nil
  "Command to use with `lispy-repeat'.")

(defvar lispy-repeat--prefix-arg nil
  "Prefix arg to use with `lispy-repeat'.")

(defun lispy-repeat ()
  "Repeat last command with last prefix arg."
  (interactive)
  (unless (memq last-command
                '(special-lispy-repeat lispy-repeat))
    (setq lispy-repeat--command last-command)
    (setq lispy-repeat--prefix-arg
          (or last-prefix-arg 1)))
  (setq current-prefix-arg lispy-repeat--prefix-arg)
  (funcall lispy-repeat--command
           lispy-repeat--prefix-arg))

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
(defun lispy-move-up (arg)
  "Move current expression up ARG times.  Don't exit parent list."
  (interactive "p")
  (let ((at-start (lispy--leftp)))
    (unless at-start (lispy-different))
    (if (region-active-p)
        (if (= arg 1)
            (let ((pt (point))
                  (bnd0 (save-excursion
                          (deactivate-mark)
                          (if (ignore-errors (up-list) t)
                              (lispy--bounds-dwim)
                            (cons (point-min) (point-max)))))
                  (bnd1 (lispy--bounds-dwim))
                  bnd2)
              (goto-char (car bnd1))
              (if (re-search-backward "[^ \t\n`'#(]" (car bnd0) t)
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
                    (exchange-point-and-mark))
                (goto-char pt)))
          (let ((bnd1 (lispy--bounds-dwim)))
            (lispy-up arg)
            (lispy--mark
             (car
              (lispy--swap-regions
               bnd1 (lispy--bounds-dwim))))
            (exchange-point-and-mark)))
      (lispy--mark (lispy--bounds-dwim))
      (lispy-move-up arg)
      (deactivate-mark)
      (lispy-different))
    (unless at-start (lispy-different))))

(defun lispy-move-down (arg)
  "Move current expression down ARG times.  Don't exit parent list."
  (interactive "p")
  (let ((at-start (lispy--leftp)))
    (unless at-start (lispy-different))
    (if (region-active-p)
        (if (= arg 1)
            (let ((pt (point))
                  (bnd0 (save-excursion
                          (deactivate-mark)
                          (if (ignore-errors (up-list) t)
                              (lispy--bounds-dwim)
                            (cons (point-min) (point-max)))))
                  (bnd1 (lispy--bounds-dwim))
                  bnd2)
              (goto-char (cdr bnd1))
              (if (re-search-forward "[^ \t\n]" (1- (cdr bnd0)) t)
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
                    (backward-char (- (cdr bnd1) (car bnd1))))
                (goto-char pt)))
          (let ((bnd1 (lispy--bounds-dwim)))
            (lispy-down arg)
            (lispy--mark
             (cdr
              (lispy--swap-regions
               bnd1 (lispy--bounds-dwim))))
            (lispy-different)))
      (lispy--mark (lispy--bounds-dwim))
      (lispy-move-down arg)
      (deactivate-mark)
      (lispy-different))
    (unless at-start (lispy-different))))

(defun lispy-move-left (arg)
  "Move region left ARG times."
  (interactive "p")
  (lispy-dotimes arg
    (when (save-excursion (ignore-errors (up-list) t))
      (let* ((regionp (region-active-p))
             (leftp (lispy--leftp))
             (bnd (lispy--bounds-dwim))
             (str (lispy--string-dwim bnd))
             pt)
        (delete-region (car bnd) (cdr bnd))
        (cond ((looking-at " *;"))
              ((and (looking-at "\n")
                    (looking-back "^ *"))
               (delete-blank-lines))
              ((looking-at "\\([\n ]+\\)[^\n ;]")
               (delete-region (match-beginning 1)
                              (match-end 1))))
        (deactivate-mark)
        (lispy--out-backward 1)
        (setq pt (point))
        (insert str)
        (newline-and-indent)
        (skip-chars-backward " \n")
        (indent-region pt (point))
        (if regionp
            (progn
              (setq deactivate-mark)
              (set-mark pt)
              (when leftp
                (exchange-point-and-mark)))
          (when leftp
            (lispy-different)))))))

(defun lispy-move-right (arg)
  "Move region right ARG times."
  (interactive "p")
  (lispy-dotimes arg
    (when (save-excursion (ignore-errors (up-list) t))
      (let* ((regionp (region-active-p))
             (leftp (lispy--leftp))
             (bnd (lispy--bounds-dwim))
             (str (lispy--string-dwim bnd))
             pt)
        (delete-region (car bnd) (cdr bnd))
        (cond ((looking-at " *;"))
              ((and (looking-at "\n")
                    (looking-back "^ *"))
               (delete-blank-lines))
              ((looking-at "\\([\n ]+\\)[^\n ;]")
               (delete-region (match-beginning 1)
                              (match-end 1))))
        (lispy--out-backward 1)
        (lispy-different)
        (newline-and-indent)
        (setq pt (point))
        (insert str)
        (indent-region pt (point))
        (if regionp
            (progn
              (setq deactivate-mark)
              (set-mark pt)
              (when leftp
                (exchange-point-and-mark)))
          (when leftp
            (lispy-different)))))))

(defun lispy-clone (arg)
  "Clone sexp ARG times."
  (interactive "p")
  (let* ((bnd (lispy--bounds-dwim))
         (str (lispy--string-dwim bnd))
         (pt (point)))
    (cond ((region-active-p)
           (lispy-dotimes arg
             (cl-labels
                 ((doit ()
                    (let (deactivate-mark)
                      (save-excursion
                        (newline-and-indent)
                        (insert str)))))
               (if (= (point) (region-end))
                   (doit)
                 (exchange-point-and-mark)
                 (doit)
                 (exchange-point-and-mark)))))
          ((looking-at lispy-left)
           (goto-char (car bnd))
           (lispy-dotimes arg
             (insert str)
             (newline-and-indent))
           (goto-char pt))
          ((looking-back lispy-right)
           (lispy-dotimes arg
             (newline-and-indent)
             (insert str)))
          (t
           (error "Unexpected")))))

(defun lispy-oneline ()
  "Squeeze current sexp into one line.
Comments will be moved ahead of sexp."
  (interactive)
  (if (region-active-p)
      (let* ((beg (region-beginning))
             (end (region-end))
             (str (buffer-substring-no-properties
                   beg end)))
        (delete-region beg end)
        (insert (mapconcat #'identity (split-string str "[\n \t]+") " ")))
    (let ((from-left (looking-at lispy-left))
          str bnd)
      (setq str (lispy--string-dwim (setq bnd (lispy--bounds-dwim))))
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
                 (replace-regexp-in-string "\n *" " " no-comment) 1))
        (when from-left
          (backward-list)))
      (lispy--normalize-1))))

(defun lispy-multiline ()
  "Spread current sexp over multiple lines."
  (interactive)
  (lispy-from-left
   (let* ((bnd (lispy--bounds-list))
          (str (lispy--string-dwim bnd))
          (expr (lispy--read str)))
     (if (cl-some #'listp expr)
         (let ((pt (point)))
           (lispy-forward 1)
           (while (and (lispy-flow 1) (> (point) pt))
             (unless (looking-at ")\\|\n")
               (when (looking-at " *")
                 (replace-match "\n")
                 (backward-char 1))))
           (goto-char pt)
           (indent-sexp))
       (delete-region (car bnd) (cdr bnd))
       (lispy--insert
        (butlast
         (cl-mapcan (lambda (y) (list y '(ly-raw newline)))
                    (lispy--read str))))))))

(defvar lispy-do-fill nil
  "If t, `lispy-insert-1' will try to fill.")

(defun lispy-fill ()
  "Fill current expression."
  (interactive)
  (if (or (looking-at lispy-left)
          (looking-back lispy-right))
      (let ((lispy-do-fill t))
        (lispy--normalize-1))
    (fill-paragraph)))

(defun lispy-comment (&optional arg)
  "Comment ARG sexps."
  (interactive "p")
  (setq arg (or arg 1))
  (if (and (> arg 1) (lispy--in-comment-p))
      (let ((bnd (lispy--bounds-comment)))
        (uncomment-region (car bnd) (cdr bnd)))
    (lispy-dotimes arg
      (let (bnd)
        (cond ((region-active-p)
               (comment-dwim nil)
               (when (lispy--in-string-or-comment-p)
                 (lispy--out-backward 1)))
              ((lispy--in-string-or-comment-p)
               (self-insert-command 1))
              ((looking-at lispy-left)
               (setq bnd (lispy--bounds-dwim))
               (lispy-down 1)
               (comment-region (car bnd) (cdr bnd))
               (when (lispy--in-string-or-comment-p)
                 (lispy--out-backward 1)))
              ((bolp)
               (insert ";;"))
              ((looking-back lispy-right)
               (comment-dwim nil)
               (insert " "))
              ((eolp)
               (comment-dwim nil))
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
           (unless (lispy-dotimes arg
                     (forward-list 1))
             (error "Unexpected")))
          ((looking-back lispy-right)
           (setq endp t)
           (unless (lispy-dotimes arg
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
        (lispy--teleport ,beg ,end ,endp ,regionp)))))

(defun lispy-replace-symbol ()
  "Overwrite `lispy-ace-symbol' with current thing."
  (interactive)
  (lexical-let ((bnd-defun (save-excursion
                             (lispy-beginning-of-defun)
                             (lispy--bounds-dwim)))
                (bnd-curr (lispy--bounds-dwim)))
    (lispy--ace-do
     "[([{ ]\\(?:\\sw\\|\\s_\\|\\s(\\|[\"'`#]\\)"
     bnd-defun
     (lambda () (or (not (lispy--in-string-or-comment-p)) (looking-back ".\"")))
     (lambda ()
       (let ((str (lispy--string-dwim bnd-curr)))
         (let ((pt (1+ (point))))
           (goto-char (car bnd-curr))
           (save-excursion
             (goto-char pt)
             (lispy-mark-symbol)
             (delete-active-region)
             (deactivate-mark)
             (insert str)
             (lispy--normalize-1))))))))

;; ——— Locals:  tags ———————————————————————————————————————————————————————————
(defun lispy-goto (&optional arg)
  "Jump to symbol within files in current directory.
When ARG isn't nil, call `lispy-goto-projectile' instead."
  (interactive "P")
  (deactivate-mark)
  (lispy--goto
   (if arg
       #'lispy--fetch-tags-projectile
     #'lispy--fetch-tags)))

(defun lispy-goto-recursive ()
  "Jump to symbol within files in current directory and its subdiretories."
  (interactive)
  (deactivate-mark)
  (lispy--goto 'lispy--fetch-tags-recursive))

(defun lispy-goto-local ()
  "Jump to symbol within current file."
  (interactive)
  (deactivate-mark)
  (lispy--goto 'lispy--fetch-this-file-tags))

(defun lispy-goto-projectile ()
  "Jump to symbol within files in (`projectile-project-root')."
  (interactive)
  (deactivate-mark)
  (lispy--goto 'lispy--fetch-tags-projectile))

(defun lispy-goto-def-down (arg)
  "Jump to definition of ARGth element of current list."
  (interactive "p")
  (let* ((expr (read (lispy--string-dwim)))
         (n (length expr)))
    (if (>= arg n)
        (error "Out of range: %s/%s" arg n)
      (let ((elt (nth arg expr)))
        (while (consp elt)
          (if (eq (car elt) 'quote)
              (setq elt (cadr elt))
            (setq elt (car elt))))
        (if elt
            (lispy-goto-symbol elt)
          (error "No symbol found"))))))

(defun lispy-goto-def-ace (arg)
  "Jump to definition of selected element of current sexp.
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
   (lambda () (forward-char 1) (call-interactively 'lispy-goto-symbol))))

(declare-function cider--jump-to-loc-from-info "ext:cider")
(declare-function cider-var-info "ext:cider")
(declare-function slime-edit-definition "ext:slime")
(declare-function lispy--clojure-resolve "ext:lispy")
(declare-function View-quit "view")
(declare-function org-overview "org")

(defun lispy-goto-symbol (symbol)
  "Go to definition of SYMBOL."
  (interactive (list (let ((str (thing-at-point 'symbol)))
                       (if str
                           (intern str)
                         (lispy--current-function)))))
  (let (rsymbol)
    (deactivate-mark)
    (ring-insert find-tag-marker-ring (point-marker))
    (cond ((and (memq major-mode '(emacs-lisp-mode lisp-interaction-mode))
                (setq symbol (intern-soft symbol)))
           (cond ((fboundp symbol)
                  (find-function symbol))
                 ((boundp symbol)
                  (find-variable symbol))
                 ((or (featurep symbol)
                      (locate-library
                       (prin1-to-string symbol)))
                  (find-library (prin1-to-string symbol)))
                 ((setq rsymbol
                        (cl-find-if
                         `(lambda (x)
                            (equal (car x)
                                   ,(symbol-name symbol)))
                         (lispy--fetch-this-file-tags)))
                  (goto-char (aref (nth 4 rsymbol) 0)))
                 (t
                  (error "Couldn't fild definition of %s"
                         symbol))))
          ((eq major-mode 'clojure-mode)
           (require 'le-clojure)
           (setq rsymbol (lispy--clojure-resolve symbol))
           (cond ((stringp rsymbol)
                  (cider--jump-to-loc-from-info
                   (cider-var-info rsymbol)))
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
           (slime-edit-definition symbol)))))

;; ——— Locals:  dialect-related ————————————————————————————————————————————————
(defun lispy-eval ()
  "Eval last sexp."
  (interactive)
  (save-excursion
    (unless (or (looking-back lispy-right) (region-active-p))
      (lispy-forward 1))
    (message
     (replace-regexp-in-string "%" "%%" (lispy--eval (lispy--string-dwim))))))

(defvar lispy-do-pprint nil
  "Try a pretty-print when this ins't nil.")

(defun lispy-eval-and-insert (&optional arg)
  "Eval last sexp and insert the result.

When ARG isn't nil, try to pretty print the sexp."
  (interactive "P")
  (let ((lispy-do-pprint arg))
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
        (doit)))))

(defun lispy-eval-and-replace ()
  "Eval last sexp and replace it with the result."
  (interactive)
  (let* ((leftp (lispy--leftp))
         (bnd (lispy--bounds-dwim))
         (str (lispy--string-dwim bnd)))
    (delete-region (car bnd) (cdr bnd))
    (insert (lispy--eval str))
    (unless (or (looking-at lispy-left)
                (looking-back lispy-right))
      (lispy--out-backward 1))
    (when (and leftp (looking-back lispy-right))
      (lispy-different))))

(defconst lispy--eval-cond-msg
  (format "%s: nil" (propertize "cond" 'face 'font-lock-keyword-face))
  "Message to echo when the current `cond' branch is nil.")

(defvar lispy-eval--active-window nil
  "Source window for `lispy-eval-other-window'.")
(defvar lispy-eval--eval-window nil
  "Target window for `lispy-eval-other-window'.")
(defvar lispy-eval--eval-buffer nil
  "Target buffer for `lispy-eval-other-window'")
(defvar lispy-eval--expr nil
  "The expression for`lispy-eval-other-window'.")

(defun lispy--eval-in-window (aj-data)
  "Eval in the context of AJ-DATA."
  (let ((frame (aj-position-frame aj-data))
        (window (aj-position-window aj-data)))
    (when (and (frame-live-p frame)
               (not (eq frame (selected-frame))))
      (select-frame-set-input-focus frame))
    (select-window window)
    (setq lispy-eval--eval-window window)
    (setq lispy-eval--eval-buffer (current-buffer))
    (let ((res (condition-case e
                   (eval lispy-eval--expr lexical-binding)
                 (error
                  (message "error: %s" (error-message-string e))))))
      (if (equal res lispy--eval-cond-msg)
          (message res)
        (message "%S" res))
      (select-window lispy-eval--active-window))))

(defun lispy-eval-other-window ()
  "Eval current expression in the context of other window.
In case the point is on a let-bound variable, add a `setq'."
  (interactive)
  (lexical-let*
      ((lispy-ignore-whitespace t)
       (str (save-match-data
              (lispy--string-dwim)))
       (expr (save-match-data
               (lispy-from-left
                (cond
                  ((looking-back "(\\(?:lexical-\\)?let\\*?[ \t\n]*")
                   (cons 'setq
                         (cl-mapcan
                          (lambda (x) (unless (listp x) (list x nil)))
                          (read str))))

                  ;; point moves
                  ((progn
                     (lispy--out-backward 1)
                     (looking-back
                      "(\\(?:lexical-\\)?let\\*?[ \t\n]*"))
                   (cons 'setq (read str)))

                  ((looking-back "(\\(?:cl-\\)?labels[ \t\n]*")
                   (cons 'defun (read str)))

                  ((looking-at
                    "(cond\\b")
                   (let ((re (read str)))
                     `(if ,(car re)
                          (progn
                            ,@(cdr re))
                        lispy--eval-cond-msg)))
                  (t (read str))))))
       res)
    (other-window 1)
    (setq res (eval expr lexical-binding))
    (other-window -1)
    (if (equal res lispy--eval-cond-msg)
        (message res)
      (message "%S" res))))

(defun lispy-follow ()
  "Follow to `lispy--current-function'."
  (interactive)
  (lispy-goto-symbol (lispy--current-function)))

(defun lispy-describe ()
  "Display documentation for `lispy--current-function'."
  (interactive)
  (let ((symbol (intern-soft (lispy--current-function))))
    (cond ((fboundp symbol)
           (describe-function symbol))
          ((boundp symbol)
           (describe-variable symbol)))))

(defun lispy-arglist ()
  "Display arglist for `lispy--current-function'."
  (interactive)
  (let ((sym (intern-soft (lispy--current-function))))
    (cond ((fboundp sym)
           (let ((args (car (help-split-fundoc (documentation sym t) sym))))
             (message "%s" args))))))

(defvar lispy-bof-last-point 1)

(defun lispy-beginning-of-defun (&optional arg)
  "Forward to `beginning-of-defun' with ARG.  Deactivate region.
When called twice in a row, restore point and mark."
  (interactive "p")
  (if (and (looking-at "(def")
           (memq last-command
                 '(lispy-beginning-of-defun
                   special-lispy-beginning-of-defun)))
      (if (consp lispy-bof-last-point)
          (progn
            (goto-char (car lispy-bof-last-point))
            (set-mark (cdr lispy-bof-last-point)))
        (goto-char lispy-bof-last-point))
    (if (region-active-p)
        (progn
          (setq lispy-bof-last-point
                (cons (region-beginning)
                      (region-end)))
          (deactivate-mark))
      (setq lispy-bof-last-point (point)))
    (beginning-of-defun arg)))

;; ——— Locals:  ace-jump-mode  —————————————————————————————————————————————————
(declare-function fancy-narrow-to-region "ext:fancy-narrow")
(declare-function fancy-widen "ext:fancy-narrow")

(defun lispy-ace-char ()
  "Call `ace-jump-char-mode' on current defun."
  (interactive)
  (let ((bnd (save-excursion
               ;; `beginning-of-defun' won't work, since it can change sexp
               (lispy--out-backward 50)
               (lispy--bounds-dwim)))
        (fancy (and lispy-fancy-narrow (package-installed-p 'fancy-narrow)))
        (ace-jump-mode-scope 'window))
    (save-restriction
      (if fancy
          (fancy-narrow-to-region (car bnd) (cdr bnd))
        (narrow-to-region (car bnd) (cdr bnd)))
      (call-interactively 'ace-jump-char-mode)
      (if fancy
          (fancy-widen)
        (widen)))))

(defun lispy-ace-paren ()
  "Use `lispy--ace-do' to jump to `lispy-left' within current defun.
When FUNC is not nil, call it after a successful move.
When NO-NARROW is not nil, don't narrow."
  (interactive)
  (deactivate-mark)
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
  (if (and (region-active-p)
           (string-match "\\`\\(\\sw+\\)\\s_"
                         (lispy--string-dwim)))
      (lispy--mark (cons (region-beginning)
                         (+ (region-beginning) (match-end 1))))
    (lispy--out-forward
     (if (region-active-p)
         (progn (deactivate-mark) arg)
       (1- arg)))
    (lispy--ace-do
     "[([{ -]\\(?:\\sw\\|\\s_\\|\\s(\\|[\"'`#]\\)"
     (lispy--bounds-dwim)
     (lambda () (or (not (lispy--in-string-or-comment-p)) (looking-back ".\"")))
     (lambda () (skip-chars-forward "-([{ `'#") (mark-word)))))

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
(defun lispy-outline-next (arg)
  "Call `outline-next-visible-heading' ARG times."
  (interactive "p")
  (lispy-dotimes arg
    (let ((pt (point)))
      (outline-next-visible-heading 1)
      (unless (looking-at outline-regexp)
        (goto-char pt)
        (error "Past last outline")))))

(defun lispy-outline-prev (arg)
  "Call `outline-previous-visible-heading' ARG times."
  (interactive "p")
  (lispy-dotimes arg
    (let ((pt (point)))
      (outline-previous-visible-heading 1)
      (unless (looking-at outline-regexp)
        (goto-char pt)
        (error "Past first outline")))))

(defun lispy-tab ()
  "Indent code and hide/show outlines.
When region is active, call `lispy-mark-car'."
  (interactive)
  (if (region-active-p)
      (lispy-mark-car)
    (if (looking-at ";")
        (progn
          (outline-minor-mode 1)
          (condition-case e
              (outline-toggle-children)
            (error
             (if (string= (error-message-string e) "before first heading")
                 (outline-next-visible-heading 1)
               (signal (car e) (cdr e))))))
      (lispy--normalize-1))))

(defun lispy-shifttab ()
  "Hide/show outline summary."
  (interactive)
  (require 'org)
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
  "Turn the current lambda or toplevel sexp into a defun."
  (interactive)
  (let (bnd expr)
    (if (and (lispy-from-left (bolp))
             (progn
               (setq expr
                     (lispy--read
                      (lispy--string-dwim
                       (setq bnd (lispy--bounds-dwim)))))
               (cl-every #'symbolp expr)))
        (progn
          (delete-region (car bnd)
                         (cdr bnd))
          (lispy--insert
           `(defun ,(car expr) ,(or (cdr expr) '(ly-raw empty))
              (ly-raw newline)))
          (backward-char))
      (let ((pt (point)))
        (when (region-active-p)
          (deactivate-mark))
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
              (message "defun stored to kill ring")
              (lispy-backward 1))
          (lispy-complain "Not in lambda")
          (goto-char pt))))))

(defun lispy-flatten (arg)
  "Inline a function at the point of its call.
The function body is obtained from `find-function-noselect'.
With ARG, use the contents of `lispy-store-region-and-buffer' instead."
  (interactive "P")
  (let* ((begp (if (looking-at lispy-left)
                   t
                 (if (looking-back lispy-right)
                     (progn (backward-list)
                            nil)
                   (lispy-out-backward 1))))
         (bnd (lispy--bounds-list))
         (str (lispy--string-dwim bnd))
         (expr (lispy--read str))
         (fstr (if arg
                   (with-current-buffer (get 'lispy-store-bounds 'buffer)
                     (lispy--string-dwim (get 'lispy-store-bounds 'region)))
                 (condition-case e
                     (lispy--function-str (car expr))
                   (unsupported-mode-error
                    (lispy-complain
                     (format "Can't flatten: symbol `%s' is defined in `%s'"
                             (lispy--prin1-fancy (car expr))
                             (lispy--prin1-fancy (cdr e))))
                    nil)))))
    (when fstr
      (let* ((e-args (cl-remove-if #'lispy--whitespacep (cdr expr)))
             (body (if (macrop (car expr))
                       (macroexpand (read str))
                     (lispy--flatten-function fstr e-args))))
        (goto-char (car bnd))
        (delete-region (car bnd) (cdr bnd))
        (lispy--insert body)
        (when begp
          (goto-char (car bnd)))))))

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

(defun lispy-unbind-variable ()
  "Subsititute let-bound variable."
  (interactive)
  (forward-char 1)
  (noflet ((message (&rest)))
    (iedit-mode 0))
  (lispy-mark-symbol)
  (lispy-move-down 1)
  (iedit-mode)
  (deactivate-mark)
  (lispy-out-backward 1)
  (lispy-delete 1)
  (save-excursion
    (lispy--out-backward 1)
    (lispy--normalize-1)))

(defvar lispy-bind-var-in-progress nil
  "When t, `lispy-mark-symbol' will exit `iedit'.")
(make-variable-buffer-local 'lispy-bind-var-in-progress)

(defun lispy-bind-variable ()
  "Bind current expression as variable."
  (interactive)
  (deactivate-mark)
  (let* ((bnd (lispy--bounds-dwim))
         (str (lispy--string-dwim bnd)))
    (setq lispy-bind-var-in-progress t)
    (delete-region (car bnd)
                   (cdr bnd))
    (insert (format "(let ((foobar %s)))" str))
    (backward-char 1)
    (newline-and-indent)
    (insert "foobar")
    (iedit-mode 1)
    (backward-delete-char 6)))

;; ——— Locals:  multiple cursors ———————————————————————————————————————————————
(declare-function mc/create-fake-cursor-at-point "ext:multiple-cursors")
(declare-function mc/all-fake-cursors "ext:multiple-cursors")
(declare-function mc/maybe-multiple-cursors-mode "ext:multiple-cursors")
(declare-function mc/mark-lines "ext:multiple-cursors")
(declare-function mc/remove-fake-cursors "ext:multiple-cursors")

(defun lispy-cursor-down (arg)
  "Add ARG cursors using `lispy-down'."
  (interactive "p")
  (require 'multiple-cursors)
  (if (and (mc/all-fake-cursors)
           (not (eq last-command
                    'lispy-cursor-down)))
      (progn
        (deactivate-mark)
        (mc/remove-fake-cursors))
    (if (looking-at lispy-left)
        (lispy-dotimes arg
          (mc/create-fake-cursor-at-point)
          (loop do (lispy-down 1)
             while (mc/all-fake-cursors (point) (1+ (point)))))
      (mc/mark-lines arg 'forwards))
    (mc/maybe-multiple-cursors-mode)))

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

(defun lispy--vertical-splitp ()
  "Return nil if the frame isn't two vertical windows.
In case it is, return the left window."
  (let ((windows (window-list)))
    (when (= (length windows) 2)
      (let ((wnd1 (car windows))
            (wnd2 (cadr windows)))
        (when (= (window-pixel-top wnd1)
                 (window-pixel-top wnd2))
          (if (< (window-pixel-left wnd1)
                 (window-pixel-left wnd2))
              wnd1
            wnd2))))))

(defun lispy-ediff-regions ()
  "Comparable to `ediff-regions-linewise'.
First region and buffer come from `lispy-store-region-and-buffer'
Second region and buffer are the current ones."
  (interactive)
  (let ((wnd (current-window-configuration))
        (e1 (lispy--make-ediff-buffer
             (current-buffer) "-A-"
             (lispy--bounds-dwim)))
        (e2 (lispy--make-ediff-buffer
             (get 'lispy-store-bounds 'buffer) "-B-"
             (get 'lispy-store-bounds 'region))))
    (apply #'ediff-regions-internal
           `(,@(if (equal (selected-window)
                          (lispy--vertical-splitp))
                   (append e1 e2)
                   (append e2 e1))
               nil ediff-regions-linewise nil nil))
    (add-hook 'ediff-after-quit-hook-internal
              `(lambda ()
                 (setq ediff-after-quit-hook-internal)
                 (set-window-configuration ,wnd)))))

;; ——— Locals:  marking ————————————————————————————————————————————————————————
(defun lispy-mark-right (arg)
  "Go right ARG times and mark."
  (interactive "p")
  (deactivate-mark)
  (let* ((pt (point))
         (lispy-ignore-whitespace t)
         (r (lispy--out-forward arg)))
    (if (or (= pt (point))
            (and (region-active-p)
                 (= (region-beginning)
                    (region-end))))
        (progn
          (goto-char pt)
          (lispy-complain "can't go any further"))
      (lispy--mark
       (lispy--bounds-dwim))
      r)))

(defun lispy-mark-left (arg)
  "Go left ARG times and mark."
  (interactive "p")
  (when (lispy-mark-right arg)
    (lispy-different)))

(defun lispy-mark-car ()
  "Mark the car of current thing."
  (interactive)
  (let ((bnd-1 (lispy--bounds-dwim))
        bnd-2)
    (if (and (eq (char-after (car bnd-1)) ?\")
             (eq (char-before (cdr bnd-1)) ?\"))
        (lispy--mark (cons (1+ (car bnd-1))
                           (1- (cdr bnd-1))))
      (goto-char (car bnd-1))
      (while (and (equal bnd-1 (setq bnd-2 (bounds-of-thing-at-point 'sexp)))
                  (< (point) (cdr bnd-1)))
        (forward-char)
        (skip-chars-forward " "))
      (if bnd-2
          (lispy--mark bnd-2)
        (lispy-complain "can't descend further")))))

;; ——— Locals:  edebug —————————————————————————————————————————————————————————
(defun lispy-edebug-stop ()
  "Stop edebugging, while saving current function arguments."
  (interactive)
  (if (bound-and-true-p edebug-active)
      (save-excursion
        (lispy-out-backward 99)
        (if (looking-at
             "(\\(?:cl\\|\\)def\\(?:un\\|macro\\)\\s-+\\_<[^ ]+\\_>\\s-+(")
            (progn
              (goto-char (match-end 0))
              (backward-char 1)
              (forward-sexp 1)
              (let ((sexps
                     (mapcar
                      (lambda (x!)
                        (cons x!
                              (let ((expr x!))
                                (edebug-eval expr))))
                      (delq '&optional (delq '&rest (lispy--preceding-sexp)))))
                    (wnd (current-window-configuration))
                    (pt (point)))
                (run-with-timer
                 0 nil
                 `(lambda ()
                    (mapc (lambda (x!) (set (car x!) (cdr x!))) ',sexps)
                    (set-window-configuration ,wnd)
                    (goto-char ,pt)))
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
                          (error "Argument = %s isn't supported" arg)))))
             (error "%s isn't bound" fun))))))

(defun lispy-debug-step-in ()
  "Eval current function arguments and jump to definition."
  (interactive)
  (let* ((ldsi-sxp (lispy--setq-expression))
         (ldsi-fun (car ldsi-sxp)))
    (if (functionp ldsi-fun)
        (let ((ldsi-args
               (copy-seq
                (help-function-arglist
                 (if (ad-is-advised ldsi-fun)
                     (ad-get-orig-definition ldsi-fun)
                   ldsi-fun)
                 t)))
              (ldsi-vals (cdr ldsi-sxp))
              ldsi-arg)
          (catch 'done
            (while (setq ldsi-arg (pop ldsi-args))
              (cond ((eq ldsi-arg '&optional)
                     (setq ldsi-arg (pop ldsi-args))
                     (set ldsi-arg (eval (pop ldsi-vals))))
                    ((eq ldsi-arg '&rest)
                     (setq ldsi-arg (pop ldsi-args))
                     (set ldsi-arg (mapcar #'eval ldsi-vals))
                     (throw 'done t))
                    (t
                     (set ldsi-arg (eval (pop ldsi-vals)))))))
          (lispy-goto-symbol ldsi-fun))
      (lispy-complain
       (format "%S isn't a function" ldsi-fun)))))

;; ——— Locals:  miscellanea ————————————————————————————————————————————————————
(defvar lispy-mode-x-map (make-sparse-keymap))

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
  (lispy-from-left
   (let ((window-line (count-lines (window-start) (point))))
     (if (or (= window-line 0)
             (and (not (bolp)) (= window-line 1)))
         (recenter (or (get 'lispy-recenter :line) 0))
       (put 'lispy-recenter :line window-line)
       (recenter 0)))))

(when (version< emacs-version "24.4")
  (defun macrop (object)
    "Non-nil if and only if OBJECT is a macro."
    (let ((def (indirect-function object t)))
      (when (consp def)
        (or (eq 'macro (car def))
            (and (autoloadp def) (memq (nth 4 def) '(macro t))))))))

(defalias 'lispy--preceding-sexp
    (if (version< emacs-version "25.1")
        'preceding-sexp
      'elisp--preceding-sexp))

(declare-function projectile-find-file "ext:projectile")
(declare-function projectile-find-file-other-window "ext:projectile")
(declare-function projectile-project-root "ext:projectile")

(defun lispy-visit (arg)
  "Forward to find file in project depending on ARG."
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

(defun lispy-other-space ()
  "Alternative to `lispy-space'."
  (interactive)
  (cond ((looking-back lispy-right)
         (backward-char 1)
         (insert " "))
        ((looking-at lispy-left)
         (insert " ")
         (backward-char 1))))

(defun lispy-paste ()
  "Forward to `yank'.
If the region is active, replace instead of yanking."
  (interactive)
  (if (region-active-p)
      (let ((bnd (lispy--bounds-dwim)))
        (deactivate-mark)
        (delete-region (car bnd)
                       (cdr bnd))
        (yank))
    (yank)
    (when (and (looking-back lispy-right)
               (looking-at lispy-left))
      (insert " "))))

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
             (comment-only-p beg (point)))
         beg)))

(defun lispy--buffer-narrowed-p ()
  "Return T if the current buffer is narrowed."
  (or (/= (point-min) 1)
      (/= (point-max) (1+ (buffer-size)))))

(defun lispy--raw-comment-p (expr)
  "Return t if EXPR is a raw comment."
  (and (listp expr)
       (eq (car expr) 'ly-raw)
       (consp (cdr expr))
       (eq (cadr expr) 'comment)))

(defun lispy--raw-string-p (expr)
  "Return t if EXPR is a raw comment."
  (and (listp expr)
       (eq (car expr) 'ly-raw)
       (consp (cdr expr))
       (eq (cadr expr) 'string)))

(defun lispy--leftp ()
  "Return t if at region beginning, or at start of the list."
  (if (region-active-p)
      (= (point) (region-beginning))
    (looking-at lispy-left)))

(defun lispy--symbolp (str)
  "Return t if STR is a symbol."
  (string-match "\\`\\(?:\\sw\\|\\s_\\)+\\'" str))

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
        ((or (looking-at (format "[`'#]*%s" lispy-left))
             (looking-at "[`'#]"))
         (bounds-of-thing-at-point 'sexp))
        ((looking-at "\"")
         (lispy--bounds-string))
        ((looking-back "\"")
         (backward-sexp)
         (prog1 (bounds-of-thing-at-point 'sexp)
           (forward-sexp)))
        (t
         (or
          (ignore-errors
            (bounds-of-thing-at-point 'symbol))
          (ignore-errors
            (bounds-of-thing-at-point 'sentence))
          (ignore-errors
            (backward-word 1)
            (bounds-of-thing-at-point 'symbol))
          (ignore-errors
            (forward-word 1)
            (bounds-of-thing-at-point 'symbol))))))

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

(defun lispy--prin1-fancy (x)
  "Return a propertized `prin1-to-string'-ed X."
  (propertize (prin1-to-string x)
              'face 'font-lock-constant-face))

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
          (if buffer-read-only
              (deactivate-mark)
            (unless lispy-ignore-whitespace
              (lispy--remove-gaps)
              (lispy--indent-for-tab)))
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
    (if (= pt (point))
        nil
      (point))))

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
         ((memq major-mode '(clojure-mode nrepl-repl-mode))
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
  (let ((e-sexp (read e-str))
        val)
    (when (and (consp e-sexp)
               (memq (car e-sexp) '(defvar defcustom))
               (consp (cdr e-sexp))
               (boundp (cadr e-sexp)))
      (set (cadr e-sexp) (eval (caddr e-sexp))))
    (condition-case e
        (prin1-to-string
         (eval e-sexp lexical-binding))
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
     (if . 1)
     (when . 1)
     (unless . 1)
     (advice-add . 1)
     (cl-defun . 1)
     ;; org-mode specific
     (org-defkey . 3)
     ;; use-package specific
     (use-package . 1)
     ;; lispy-specific
     (lispy-defverb . 1)))
  "Alist of tag arities for supported modes.")

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
                 (:command 'lispy-command-name-face)
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
              (condition-case nil
                  (forward-sexp arity)
                (error
                 (forward-sexp 1)))
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

(defun lispy--tag-sexp-elisp (x)
  "Get the actual sexp from semantic tag X."
  (let ((ov (nth 4 x))
        buf end)
    (if (overlayp ov)
        (setq buf (overlay-buffer ov)
              end (overlay-end ov))
      (if (vectorp ov)
          (setq buf (find-file-noselect (aref ov 2))
                end (aref ov 1))
        (error "Unexpected")))
    (with-current-buffer buf
      (save-excursion
        (goto-char end)
        (ignore-errors
          (elisp--preceding-sexp))))))

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
         (if (semantic-tag-get-attribute x :user-visible-flag)
             (lispy--propertize-tag nil x :command)
           (lispy--propertize-tag nil x :function)))
        ((eq (cadr x) 'variable)
         (lispy--propertize-tag "defvar" x))
        ((assq (cadr x) (cdr (assoc 'emacs-lisp-mode lispy-tag-arity)))
         (lispy--propertize-tag (symbol-name (cadr x)) x))
        ((and (eq (cadr x) 'code)
              (string= (car x) "define-derived-mode"))
         (let ((sexp (lispy--tag-sexp-elisp x)))
           (lispy--propertize-tag
            "define-derived-mode"
            (list (format "%s %s"
                          (cadr sexp)
                          (caddr sexp))))))
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
  "Given a semantic tag X, return its string representation.
This is `semantic-tag-name', amended with extra info.
For example, a `setq' statement is amended with variable name that it uses."
  (cond ((memq major-mode '(emacs-lisp-mode lisp-interaction-mode))
         (lispy--tag-name-elisp x))
        ((eq major-mode 'clojure-mode)
         (lispy--tag-name-clojure x))
        ((eq major-mode 'scheme-mode)
         ;; (lispy--tag-name-scheme x)
         (car x))
        ((eq major-mode 'lisp-mode)
         (lispy--tag-name-lisp x))
        (t (throw 'break nil))))

(defun lispy--tag-name-and-file (x)
  "Add file name to (`lispy--tag-name' X)."
  (or
   (catch 'break
     (cons
      (concat
       (lispy--pad-string
        (lispy--tag-name x)
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
  (unless path
    (lispy--fetch-this-file-tags))
  (setq path (or path (expand-file-name default-directory)))
  (unless (string-match "/$" path)
    (setq path (concat path "/")))
  (let* ((this-file (buffer-file-name))
         (default-directory path)
         (db (or (semanticdb-directory-loaded-p path)
                 ;; a trick to make sure semantic loads
                 (let ((files (lispy--file-list)))
                   (when files
                     (with-current-buffer
                         (find-file-noselect
                          (expand-file-name (car files)))
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
               (aref x 5))))
           db)))))

(defun lispy--fetch-tags-recursive ()
  "Fetch all tags in current directory recursively."
  (let ((dirs
         (split-string
          (shell-command-to-string
           (format "find %s -type d ! -regex \".*\\(\\.git\\|\\.cask\\).*\"" default-directory))
          "\n"
          t)))
    (apply #'append
           (mapcar #'lispy--fetch-tags dirs))))

(defun lispy--fetch-tags-projectile ()
  "Fetch all tags in the projectile directory recursively."
  (require 'projectile)
  (let ((default-directory (projectile-project-root)))
    (lispy--fetch-tags-recursive)))

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
          (buffer-file-name)
          (semantic-fetch-tags))))
    (when (memq major-mode '(lisp-mode emacs-lisp-mode))
      (lexical-let ((arity (cdr (assoc major-mode lispy-tag-arity)))
                    (tag-regex (lispy--tag-regexp)))
        (mapc (lambda (x) (lispy--modify-tag x tag-regex arity)) tags)))
    tags))

(defvar lispy--goto-cache nil "Maps directories to pretty tags.")

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
              (setq cached-cands (mapcar #'lispy--tag-name-and-file candidates))
              (when (> (length cached-cands) 1000)
                (push (cons default-directory cached-cands)
                      lispy--goto-cache))
              cached-cands)
             ((and (setq cached-cands (cdr cache))
                   (= (length cached-cands)
                      (length candidates)))
              cached-cands)
             (t
              (setcdr cache (mapcar #'lispy--tag-name-and-file candidates))))
       #'lispy--action-jump))
    (when (and lispy-no-permanent-semantic
               (not semantic-on))
      (semantic-mode -1))))

;; ——— Utilities: slurping and barfing —————————————————————————————————————————
(defun lispy--slurp-forward ()
  "Grow current sexp forward by one sexp."
  (unless (or (looking-back "[()])")
              (looking-at "$"))
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
      (lispy--indent-region (point) pt))))

(defun lispy--replace-regexp-in-code (regexp to-string)
  "Replace text matching REGEXP with TO-STRING in whole buffer.
Ignore the matches in strings and comments."
  (goto-char (point-min))
  (while (re-search-forward regexp nil t)
    (unless (lispy--in-string-or-comment-p)
      (replace-match to-string))))

;; ——— Utilities: source transformation ————————————————————————————————————————
(defun lispy--read (str)
  "Read STR including comments and newlines."
  (let* ((mode major-mode)
         cbnd
         (str (with-temp-buffer
                (funcall mode)
                (insert str)
                ;; ——— ly-raw —————————————————
                (lispy--replace-regexp-in-code "(ly-raw" "(ly-raw raw")
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
                  (progn
                    (setq cbnd (lispy--bounds-string))
                    (when cbnd
                      (if (looking-back "ly-raw comment \"")
                          (goto-char (cdr cbnd))
                        (setq str (lispy--string-dwim cbnd))
                        (delete-region (car cbnd) (cdr cbnd))
                        (insert (format "(ly-raw string %S)" str))))))
                ;; ——— newlines ———————————————
                (lispy--replace-regexp-in-code "\n" " (ly-raw newline)")
                ;; ——— () —————————————————————
                (lispy--replace-regexp-in-code "()" "(ly-raw empty)")
                ;; ——— ? char syntax ——————————
                (goto-char (point-min))
                (while (re-search-forward "\\(?:\\s-\\|\\s(\\)\\?" nil t)
                  (unless (lispy--in-string-or-comment-p)
                    (let ((pt (point))
                          sexp)
                      (forward-sexp)
                      (setq sexp (buffer-substring-no-properties pt (point)))
                      (delete-region (1- pt) (point))
                      (insert (format "(ly-raw char %S)" sexp)))))
                ;; ——— #' —————————————————————
                (goto-char (point-min))
                (while (re-search-forward "#'" nil t)
                  (unless (lispy--in-string-or-comment-p)
                    (let ((beg (point)))
                      (forward-sexp)
                      (insert ")")
                      (replace-match "(ly-raw function "))))
                ;; ——— #( —————————————————————
                (goto-char (point-min))
                (while (re-search-forward "#(" nil t)
                  (unless (lispy--in-string-or-comment-p)
                    (backward-char 1)
                    (backward-delete-char 1)
                    (forward-char 1)
                    (insert "ly-raw clojure-lambda ")))
                ;; ——— #{ —————————————————————
                (goto-char (point-min))
                (while (re-search-forward "#{" nil t)
                  (unless (lispy--in-string-or-comment-p)
                    (backward-char 1)
                    (save-excursion
                      (forward-list 1)
                      (backward-delete-char 1)
                      (insert ")"))
                    (backward-delete-char 1)
                    (delete-char 1)
                    (insert "(ly-raw clojure-set ")))
                ;; ——— { ——————————————————————
                (goto-char (point-min))
                (while (re-search-forward "{" nil t)
                  (unless (lispy--in-string-or-comment-p)
                    (backward-char 1)
                    (save-excursion
                      (forward-list 1)
                      (backward-delete-char 1)
                      (insert ")"))
                    (delete-char 1)
                    (insert "(ly-raw clojure-map ")))
                ;; ——— ' ——————————————————————
                (goto-char (point-min))
                (while (re-search-forward "'" nil t)
                  (unless (lispy--in-string-or-comment-p)
                    (backward-char 1)
                    (let ((beg (point))
                          (sxp (ignore-errors (read (current-buffer)))))
                      (when (and (consp sxp)
                                 (eq (car sxp) 'quote))
                        (insert ")")
                        (goto-char beg)
                        (delete-char 1)
                        (insert "(ly-raw quote ")))))
                ;; ——— ` ——————————————————————
                (goto-char (point-min))
                (while (re-search-forward "[^\\]`" nil t)
                  (unless (lispy--in-string-or-comment-p)
                    (backward-char 1)
                    (let ((beg (point))
                          (sxp (ignore-errors (read (current-buffer)))))
                      (when (and (consp sxp)
                                 (eq (car sxp) '\`))
                        (insert ")")
                        (goto-char beg)
                        (delete-char 1)
                        (insert "(ly-raw \\` ")))))
                ;; ——— , ——————————————————————
                (goto-char (point-min))
                (while (re-search-forward "[^\\],[^@\"]" nil t)
                  (unless (lispy--in-string-or-comment-p)
                    (backward-char 2)
                    (let ((beg (point))
                          (sxp (ignore-errors (read (current-buffer)))))
                      (when (and (consp sxp)
                                 (eq (car sxp) '\,))
                        (insert ")")
                        (goto-char beg)
                        (delete-char 1)
                        (insert "(ly-raw \\, ")))))
                ;; ——— ,@ —————————————————————
                (goto-char (point-min))
                (while (re-search-forward "\\(?:[^\\]\\|^\\),@" nil t)
                  (unless (lispy--in-string-or-comment-p)
                    (backward-char 2)
                    (let ((beg (point))
                          (sxp (ignore-errors (read (current-buffer)))))
                      (when (and (consp sxp)
                                 (eq (car sxp) '\,@))
                        (insert ")")
                        (goto-char beg)
                        (delete-char 2)
                        (insert "(ly-raw \\,@ ")))))
                ;; ——— overlay syntax —————————
                (goto-char (point-min))
                (while (re-search-forward "#<overlay" nil t)
                  (unless (lispy--in-string-or-comment-p)
                    (backward-delete-char 9)
                    (insert "(ly-raw overlay ")
                    (re-search-forward ">")
                    (backward-delete-char 1)
                    (insert ")")))
                ;; ——— cons cell syntax ———————
                (lispy--replace-regexp-in-code " \\. " " (ly-raw dot) ")
                ;; ———  ———————————————————————
                (buffer-substring-no-properties
                 (point-min)
                 (point-max)))))
    (ignore-errors
      (read str))))

(defvar lispy--insert-alist
  '((\` . "`")
    (\, . ",")
    (\,@ . ",@")))

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

(unless (fboundp 'define-error)
  (defun define-error (name message &optional parent)
    "Define NAME as a new error signal.
MESSAGE is a string that will be output to the echo area if such an error
is signaled without being caught by a `condition-case'.
PARENT is either a signal or a list of signals from which it inherits.
Defaults to `error'."
    (unless parent (setq parent 'error))
    (let ((conditions
           (if (consp parent)
               (apply #'nconc
                      (mapcar
                       (lambda (parent)
                         (cons parent
                               (or (get parent 'error-conditions)
                                   (error "Unknown signal `%s'" parent))))
                       parent))
             (cons parent (get parent 'error-conditions)))))
      (put name 'error-conditions
           (delete-dups (copy-sequence (cons name conditions))))
      (when message (put name 'error-message message)))))

(define-error 'unsupported-mode-error "Unsupported mode")
(defun lispy--function-str (fun)
  "Return FUN definition as a string."
  (if (fboundp fun)
      (condition-case e
          (cl-destructuring-bind (buf . pt)
              (save-window-excursion
                (save-excursion
                  (find-function-noselect fun)))
            (with-current-buffer buf
              (if (derived-mode-p
                   'emacs-lisp-mode
                   'clojure-mode
                   'lisp-mode
                   'scheme-mode)
                  (progn
                    (goto-char pt)
                    (lispy--string-dwim))
                (signal 'unsupported-mode-error major-mode))))
        (unsupported-mode-error
         (signal (car e) (cdr e)))
        (error
         (prin1-to-string (symbol-function fun))))
    (error "%s isn't bound" fun)))

(defun lispy--function-parse (str)
  "Extract the function body and args from it's expression STR."
  (let ((body (lispy--read str))
        args)
    (when (eq (car body) 'lambda)
      (setq body (cons 'defun body)))
    (cond ((memq (car body) '(defun defmacro))
           (setq body (lispy--whitespace-trim (cdr body))))
          ((eq (car body) 'defalias)
           (let ((name (cadr (cadr (read str)))))
             (setq body
                   (cons name (cdr (symbol-function name))))))
          (t
           (error "Expected defun, defmacro, or defalias got %s" (car body))))
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

(defun lispy--flatten-function (fstr e-args)
  "Return body of FSTR with args replaced by E-ARGS."
  (let* ((p (lispy--function-parse fstr))
         (f-args (car p))
         (body (cadr p))
         f-arg)
    (when (equal f-args '(ly-raw empty))
      (setq f-args))
    (while (setq f-arg (pop f-args))
      (cond ((eq f-arg '&rest)
             (setq f-arg (pop f-args))
             (when f-args
               (error "&rest must be last"))
             (setq body (lispy--replace body f-arg (cons 'list e-args))))
            ((eq f-arg '&optional)
             (setq f-arg (pop f-args))
             (setq body (lispy--replace body f-arg (pop e-args))))
            (t
             (setq body (lispy--replace body f-arg (pop e-args))))))
    (if (= (length body) 1)
        (setq body (car body))
      (setq body (cons 'progn body)))))

(defun lispy--fast-insert (f-expr)
  "`lispy--insert' F-EXPR into a temp buffer and return `buffer-string'."
  (insert
   (with-temp-buffer
     (emacs-lisp-mode)
     (lispy--insert f-expr)
     (buffer-string))))

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

;; ——— Utilities: error reporting ——————————————————————————————————————————————
(defun lispy-complain (msg)
  "Display MSG if `lispy-verbose' is t."
  (when lispy-verbose
    (message "%s: %s"
             (propertize
              (prin1-to-string
               this-command)
              'face 'font-lock-keyword-face)
             msg)
    nil))

;; ——— Utilities: rest —————————————————————————————————————————————————————————
(defun lispy--indent-region (beg end)
  "Indent region BEG END without reporting progress."
  (save-excursion
    (setq end (copy-marker end))
    (goto-char beg)
    (while (< (point) end)
      (or (and (bolp) (eolp))
          (indent-according-to-mode))
      (forward-line 1))
    (move-marker end nil)))

(defun lispy--indent-for-tab ()
  "Call `indent-for-tab-command'."
  (unless (or (eq major-mode 'minibuffer-inactive-mode)
              (= 0 (buffer-size)))
    (let ((tab-always-indent t))
      (noflet ((message (&rest)))
        (indent-for-tab-command)))))

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
    (setq deactivate-mark nil)
    (set-mark beg)
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
    (cond ((looking-at "[\n ]+")
           (delete-region (match-beginning 0)
                          (match-end 0)))
          ((looking-at lispy-right))

          (t
           (just-one-space)
           (when (looking-back "( ")
             (backward-delete-char 1))))))

(declare-function helm "ext:helm")

(defun lispy--current-tag ()
  "Forward to `semantic-current-tag'.
Try to refresh if nil is returned."
  (let ((tag (lispy-from-left
              (semantic-current-tag))))
    (setq tag
          (or tag
              (progn
                (semantic-clear-toplevel-cache)
                (semantic-fetch-tags)
                (semantic-current-tag))))
    (when tag
      (or (catch 'break
            (lispy--tag-name tag))
          (semantic-tag-name tag)))))

(defun lispy--select-candidate (candidates action)
  "Select from CANDIDATES list with `helm'.
ACTION is called for the selected candidate."
  (require 'helm)
  (require 'helm-help)
  ;; allows restriction with space
  (require 'helm-match-plugin)
  (let (helm-update-blacklist-regexps)
    (helm :sources
          `((name . "semantic tags")
            (candidates . ,(mapcar
                            (lambda (x)
                              (if (listp x)
                                  (if (stringp (cdr x))
                                      (cons (cdr x) (car x))
                                    (cons (car x) x))
                                x))
                            candidates))
            (action . ,action))
          :preselect
          (let ((stag (semantic-current-tag))
                (tag (ignore-errors
                       (lispy--current-tag))))
            (if tag
                (if (eq (semantic-tag-class
                         stag)
                        'function)
                    (format "\\_<%s\\_>"
                            (semantic-tag-name stag))
                  tag)
              ""))
          :buffer "*lispy-goto*")))

(defun lispy--action-jump (tag)
  "Jump to TAG."
  (when (semantic-tag-p tag)
    (let ((overlay (semantic-tag-overlay tag)))
      (cond ((overlayp overlay))
            ((arrayp overlay)
             (semantic--tag-set-overlay
              tag
              (setq overlay
                    (make-overlay (or (aref overlay 0) 1)
                                  (or (aref overlay 1) 1)
                                  (if (>= (length overlay) 3)
                                      (find-file (aref overlay 2))
                                    (current-buffer))))))
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

(defun lispy--prin1-to-string (expr offset mode)
  "Return the string representation of EXPR.
EXPR is indented first, with OFFSET being the column position of
the first character of EXPR.
MODE is the major mode for indenting EXPR."
  (with-temp-buffer
    (funcall mode)
    (dotimes (i offset)
      (insert ?\ ))
    (lispy--insert expr)
    (buffer-substring-no-properties
     (+ (point-min) offset)
     (point-max))))

(defun lispy--insert (expr)
  "Insert the EXPR read by `lispy--read'."
  (let ((start-pt (point))
        beg end
        sxp type)
    (prin1 expr (current-buffer))
    (save-restriction
      (narrow-to-region start-pt (setq end (point)))
      (goto-char (point-min))
      (while (and (re-search-forward "(ly-raw" nil t)
                  (setq beg (match-beginning 0)))
        (goto-char beg)
        (setq sxp (ignore-errors (read (current-buffer))))
        (setq type (cadr sxp))
        (cl-case type
          (newline
           (delete-region beg (point))
           (delete-char
            (- (skip-chars-backward " ")))
           (insert "\n"))
          ((string comment symbol)
           (delete-region beg (point))
           (insert (caddr sxp)))
          (ignore
           (delete-region beg (point))
           (backward-delete-char 1))
          (raw
           (delete-region beg (point))
           (lispy--insert (cddr sxp))
           (backward-list)
           (forward-char)
           (insert "ly-raw "))
          (quote
           (delete-region beg (point))
           (insert "'")
           (prin1 (caddr sxp) (current-buffer))
           (goto-char beg))
          (empty
           (delete-region beg (point))
           (insert "()"))
          (char
           (delete-region beg (point))
           (insert "?" (caddr sxp)))
          (function
           (delete-region beg (point))
           (insert (format "#'%S" (caddr sxp)))
           (goto-char beg))
          (clojure-lambda
           (delete-region beg (point))
           (insert (format "#%S" (cddr sxp)))
           (goto-char beg))
          (clojure-set
           (delete-region beg (point))
           (insert (format "#{%s}"
                           (let ((s (prin1-to-string (cddr sxp))))
                             (substring s 1 (1- (length s))))))
           (goto-char beg))
          (clojure-map
           (delete-region beg (point))
           (insert (format "{%s}"
                           (let ((s (prin1-to-string (cddr sxp))))
                             (substring s 1 (1- (length s))))))
           (goto-char beg))
          (overlay
           (delete-region beg (point))
           (insert (format "#<%s>"
                           (let ((s (prin1-to-string (cddr sxp))))
                             (substring s 1 (1- (length s))))))
           (goto-char beg))
          (\`
           (if (> (length sxp) 3)
               (progn
                 (goto-char beg)
                 (insert "`")
                 (delete-region (+ (point) 1)
                                (+ (point) 11)))
             (delete-region beg (point))
             (insert "`")
             (prin1 (caddr sxp) (current-buffer)))
           (goto-char beg))
          (\,
           (delete-region beg (point))
           (insert ",")
           (prin1 (caddr sxp) (current-buffer))
           (goto-char beg))
          (\,@
           (delete-region beg (point))
           (insert ",@")
           (prin1 (caddr sxp) (current-buffer))
           (goto-char beg))
          (dot
           (delete-region beg (point))
           (insert "."))
          (t (goto-char (1+ beg)))))
      (goto-char (point-min))
      (while (re-search-forward "[a-z-A-Z]\\(\\\\\\?\\)" nil t)
        (replace-match "?" t t nil 1))
      (goto-char (point-min))
      (while (re-search-forward "\\\\\\." nil t)
        (unless (lispy--in-string-p)
          (replace-match ".")))
      (when lispy-do-fill
        (goto-char (point-min))
        (while (re-search-forward " " nil t)
          (cond ((lispy--in-string-p))

                ((lispy--in-comment-p)
                 (fill-paragraph)
                 (goto-char (cdr (lispy--bounds-comment))))

                ((> (current-column) fill-column)
                 (newline-and-indent)))))
      (goto-char (point-max))
      (widen)))
  (backward-list)
  (indent-sexp)
  (forward-list))

(defun lispy--normalize-1 ()
  "Normalize/prettify current sexp."
  (let* ((bnd (lispy--bounds-dwim))
         (str (lispy--string-dwim bnd))
         (offset (save-excursion
                   (goto-char (car bnd))
                   (current-column)))
         (was-left (looking-at lispy-left)))
    (if (or (and (memq major-mode '(clojure-mode))
                 (string-match ",\\|\\^\\|#[^({]" str))
            (> (length str) 10000))

        (lispy-from-left
         (indent-sexp))
      (let* ((max-lisp-eval-depth 10000)
             (max-specpdl-size 10000)
             (res (lispy--sexp-normalize
                   (lispy--read str)))
             (new-str (lispy--prin1-to-string res offset major-mode)))
        (unless (string= str new-str)
          (delete-region (car bnd)
                         (cdr bnd))
          (insert new-str)
          (when was-left
            (backward-list)))))))

(defun lispy--sexp-trim-leading-newlines (expr comment)
  "Trim leading (ly-raw newline) from EXPR.
Treat comments differently when COMMENT is t."
  (while (and (consp expr)
              (listp expr)
              (equal (car expr) '(ly-raw newline))
              (not (and comment
                        (lispy--raw-comment-p (cadr expr)))))
    (setq expr (cdr expr)))
  expr)

(defun lispy--sexp-trim-newlines (expr)
  "Trim leading and trailing (ly-raw newline) from EXPR."
  (if (and (consp expr)
           (consp (cdr expr)))
      (nreverse
       (lispy--sexp-trim-leading-newlines
        (nreverse
         (lispy--sexp-trim-leading-newlines expr nil))
        t))
    expr))

(defun lispy--sexp-trim-trailing-newlines (foo comment)
  "Trim trailing (ly-raw newline) from FOO.
Treat comments differently when COMMENT is t."
  (if (and (consp foo) (consp (cdr foo)))
      (let ((expr (reverse foo)))
        (while (and (consp expr)
                    (listp expr)
                    (equal (car expr) '(ly-raw newline))
                    (not (and comment
                              (lispy--raw-comment-p (cadr expr)))))
          (setq expr (cdr expr)))
        (reverse expr))
    foo))

(defun lispy--sexp-normalize (foo)
  "Return a pretty version of FOO.
Only `ly-raw' lists within FOO are manipulated."
  (cond ((null foo)
         nil)

        ((consp foo)
         (cons (lispy--sexp-normalize
                (lispy--sexp-trim-trailing-newlines (car foo) t))
               (lispy--sexp-normalize
                (lispy--sexp-trim-trailing-newlines (cdr foo) t))))
        (t
         foo)))

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
  "Swap buffer regions BND1 and BND2.
Return a cons of the new text cordinates."
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
    (goto-char (car bnd1)))
  (let* ((l1 (- (cdr bnd1) (car bnd1)))
         (l2 (- (cdr bnd2) (car bnd2)))
         (new-beg (+ (car bnd2) (- l2 l1)))
         (new-end (+ new-beg l1)))
    (cons
     (cons (car bnd1) (+ (car bnd1) l2))
     (cons new-beg new-end))))

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

(defvar lispy--edebug-command nil
  "Command that corresponds to currently pressed key.")

(defun lispy--edebug-commandp ()
  "Return true if `this-command-keys' should be forwarded to edebug."
  (when (and (bound-and-true-p edebug-active)
             (= 1 (length (this-command-keys))))
    (let ((char (aref (this-command-keys) 0)))
      (setq lispy--edebug-command
            (cdr (or (assq char edebug-mode-map)
                     (assq char global-edebug-map)))))))

(defun lispy--insert-or-call (def plist)
  "Return a lambda to call DEF if position is special.
Otherwise call `self-insert-command'.
PLIST currently accepts:
- :disable with a mode to disable
- :override with a lambda to conditionally abort command"
  (let ((disable (plist-get plist :disable))
        (override (plist-get plist :override)))
    `(lambda ,(help-function-arglist def)
       ,(format "Call `%s' when special, self-insert otherwise.\n\n%s"
                (symbol-name def) (documentation def))
       ,(interactive-form def)
       ,@(when disable `((,disable -1)))
       (cond ,@(when override `(((,override))))

             ((lispy--edebug-commandp)
              (call-interactively
               lispy--edebug-command))

             ((and (bound-and-true-p god-global-mode))
              (call-interactively 'god-mode-self-insert))

             ((region-active-p)
              (call-interactively ',def))

             ((lispy--in-string-or-comment-p)
              (call-interactively 'self-insert-command))

             ((save-match-data
                (or (looking-at lispy-left)
                    (looking-back lispy-right)))
              (call-interactively ',def))

             ((save-match-data
                (and (looking-back "^ *") (looking-at ";")))
              (call-interactively ',def))

             (t
              (call-interactively 'self-insert-command))))))

(defun lispy--setq-expression ()
  "Return the smallest list to contain point.
If inside VARLIST part of `let' form,
return the corresponding `setq' expression."
  (ignore-errors
    (save-excursion
      (cond ((looking-at lispy-left)
             (forward-list))
            ((looking-back lispy-right))
            (t
             (up-list)))
      (let ((tsexp (lispy--preceding-sexp)))
        (backward-list 1)
        (cond
          ((looking-back "(\\(?:let\\|lexical-let\\)\\*?[ \t]*")
           (cons 'setq
                 (if (and (= (length tsexp) 1)
                          (listp (car tsexp)))
                     (car tsexp)
                   (cl-mapcan
                    (lambda (x) (unless (listp x) (list x nil)))
                    tsexp))))
          ((ignore-errors
             (up-list)
             (backward-list 1)
             (looking-back "(\\(?:let\\|lexical-let\\)\\*?[ \t]*"))
           (cons 'setq tsexp))
          (t
           tsexp))))))

;; ——— Key definitions —————————————————————————————————————————————————————————
(defvar ac-trigger-commands '(self-insert-command))
(defvar company-begin-commands '(self-insert-command))
(defvar company-no-begin-commands '(special-lispy-space))
(defvar mc/cmds-to-run-for-all nil)
(defvar mc/cmds-to-run-once nil)
(mapc (lambda (x) (add-to-list 'mc/cmds-to-run-once x))
      '(lispy-cursor-down special-lispy-other-mode))
(mapc (lambda (x) (add-to-list 'mc/cmds-to-run-for-all x))
      '(lispy-parens lispy-brackets lispy-braces lispy-quotes
        lispy-kill lispy-delete))

(defadvice ac-handle-post-command (around ac-post-command-advice activate)
  "Don't `auto-complete' when region is active."
  (unless (region-active-p)
    ad-do-it))

(let ((map lispy-mode-x-map))
  (define-key map "d" 'lispy-to-defun)
  (define-key map "l" 'lispy-to-lambda)
  (define-key map "e" 'lispy-edebug)
  (define-key map "m" 'lispy-cursor-ace)
  (define-key map "i" 'lispy-to-ifs)
  (define-key map "c" 'lispy-to-cond)
  (define-key map "f" 'lispy-flatten)
  (define-key map "r" 'lispy-eval-and-replace)
  (define-key map "s" 'save-buffer)
  (define-key map "j" 'lispy-debug-step-in)
  (define-key map "h" 'lispy-describe)
  (define-key map "u" 'lispy-unbind-variable)
  (define-key map "b" 'lispy-bind-variable)
  (define-key map (char-to-string help-char)
    (lambda ()
      (interactive)
      (describe-bindings (kbd "C-4")))))

(defun lispy-define-key (keymap key def &rest plist)
  "Forward to (`define-key' KEYMAP KEY FUNC).
FUNC is obtained from (`lispy--insert-or-call' DEF PLIST)"
  (declare (indent 3))
  (let ((func (defalias (intern (concat "special-" (symbol-name def)))
                  (lispy--insert-or-call def plist))))
    (add-to-list 'ac-trigger-commands func)
    (unless (memq func mc/cmds-to-run-once)
      (add-to-list 'mc/cmds-to-run-for-all func))
    (unless (memq func company-no-begin-commands)
      (add-to-list 'company-begin-commands func))
    (eldoc-add-command func)
    (define-key keymap (kbd key) func)))

(lispy-defverb
 "goto"
 (("d" lispy-goto)
  ("l" lispy-goto-local)
  ("r" lispy-goto-recursive)
  ("p" lispy-goto-projectile)
  ("f" lispy-follow)
  ("b" pop-tag-mark)
  ("q" lispy-quit)
  ("j" lispy-goto-def-down)
  ("a" lispy-goto-def-ace)))

(lispy-defverb
 "other"
 (("h" lispy-move-left)
  ("j" lispy-down-slurp)
  ("k" lispy-up-slurp)
  ("l" lispy-move-right)
  ("SPC" lispy-other-space)
  ("g" lispy-goto-mode)))

(lispy-defverb
 "transform"
 (("o" lispy-oneline)
  ("m" lispy-multiline)
  ("s" lispy-stringify)
  ("d" lispy-to-defun)
  ("l" lispy-to-lambda)
  ("i" lispy-to-ifs)
  ("c" lispy-to-cond)
  ("f" lispy-flatten)
  ("a" lispy-teleport)))

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
  (define-key map (kbd "'") 'lispy-tick)
  (define-key map (kbd "`") 'lispy-backtick)
  (define-key map (kbd "C-j") 'lispy-newline-and-indent)
  (define-key map (kbd "M-j") 'lispy-split)
  (define-key map (kbd "M-J") 'lispy-join)
  (define-key map (kbd "RET") 'lispy-newline-and-indent-plain)
  (define-key map (kbd ";") 'lispy-comment)
  (define-key map (kbd "C-e") 'lispy-move-end-of-line)
  (define-key map (kbd "<C-return>") 'lispy-open-line)
  (define-key map (kbd "<M-return>") 'lispy-alt-line)
  ;; ——— globals: C-1 .. C-9 ——————————————————
  (when lispy-use-ctrl-digits
    (define-key map (kbd "C-1") 'lispy-describe-inline)
    (define-key map (kbd "C-2") 'lispy-arglist-inline)
    (define-key map (kbd "C-3") 'lispy-out-forward)
    (define-key map (kbd "C-4") lispy-mode-x-map)
    (define-key map (kbd "C-7") 'lispy-cursor-down)
    (define-key map (kbd "C-8") 'lispy-parens-down)
    (define-key map (kbd "C-9") 'lispy-out-forward-newline))
  ;; ——— globals: tags ————————————————————————
  (define-key map (kbd "M-.") 'lispy-goto-symbol)
  (define-key map (kbd "M-,") 'pop-tag-mark)
  ;; ——— globals: miscellanea —————————————————
  (define-key map (kbd "M-o") 'lispy-string-oneline)
  (define-key map (kbd "M-i") 'lispy-iedit)
  (define-key map (kbd "M-q") 'lispy-fill)
  ;; ——— locals: navigation ———————————————————
  (lispy-define-key map "l" 'lispy-out-forward)
  (lispy-define-key map "h" 'lispy-out-backward)
  (lispy-define-key map "f" 'lispy-flow)
  (lispy-define-key map "j" 'lispy-down)
  (lispy-define-key map "k" 'lispy-up)
  (lispy-define-key map "d" 'lispy-different)
  (lispy-define-key map "o" 'lispy-other-mode)
  (lispy-define-key map "p" 'lispy-eval-other-window)
  (lispy-define-key map "P" 'lispy-paste)
  (lispy-define-key map "J" 'lispy-outline-next)
  (lispy-define-key map "K" 'lispy-outline-prev)
  (lispy-define-key map "y" 'lispy-occur)
  ;; ——— locals: Paredit transformations ——————
  (lispy-define-key map ">" 'lispy-slurp)
  (lispy-define-key map "<" 'lispy-barf)
  (lispy-define-key map "/" 'lispy-splice)
  (lispy-define-key map "r" 'lispy-raise)
  (lispy-define-key map "R" 'lispy-raise-some)
  (lispy-define-key map "+" 'lispy-join)
  ;; ——— locals: more transformations —————————
  (lispy-define-key map "C" 'lispy-convolute)
  (lispy-define-key map "w" 'lispy-move-up)
  (lispy-define-key map "s" 'lispy-move-down)
  (lispy-define-key map "O" 'lispy-oneline)
  (lispy-define-key map "M" 'lispy-multiline)
  (lispy-define-key map "S" 'lispy-stringify)
  ;; ——— locals: marking —————————————————————
  (lispy-define-key map "a" 'lispy-ace-symbol)
  (lispy-define-key map "H" 'lispy-ace-symbol-replace)
  (lispy-define-key map "m" 'lispy-mark-list)
  ;; ——— locals: dialect-specific —————————————
  (lispy-define-key map "e" 'lispy-eval)
  (lispy-define-key map "E" 'lispy-eval-and-insert)
  (lispy-define-key map "G" 'lispy-goto-local)
  (lispy-define-key map "g" 'lispy-goto)
  (lispy-define-key map "F" 'lispy-follow t)
  (lispy-define-key map "D" 'pop-tag-mark)
  (lispy-define-key map "A" 'lispy-beginning-of-defun)
  ;; ——— locals: miscellanea ——————————————————
  (lispy-define-key map "SPC" 'lispy-space)
  (lispy-define-key map "i" 'lispy-tab)
  (lispy-define-key map "I" 'lispy-shifttab)
  (lispy-define-key map "N" 'lispy-narrow)
  (lispy-define-key map "W" 'lispy-widen)
  (lispy-define-key map "c" 'lispy-clone)
  (lispy-define-key map "u" 'lispy-undo)
  (lispy-define-key map "q" 'lispy-ace-paren
    :override (lambda () (when (bound-and-true-p view-mode) (View-quit) t)))
  (lispy-define-key map "Q" 'lispy-ace-char)
  (lispy-define-key map "v" 'lispy-view)
  (lispy-define-key map "T" 'lispy-ert)
  (lispy-define-key map "t" 'lispy-teleport)
  (lispy-define-key map "n" 'lispy-new-copy)
  (lispy-define-key map "b" 'lispy-store-region-and-buffer)
  (lispy-define-key map "B" 'lispy-ediff-regions)
  (lispy-define-key map "x" 'lispy-x)
  (lispy-define-key map "Z" 'lispy-edebug-stop)
  (lispy-define-key map "V" 'lispy-visit)
  (lispy-define-key map "-" 'lispy-ace-subword)
  (lispy-define-key map "." 'lispy-repeat)
  ;; ——— locals: digit argument ———————————————
  (mapc (lambda (x) (lispy-define-key map (format "%d" x) 'digit-argument))
        (number-sequence 0 9)))

(defun lispy-setup-new-bindings ()
  "Change to new-style bindings.
They may become the defaults in the future."
  (let ((map lispy-mode-map))
    (lispy-define-key map "t" 'lispy-transform-mode))
  (let ((map lispy-mode-x-map))
    (define-key map "x" nil)))

(provide 'lispy)

;;; Local Variables:
;;; outline-regexp: ";; ———"
;;; bug-reference-url-format: "https://github.com/abo-abo/lispy/issues/%s"
;;; End:

;;; lispy.el ends here
