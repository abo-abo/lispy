;;; lispy.el --- vi-like Paredit. -*- lexical-binding: t -*-

;; Copyright (C) 2014-2015 Oleh Krehel

;; Author: Oleh Krehel <ohwoeowho@gmail.com>
;; URL: https://github.com/abo-abo/lispy
;; Version: 0.26.0
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
;; "C-3" - calls `lispy-right' (exit current list forwards)
;; ")" - calls `lispy-right-nostring' (exit current list
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

;;* Requires
(eval-when-compile
  (require 'cl)
  (require 'org))
(require 'lispy-tags)
(require 'help-fns)
(require 'edebug)
(require 'ediff)
(require 'ediff-util)
(require 'eldoc)
(require 'etags)
(require 'outline)
(require 'semantic)
(require 'semantic/db)
(require 'semantic/bovine/el)
(require 'avy)
(require 'newcomment)
(require 'lispy-inline)
(require 'iedit)
(require 'delsel)
(require 'hydra)
(require 'swiper)

;;* Customization
(defgroup lispy nil
  "List navigation and editing for the Lisp family."
  :group 'bindings
  :prefix "lispy-")

(defvar lispy-left "[([{]"
  "Opening delimiter.")

(defvar lispy-right "[])}]"
  "Closing delimiter.")

(defvar lispy-outline "^;;\\(?:;[^#]\\|\\*+\\)"
  "Outline delimiter.")

(defcustom lispy-no-space nil
  "When non-nil, don't insert a space before parens/brackets/braces/colons."
  :type 'boolean
  :group 'lispy)
(make-variable-buffer-local 'lispy-no-space)

(defcustom lispy-lax-eval t
  "When non-nil, fix \"unbound variable\" error by setting the it to nil.
This is useful when hacking functions with &optional arguments.
So evaling (setq mode (or mode major-mode)) will set mode to nil on
the first eval, and to major-mode on the second eval."
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

(defcustom lispy-helm-columns '(70 80)
  "Max lengths of tag and tag+filename when completing with `helm'."
  :group 'lispy)

(defcustom lispy-no-permanent-semantic nil
  "When t, `lispy' will not enable function `semantic-mode' when it's off."
  :type 'boolean
  :group 'lispy)

(defcustom lispy-completion-method 'ivy
  "Method to select a candidate from a list of strings."
  :type '(choice
          (const :tag "Ivy" ivy)
          ;; sensible choice for many tags
          (const :tag "Helm" helm)
          ;; `ido-vertical-mode' is highly recommended here
          (const :tag "Ido" ido)
          ;; `icomplete-mode' and `icy-mode' will affect this
          (const :tag "Default" default)))

(defcustom lispy-visit-method 'ffip
  "Method to switch to a file in the current project."
  :type '(choice
          (const :tag "Find File in Project" ffip)
          (const :tag "Projectile" projectile)))

(defcustom lispy-avy-style-char 'pre
  "Method of displaying the overlays for a char during visual selection."
  :type '(choice
          (const :tag "Pre" pre)
          (const :tag "At" at)
          (const :tag "At full" at-full)
          (const :tag "Post" post)))

(defcustom lispy-avy-style-paren 'at
  "Method of displaying the overlays for a paren during visual selection."
  :type '(choice
          (const :tag "Pre" pre)
          (const :tag "At" at)
          (const :tag "At full" at-full)
          (const :tag "Post" post)))

(defcustom lispy-avy-style-symbol 'pre
  "Method of displaying the overlays for a symbol during visual selection."
  :type '(choice
          (const :tag "Pre" pre)
          (const :tag "At" at)
          (const :tag "At full" at-full)
          (const :tag "Post" post)))

(defcustom lispy-avy-keys (number-sequence ?a ?z)
  "Keys for jumping.")

(defface lispy-command-name-face
  '((((class color) (background light))
     :background "#d8d8f7" :inherit font-lock-function-name-face)
    (((class color) (background dark))
     :background "#333333" :inherit font-lock-function-name-face))
  "Face for Elisp commands."
  :group 'lispy-faces)

(defface lispy-cursor-face
  '((((class color) (background light))
     :background "#000000" :foreground "#ffffff")
    (((class color) (background dark))
     :background "#ffffff" :foreground "#000000"))
  "Face for `lispy-view-test'."
  :group 'lispy-faces)

(defface lispy-test-face
    '((t (:inherit lispy-face-hint)))
  "Face for `lispy-view-test'."
  :group 'lispy-faces)

(defvar lispy-mode-map (make-sparse-keymap))

(defvar lispy-known-verbs nil
  "List of registered verbs.")

(defvar lispy-ignore-whitespace nil
  "When set to t, function `lispy-right' will not clean up whitespace.")

(defcustom lispy-compat '(edebug)
  "List of package compatibility options.
Enabling them adds overhead, so make sure that you are actually
using those packages."
  :type '(repeat
          (choice
           (const :tag "god-mode" god-mode)
           (const :tag "edebug" edebug)
           (const :tag "macrostep" macrostep))))

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
  (if lispy-mode
      (progn
        (setq-local outline-level 'lispy-outline-level)
        (setq-local outline-regexp (substring lispy-outline 1))
        (when (called-interactively-p 'any)
          (mapc #'lispy-raise-minor-mode
                (cons 'lispy-mode lispy-known-verbs))))
    (setq-local outline-regexp ";;;\\(;* [^ \t\n]\\|###autoload\\)\\|(")
    (setq-local outline-level 'lisp-outline-level)))

(defun lispy-raise-minor-mode (mode)
  "Make MODE the first on `minor-mode-map-alist'."
  (let ((x (assq mode minor-mode-map-alist)))
    (when x
      (setq minor-mode-map-alist
            (cons x (delq mode minor-mode-map-alist))))))

;;* Macros
(defmacro lispy-dotimes (n &rest bodyform)
  "Execute N times the BODYFORM unless an error is signaled.
Return nil if couldn't execute BODYFORM at least once.
Otherwise return the amount of times executed."
  (declare (indent 1)
           (debug (form body)))
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

(defmacro lispy-flet (binding &rest body)
  "Temporarily override BINDING and execute BODY."
  (declare (indent 1))
  (let* ((name (car binding))
         (old (cl-gensym (symbol-name name))))
    `(let ((,old (symbol-function ',name)))
       (unwind-protect
            (progn
              (fset ',name (lambda ,@(cdr binding)))
              ,@body)
         (fset ',name ,old)))))

(defvar lispy-site-directory (file-name-directory
                              load-file-name)
  "The directory where all of the lispy files are located.")

;;* Verb related
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

;;* Globals: navigation
(defsubst lispy-right-p ()
  "Return t if after variable `lispy-right'."
  (looking-back lispy-right
                (line-beginning-position)))

(defsubst lispy-left-p ()
  "Return t if before variable `lispy-left'."
  (looking-at lispy-left))

(defun lispy-forward (arg)
  "Move forward list ARG times or until error.
Return t if moved at least once,
otherwise call function `lispy-right' and return nil."
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
            (and (not (lispy-right-p))
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
            (and (not (lispy-left-p))
                 (progn
                   (forward-list)
                   (backward-list)
                   (= pt (point)))))
        (prog1 nil
          (lispy--out-forward 1)
          (backward-list))
      (point))))

(defun lispy-right (arg)
  "Move outside list forwards ARG times.
Return nil on failure, t otherwise."
  (interactive "p")
  (when (called-interactively-p 'interactive)
    (lispy--remember))
  (when (bound-and-true-p abbrev-mode)
    (ignore-errors (expand-abbrev)))
  (cond ((region-active-p)
         (lispy-mark-right arg))
        ((looking-at lispy-outline)
         (lispy-outline-right))
        (t
         (lispy--out-forward arg))))

(defun lispy-right-nostring (arg)
  "Call `lispy--out-forward' with ARG unless in string or comment.
Self-insert otherwise."
  (interactive "p")
  (if (or (lispy--in-string-or-comment-p)
          (looking-back "?\\\\"
                        (line-beginning-position)))
      (self-insert-command arg)
    (lispy--out-forward arg)))

(defun lispy-left (arg)
  "Move outside list forwards ARG times.
Return nil on failure, t otherwise."
  (interactive "p")
  (when (called-interactively-p 'interactive)
    (lispy--remember))
  (cond ((region-active-p)
         (lispy-mark-left arg))
        ((looking-at lispy-outline)
         (lispy-outline-left))
        (t
         (lispy--out-backward arg))))

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
  (let ((pt (point))
        bnd)
    (if (eq pt (line-end-position))
        (if (setq bnd (lispy--bounds-string))
            (goto-char (cdr bnd))
          (when (and (< lispy-meol-point pt)
                     (>= lispy-meol-point (line-beginning-position)))
            (goto-char lispy-meol-point)
            (when (setq bnd (lispy--bounds-string))
              (goto-char (cdr bnd)))))
      (setq lispy-meol-point (point))
      (move-end-of-line 1))))

(defun lispy-move-beginning-of-line ()
  "Forward to `move-beginning-of-line'.
Reveal outlines."
  (interactive)
  (lispy--ensure-visible)
  (if (bolp)
      (back-to-indentation)
    (move-beginning-of-line 1)))

;;* Locals: navigation
(defun lispy-flow (arg)
  "Move inside list ARG times.
Don't enter strings or comments.
Return nil if can't move."
  (interactive "p")
  (when (called-interactively-p 'interactive)
    (lispy--remember))
  (let ((pt (point))
        success)
    (lispy-dotimes arg
      (cond ((or (lispy-left-p)
                 (and (lispy-bolp)
                      (looking-at ";")))
             (forward-char)
             (re-search-forward lispy-left nil t)
             (while (and (lispy--in-string-or-comment-p)
                         (re-search-forward lispy-left nil t)))
             (unless (lispy--in-string-or-comment-p)
               (setq success t))
             (backward-char))

            ((lispy-right-p)
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

(defun lispy-down (arg)
  "Move down ARG times inside current list."
  (interactive "p")
  (when (called-interactively-p 'interactive)
    (lispy--remember))
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

          ((lispy-left-p)
           (lispy-forward arg)
           (let ((pt (point)))
             (if (lispy-forward 1)
                 (lispy-backward 1)
               (goto-char pt)
               (lispy-different))))

          ((lispy-right-p)
           (let ((pt (point)))
             (unless (lispy-forward arg)
               (goto-char pt))))

          ((or (looking-at lispy-outline)
               (and (bolp) (looking-at ";")))
           (let ((pt (point)))
             (lispy-dotimes arg
               (outline-next-visible-heading 1)
               (if (looking-at lispy-outline)
                   (setq pt (point))
                 (goto-char pt)
                 (error "Last outline reached")))))

          (t
           (lispy-forward 1)
           (lispy-backward 1))))
  (lispy--ensure-visible))

(defun lispy-up (arg)
  "Move up ARG times inside current list."
  (interactive "p")
  (when (called-interactively-p 'interactive)
    (lispy--remember))
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

          ((lispy-left-p)
           (let ((pt (point)))
             (unless (lispy-backward arg)
               (goto-char pt))))

          ((lispy-right-p)
           (lispy-backward arg)
           (let ((pt (point)))
             (if (lispy-backward 1)
                 (lispy-forward 1)
               (goto-char pt)
               (lispy-different))))

          ((or (looking-at lispy-outline)
               (and (bolp) (looking-at ";")))
           (let ((pt (point)))
             (lispy-dotimes arg
               (outline-previous-visible-heading 1)
               (if (looking-at lispy-outline)
                   (setq pt (point))
                 (goto-char pt)
                 (error "First outline reached")))))
          (t
           (lispy-backward 1)
           (lispy-forward 1))))
  (lispy--ensure-visible))

(defvar lispy-pos-ring (make-ring 100)
  "Ring for point and mark position history.")

(defun lispy--remember ()
  "Store the current point and mark in history."
  (let* ((emptyp (zerop (ring-length lispy-pos-ring)))
         (top (unless emptyp
                (ring-ref lispy-pos-ring 0))))
    (if (region-active-p)
        (let* ((bnd (lispy--bounds-dwim))
               (bnd (cons
                     (move-marker (make-marker) (car bnd))
                     (move-marker (make-marker) (cdr bnd)))))
          (when (or emptyp
                    (not (equal bnd top)))
            (ring-insert lispy-pos-ring bnd)))
      (when (or emptyp
                (not (equal (point-marker) top)))
        (ring-insert lispy-pos-ring (point-marker))))))

(defun lispy-back (arg)
  "Move point to ARGth previous position.
If position isn't special, move to previous or error."
  (interactive "p")
  (lispy-dotimes arg
    (if (zerop (ring-length lispy-pos-ring))
        (lispy-complain "At beginning of point history")
      (let ((pt (ring-remove lispy-pos-ring 0)))
        ;; After deleting some text, markers that point to it converge
        ;; to one point
        (while (and (not (zerop (ring-length lispy-pos-ring)))
                    (equal (ring-ref lispy-pos-ring 0)
                           pt))
          (ring-remove lispy-pos-ring 0))
        (if (consp pt)
            (lispy--mark pt)
          (deactivate-mark)
          (goto-char pt))))))

(defun lispy-knight-down ()
  "Make a knight-like move: down and right."
  (interactive)
  (cond ((lispy-right-p)
         (lispy-different))
        ((lispy-left-p))
        (t (lispy-backward 1)))
  (let ((pt (point))
        (bnd (save-excursion
               (lispy-beginning-of-defun)
               (lispy--bounds-list))))
    (catch 'done
      (while t
        (forward-line)
        (cond ((>= (point) (cdr bnd))
               (goto-char pt)
               (throw 'done nil))
              ((looking-at (concat "\\s-*" lispy-left))
               (goto-char (1- (match-end 0)))
               (throw 'done t)))))))

(defun lispy-knight-up ()
  "Make a knight-like move: up and right."
  (interactive)
  (cond ((lispy-right-p)
         (lispy-different))
        ((lispy-left-p))
        (t (lispy-backward 1)))
  (let ((pt (point))
        (bnd (save-excursion
               (lispy-beginning-of-defun)
               (lispy--bounds-list))))
    (catch 'done
      (while t
        (beginning-of-line 0)
        (cond ((< (point) (car bnd))
               (goto-char pt)
               (throw 'done nil))
              ((looking-at (concat "\\s-*" lispy-left))
               (goto-char (1- (match-end 0)))
               (throw 'done t)))))))

(defun lispy-different ()
  "Switch to the different side of current sexp."
  (interactive)
  (cond ((and (region-active-p)
              (not (= (region-beginning) (region-end))))
         (exchange-point-and-mark))
        ((lispy-right-p)
         (backward-list))
        ((lispy-left-p)
         (forward-list))
        (t
         (user-error "Unexpected"))))

;;* Globals: kill, yank, delete, mark, copy
(defun lispy-kill ()
  "Kill line, keeping parens consistent."
  (interactive)
  (let (bnd)
    (cond ((lispy--in-comment-p)
           (kill-line))

          ((and (setq bnd (lispy--bounds-string))
                (or
                 (not (eq (point) (car bnd)))
                 (> (count-lines (car bnd) (cdr bnd)) 1)))
           (if (> (cdr bnd) (line-end-position))
               (if (eq (point) (car bnd))
                   (kill-region (car bnd) (cdr bnd))
                 (kill-line))
             (kill-region (point) (1- (cdr bnd)))))
          ((looking-at " *\n")
           (kill-region
            (match-beginning 0)
            (match-end 0))
           (lispy--indent-for-tab))
          ((and (looking-at lispy-right) (looking-back lispy-left
                                                       (line-beginning-position)))
           (delete-char 1)
           (backward-delete-char 1))
          (t
           (let ((beg (point))
                 (end (line-end-position)))
             (if (= (count-matches lispy-left beg end)
                    (count-matches lispy-right beg end))
                 (kill-line)
               (if (let ((lispy-ignore-whitespace t))
                     (lispy--out-forward 1))
                   (progn
                     (backward-char 1)
                     (if (= beg (point))
                         (lispy--out-backward 1)
                       (kill-region beg (point))))
                 (if (= beg (point))
                     (progn
                       (setq bnd (lispy--bounds-dwim))
                       (kill-region (car bnd) (cdr bnd)))
                   (kill-region beg (point))))))))))

(defun lispy-kill-word (arg)
  "Kill ARG words, keeping parens consistent."
  (interactive "p")
  (let (bnd)
    (lispy-dotimes arg
      (while (not (or (eobp)
                      (memq (char-syntax (char-after))
                            '(?w ?_))))
        (forward-char 1))
      (delete-horizontal-space)
      (if (setq bnd (lispy--bounds-string))
          (save-restriction
            (narrow-to-region (1+ (car bnd)) (1- (cdr bnd)))
            (kill-word 1)
            (widen))
        (kill-word 1)))))

(defun lispy-backward-kill-word (arg)
  "Kill ARG words backward, keeping parens consistent."
  (interactive "p")
  (let (bnd
        (pt (point))
        skipped)
    (lispy-dotimes arg
      (setq skipped (skip-chars-backward " \n"))
      (if (memq (char-syntax (char-before))
                '(?w ?_))
          (if (lispy-looking-back "\\_<\\s_+")
              (delete-region (match-beginning 0)
                             (match-end 0))
            (backward-word 1)
            (kill-region (point) pt)
            (when (and (lispy--in-string-p)
                       (not (lispy-looking-back "\\\\\\\\"))
                       (lispy-looking-back "\\\\"))
              (delete-char -1)))
        (delete-region (point) pt)
        (unless (or (zerop skipped)
                    (looking-at " \\|$"))
          (insert " ")
          (backward-char))
        (while (not (or (bobp)
                        (memq (char-syntax (char-before))
                              '(?w ?_))))
          (backward-char 1))
        (if (setq bnd (lispy--bounds-string))
            (progn
              (save-restriction
                (if (and (looking-at "\\s-+\"")
                         (eq (match-end 0) (cdr bnd)))
                    (goto-char (1- (cdr bnd)))
                  (when (and (> pt (car bnd))
                             (< pt (cdr bnd)))
                    (goto-char pt)))
                (narrow-to-region (1+ (car bnd)) (point))
                (kill-region (progn
                               (forward-word -1)
                               (when (and (not (lispy-looking-back "\\\\\\\\"))
                                          (lispy-looking-back "\\\\"))
                                 (backward-char))
                               (point))
                             (point-max))
                (widen)))
          (backward-kill-word 1))))))

(defun lispy-kill-sentence ()
  "Kill until the end of current string or list."
  (interactive)
  (if (or (lispy-left-p) (looking-at "\""))
      (lispy-delete 1)
    (let ((bnd
           (or (lispy--bounds-string)
               (lispy--bounds-list))))
      (kill-region (point) (1- (cdr bnd))))))

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
  (let (bnd)
    (cond ((region-active-p)
           (delete-region
            (region-beginning) (region-end)))

          ((setq bnd (lispy--bounds-string))
           (cond ((eq (1+ (point)) (cdr bnd))
                  (goto-char (car bnd)))
                 ((looking-at "\\\\\"")
                  (if (eq (+ (point) 2) (cdr bnd))
                      (goto-char (car bnd))
                    (delete-char 2)))
                 ((and (looking-at "\"")
                       (lispy-looking-back "\\\\"))
                  (backward-char 1)
                  (delete-char 2))
                 ((lispy--delete-pair-in-string "\\\\\\\\(" "\\\\\\\\)"))
                 ((looking-at "\\\\\\\\")
                  (delete-char 2))
                 ((and (looking-at "\\\\")
                       (lispy-looking-back "\\\\"))
                  (backward-char 1)
                  (delete-char 2))
                 ((eq (point) (car bnd))
                  (delete-region (car bnd)
                                 (cdr bnd))
                  (let ((pt (point)))
                    (skip-chars-forward " ")
                    (delete-region pt (point))))
                 ((save-excursion
                    (forward-char 1)
                    (lispy--in-string-or-comment-p))
                  (delete-char arg))
                 (t
                  (lispy--exit-string))))

          ((lispy--in-comment-p)
           (if (lispy-bolp)
               (let ((bnd (lispy--bounds-comment)))
                 (delete-region (car bnd) (cdr bnd)))
             (delete-char arg)))

          ((looking-at lispy-right)
           (lispy-left 1))

          ((lispy-left-p)
           (lispy--delete-quote-garbage)
           (lispy-dotimes arg
             (lispy--delete)))

          ((eolp)
           (delete-char 1)
           (let ((pt (point)))
             (skip-chars-forward " ")
             (delete-region pt (point))
             (unless (or (eolp)
                         (bolp)
                         (lispy-bolp))
               (insert " "))))

          (t
           (delete-char arg)))))

(defun lispy--delete-quote-garbage ()
  "Delete any combination of `',@ preceeding point."
  (let ((pt (point)))
    (skip-chars-backward "`',@")
    (delete-region (point) pt)))

(defun lispy--delete-whitespace-backward ()
  "Delete spaces backward."
  (let ((pt (point)))
    (skip-chars-backward " ")
    (delete-region (point) pt)))

(defun lispy-delete-backward (arg)
  "From \")|\", delete ARG sexps backwards.
Otherwise (`backward-delete-char-untabify' ARG)."
  (interactive "p")
  (let (bnd)
    (cond ((region-active-p)
           (delete-region (region-beginning)
                          (region-end)))
          ((bobp))

          ((and (setq bnd (lispy--bounds-string))
                (not (eq (point) (car bnd))))
           (cond ((eq (- (point) (car bnd)) 1)
                  (goto-char (cdr bnd)))
                 ((or (looking-back "\\\\\\\\(" (car bnd))
                      (looking-back "\\\\\\\\)" (car bnd)))
                  (let ((pt (point)))
                    (goto-char (match-beginning 0))
                    (unless (lispy--delete-pair-in-string
                             "\\\\\\\\(" "\\\\\\\\)")
                      (goto-char pt)
                      (backward-delete-char-untabify arg))))
                 ((looking-back "\\\\[^\\]" (car bnd))
                  (backward-delete-char 2))
                 (t
                  (backward-delete-char-untabify arg))))

          ((looking-at lispy-outline)
           (if (lispy-looking-back (concat lispy-outline ".*\n"))
               (delete-region
                (match-beginning 0)
                (match-end 0))
             (delete-char -1)))

          ((lispy--in-comment-p)
           (if (lispy-looking-back "^ +")
               (progn
                 (delete-region (1- (match-beginning 0))
                                (match-end 0))
                 (indent-for-tab-command))
             (backward-delete-char-untabify arg)))

          ((lispy-looking-back "\\\\.")
           (backward-delete-char-untabify arg))

          ((and (lispy-looking-back (concat lispy-right " "))
                (looking-at " *$"))
           (backward-delete-char-untabify arg))

          ((or (lispy-right-p)
               (and (lispy-looking-back (concat lispy-right " "))
                    (or (lispy-left-p) (looking-at "\""))))
           (let ((pt (point)))
             (lispy-backward arg)
             (skip-chars-backward "`',@# \t")
             (delete-region pt (point))
             (unless (or (looking-at " ")
                         (lispy-bolp)
                         (and (lispy-right-p)
                              (not (or (lispy-left-p)
                                       (looking-at "\""))))
                         (lispy-looking-back lispy-left))
               (just-one-space))
             (setq pt (point))
             (if (and
                  (not (lispy-bolp))
                  (not (lispy-left-p))
                  (progn
                    (skip-chars-backward " \t\n")
                    (lispy-right-p)))
                 (delete-region (point) pt)
               (goto-char pt)
               (indent-for-tab-command))))

          ((and (lispy-looking-back lispy-left)
                (not (lispy-looking-back "\\\\.")))
           (lispy--out-forward 1)
           (lispy-delete-backward 1))

          ((eq (char-before) ?\")
           (backward-char 1)
           (let ((bnd (lispy--bounds-string)))
             (delete-region (car bnd)
                            (cdr bnd))
             (lispy--delete-whitespace-backward)
             (unless (looking-at " ")
               (insert " "))
             (indent-for-tab-command)))

          ((and (lispy-after-string-p "\" ")
                (not (looking-at lispy-right)))
           (let ((pt (point)))
             (goto-char (match-beginning 0))
             (delete-region (car (lispy--bounds-string)) pt))
           (lispy--delete-whitespace-backward)
           (unless (lispy-looking-back lispy-left)
             (just-one-space))
           (indent-for-tab-command))

          ((lispy-bolp)
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
                           (lispy-looking-back lispy-left))
                 (just-one-space)))
             (indent-for-tab-command)))

          ((lispy-looking-back "[^ ]  +")
           (delete-region (+ (match-beginning 0) 2) (point)))

          (t
           (backward-delete-char-untabify arg))))
  (when (and (buffer-file-name)
             (< (- (line-number-at-pos (point))
                   (line-number-at-pos (window-start)))
                5))
    (ignore-errors
      (recenter -20)))
  (when (lispy-left-p)
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
  (when (called-interactively-p 'interactive)
    (lispy--remember))
  (cond ((> arg 1)
         (lispy-mark-car)
         (lispy-down (1- arg)))
        ((= arg 0)
         (let ((bnd (lispy--bounds-dwim)))
           (lispy--mark
            (cons (1+ (car bnd))
                  (1- (cdr bnd))))))
        ((region-active-p)
         (deactivate-mark)
         (if (lispy--in-comment-p)
             (progn
               (beginning-of-line)
               (skip-chars-forward " "))
           (skip-chars-forward ",@'`")))
        ((lispy-left-p)
         (lispy--mark
          (lispy--bounds-dwim)))
        ((lispy-right-p)
         (lispy--mark
          (lispy--bounds-dwim))
         (lispy-different))
        ((and (lispy-bolp) (looking-at ";"))
         (lispy--mark (lispy--bounds-comment))))
  (setq this-command 'lispy-mark-list))

(defvar-local lispy-bind-var-in-progress nil
  "When t, `lispy-mark-symbol' will exit `iedit'.")

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
            (setq bnd (lispy--bounds-string))
            (= (1+ (point))
               (cdr bnd)))
           (lispy--mark bnd))

          ((looking-at " *[[({]")
           (let ((pt (point)))
             (skip-chars-forward "(){}[] \n")
             (set-mark-command nil)
             (condition-case nil
                 (progn
                   (re-search-forward "[][(){} \n]")
                   (while (lispy--in-string-or-comment-p)
                     (re-search-forward "[() \n]"))
                   (backward-char 1))
               (error
                (message "No further symbols found")
                (deactivate-mark)
                (goto-char pt)))))

          ((lispy-right-p)
           (skip-chars-backward "() \n")
           (set-mark-command nil)
           (re-search-backward "[() \n]")
           (while (lispy--in-string-or-comment-p)
             (re-search-backward "[() \n]"))
           (forward-char 1))

          ((region-active-p)
           (ignore-errors
             (forward-sexp)))

          ((looking-at lispy-right)
           (lispy--mark
            (save-excursion
              (backward-char 1)
              (lispy--bounds-dwim))))

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
    (unless (equal str (ignore-errors
                         (current-kill 0)))
      (kill-new str))))

;;* Globals: pairs
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
            (when (and (lispy-left-p)
                       (looking-back lispy-left))
              (insert " "))
            (backward-char 1))
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
            (if (lispy-right-p)
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
  (let (bnd)
    (cond ((region-active-p)
           (if arg
               (lispy-unstringify)
             (lispy-stringify)))
          ((and (setq bnd (lispy--bounds-string))
                (not (= (point) (car bnd))))
           (if arg
               (lispy-unstringify)
             (insert "\\\"\\\"")
             (backward-char 2)))

          (arg
           (lispy-stringify))

          ((lispy-after-string-p "?\\")
           (self-insert-command 1))

          (t
           (lispy--space-unless "^\\|\\s-\\|\\s(\\|[#]")
           (insert "\"\"")
           (unless (looking-at "\n\\|)\\|}\\|\\]\\|$")
             (just-one-space)
             (backward-char 1))
           (backward-char)))))

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

;;* Globals: insertion
(defun lispy-space (arg)
  "Insert one space, with position depending on ARG.
If ARG is 2, amend the current list with a space from current side.
If ARG is 3, switch to the different side beforehand.
If jammed between parens, \"(|(\" unjam: \"( |(\"."
  (interactive "p")
  (cond ((region-active-p)
         (goto-char (region-end))
         (deactivate-mark)
         (insert " "))
        ((eq arg 4)
         (when (lispy--leftp)
           (lispy-different))
         (backward-char)
         (unless (lispy-bolp)
           (newline-and-indent)))
        ((or (eq arg 2)
             (when (eq arg 3)
               (lispy-different)
               t))

         (if (lispy-left-p)
             (progn
               (forward-char)
               (just-one-space)
               (backward-char))
           (backward-char)
           (just-one-space)))
        (t
         (insert " ")
         (when (and (lispy-left-p)
                    (lispy-after-string-p "( "))
           (backward-char)))))

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
  (lispy--space-unless "\\s-\\|\\s(\\|[:?]\\|\\\\")
  (insert "^"))

(defun lispy-tick ()
  "Insert '."
  (interactive)
  (if (lispy--string-markedp)
      (lispy-unstringify)
    (lispy--space-unless "\\s-\\|\\s(\\|[~#:?'`]\\|\\\\")
    (insert "'")))

(defun lispy-backtick ()
  "Insert `."
  (interactive)
  (if (region-active-p)
      (lispy--surround-region "`" "'")
    (lispy--space-unless "\\s-\\|\\s(\\|[:?`']\\|\\\\")
    (insert "`")))

(defun lispy-tilde (arg)
  "Insert ~ ARG times.
When region is active, toggle a ~ at the start of the region."
  (interactive "p")
  (if (region-active-p)
      (let ((bnd (lispy--bounds-dwim))
            deactivate-mark)
        (save-excursion
          (goto-char (car bnd))
          (if (eq (char-after) ?~)
              (delete-char 1)
            (insert "~"))))
    (self-insert-command arg)))

(defun lispy-hash ()
  "Insert #."
  (interactive)
  (if (and (memq major-mode '(clojure-mode
                              nrepl-repl-mode
                              cider-clojure-interaction-mode))
           (lispy-looking-back "\\sw #"))
      (progn
        (backward-delete-char 2)
        (insert "#"))
    (lispy--space-unless "\\s-\\|\\s(\\|[#:?'`,]\\\\?")
    (insert "#")))

(declare-function cider-eval-print-last-sexp "ext:cider-interaction")
(defun lispy-newline-and-indent ()
  "Insert newline."
  (interactive)
  (cond ((eq major-mode 'lisp-interaction-mode)
         (setq this-command 'eval-last-sexp)
         (eval-print-last-sexp))
        ((eq major-mode 'cider-clojure-interaction-mode)
         (setq this-command 'cider-eval-print-last-sexp)
         (cider-eval-print-last-sexp))
        ((lispy-left-p)
         (skip-chars-backward ",@'`#")
         (newline-and-indent)
         (skip-chars-forward ",@'`#")
         (indent-sexp))
        (t
         (lispy-newline-and-indent-plain))))

(declare-function cider-repl-return "ext:cider-repl")
(declare-function slime-repl-return "ext:slime-repl")
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
     (if (and (not (lispy--in-string-or-comment-p))
              (lispy-looking-back "[^#`',@~][#`',@~]+"))
         (save-excursion
           (goto-char (match-beginning 0))
           (newline-and-indent))
       (newline-and-indent))
     (let ((lispy-ignore-whitespace t))
       (save-excursion
         (lispy--out-backward 1)
         (indent-sexp))))))

(defun lispy-open-line ()
  "Add one line after the current expression."
  (interactive)
  (cond ((lispy-left-p)
         (save-excursion
           (forward-list)
           (newline)))
        ((lispy-right-p)
         (save-excursion (newline)))
        (t
         (save-excursion
           (lispy--out-forward 1)
           (newline)))))

(defun lispy-meta-return ()
  "Insert a new heading."
  (interactive)
  (let ((pt (point)))
    (cond ((lispy--in-comment-p)
           (end-of-line)
           (newline))
          ((and (lispy-bolp)
                (looking-at " *$"))
           (delete-region
            (line-beginning-position)
            (line-end-position)))
          (t
           (lispy-beginning-of-defun)
           (if (save-excursion
                 (forward-list 1)
                 (= (point) pt))
               (progn
                 (forward-list 1)
                 (newline))
             (newline)
             (backward-char 1)))))
  (insert ";;"
          (make-string (max (lispy-outline-level) 1)
                       ?\*)
          " ")
  (beginning-of-line))

(defun lispy-alt-line (&optional N)
  "Do a context-aware exit, then `newline-and-indent', N times.

Exit branches:

- When in the minibuffer, exit the minibuffer.
- When in a string, exit the string.
- When \")|\", do nothing.
- When \" |)\", exit the list and normalize it.
- When \"|(\", move to the other side of the list.
- When there's a \")\" on the current line before the point, move there.
- Otherwise, move to the end of the line.

This should generally be useful when generating new code.
If you find yourself with:

    (foo (bar (baz 1 2 \"3|\")))

calling this function consecutively, you will get a chance to add arguments
to all the functions, while maintaining the parens in a pretty state."
  (interactive "p")
  (setq N (or N 1))
  (when (bound-and-true-p abbrev-mode)
    (expand-abbrev))
  (lispy-dotimes N
    (cond ((> (minibuffer-depth) 0)
           (exit-minibuffer))
          ((lispy--in-string-p)
           (goto-char (cdr (lispy--bounds-string))))
          ((lispy-right-p))
          ((looking-at lispy-right)
           (when (eq (char-before) ?\ )
             (lispy-right 1)))
          ((lispy-left-p)
           (lispy-different))
          ((lispy-looking-back "^ +")
           (if (re-search-forward lispy-right (line-end-position) t)
               (backward-char 1)
             (move-end-of-line 1)))
          ((re-search-forward lispy-right (line-end-position) t)
           (backward-char 1))
          (t
           (move-end-of-line 1)))
    (newline-and-indent)))

;;* Globals: miscellanea
(defun lispy-string-oneline ()
  "Convert current string to one line."
  (interactive)
  (when (eq (char-before) ?\")
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
    (when (lispy-left-p)
      (forward-char 1))
    (iedit-mode 0)))

;;* Locals: navigation
;;** Occur
(defcustom lispy-occur-backend 'ivy
  "Method to navigate to a line with `lispy-occur'."
  :type '(choice
          (const :tag "Ivy" ivy)
          (const :tag "Helm" helm)))

(defvar lispy--occur-beg 1
  "Start position of the top level sexp during `lispy-occur'.")

(defvar lispy--occur-end 1
  "End position of the top level sexp during `lispy-occur'.")

(defun lispy--occur-candidates ()
  "Return the candidates for `lispy-occur'."
  (let ((bnd (save-excursion
               (unless (and (bolp)
                            (lispy-left-p))
                 (beginning-of-defun))
               (lispy--bounds-dwim)))
        (line-number -1)
        candidates)
    (setq lispy--occur-beg (car bnd))
    (setq lispy--occur-end (cdr bnd))
    (save-excursion
      (goto-char lispy--occur-beg)
      (while (< (point) lispy--occur-end)
        (push (format "%-3d %s"
                      (cl-incf line-number)
                      (buffer-substring
                       (line-beginning-position)
                       (line-end-position)))
              candidates)
        (forward-line 1)))
    (nreverse candidates)))

(defun lispy--occur-preselect ()
  "Initial candidate regex for `lispy-occur'."
  (format "^%d"
          (-
           (line-number-at-pos (point))
           (line-number-at-pos lispy--occur-beg))))

(defvar helm-input)
(declare-function helm "ext:helm")

(defun lispy--occur-action (x)
  "Goto line X for `lispy-occur'."
  (setq x (read x))
  (goto-char lispy--occur-beg)
  (let ((input (if (eq lispy-occur-backend 'helm)
                   helm-input
                 ivy-text))
        str-or-comment)
    (cond ((string= input "")
           (forward-line x)
           (back-to-indentation)
           (when (re-search-forward lispy-left (line-end-position) t)
             (goto-char (match-beginning 0))))

          ((setq str-or-comment
                 (progn
                   (forward-line x)
                   (re-search-forward (ivy--regex input)
                                      (line-end-position) t)
                   (lispy--in-string-or-comment-p)))
           (goto-char str-or-comment))

          ((re-search-backward lispy-left (line-beginning-position) t)
           (goto-char (match-beginning 0)))

          ((re-search-forward lispy-left (line-end-position) t)
           (goto-char (match-beginning 0)))

          (t
           (back-to-indentation)))))

(defun lispy-occur ()
  "Select a line within current top level sexp.
See `lispy-occur-backend' for the selection back end."
  (interactive)
  (swiper--init)
  (cond ((eq lispy-occur-backend 'helm)
         (require 'helm)
         (add-hook 'helm-move-selection-after-hook
                   #'lispy--occur-update-input-helm)
         (add-hook 'helm-update-hook
                   #'lispy--occur-update-input-helm)
         (unwind-protect
              (helm :sources
                    `((name . "this defun")
                      (candidates . ,(lispy--occur-candidates))
                      (action . lispy--occur-action)
                      (match-strict .
                                    (lambda (x)
                                      (ignore-errors
                                        (string-match
                                         (ivy--regex helm-input) x)))))
                    :preselect (lispy--occur-preselect)
                    :buffer "*lispy-occur*")
           (swiper--cleanup)
           (remove-hook 'helm-move-selection-after-hook
                        #'lispy--occur-update-input-helm)
           (remove-hook 'helm-update-hook
                        #'lispy--occur-update-input-helm)))
        ((eq lispy-occur-backend 'ivy)
         (unwind-protect
              (ivy-read "pattern: "
                        (lispy--occur-candidates)
                        :preselect (lispy--occur-preselect)
                        :require-match t
                        :update-fn (lambda ()
                                     (lispy--occur-update-input
                                      ivy-text ivy--current))
                        :action #'lispy--occur-action)
           (swiper--cleanup)
           (when (null ivy-exit)
             (goto-char swiper--opoint))))
        (t
         (error "Bad `lispy-occur-backend': %S" lispy-occur-backend))))

(defun lispy--occur-update-input-helm ()
  "Update selection for `lispy-occur' using `helm' back end."
  (lispy--occur-update-input
   helm-input
   (buffer-substring-no-properties
    (line-beginning-position)
    (line-end-position))))

(defun lispy--occur-update-input (input str)
  "Update selection for `ivy-occur'.
INPUT is the current input text.
STR is the full current candidate."
  (swiper--cleanup)
  (let ((re (ivy--regex input))
        (num (if (string-match "^[0-9]+" str)
                 (string-to-number (match-string 0 str))
               0)))
    (with-selected-window swiper--window
      (goto-char lispy--occur-beg)
      (when (cl-plusp num)
        (forward-line num)
        (unless (<= (point) lispy--occur-end)
          (recenter)))
      (let ((ov (make-overlay (line-beginning-position)
                              (1+ (line-end-position)))))
        (overlay-put ov 'face 'swiper-line-face)
        (overlay-put ov 'window swiper--window)
        (push ov swiper--overlays))
      (swiper--add-overlays
       re
       lispy--occur-beg
       lispy--occur-end))))

;;* Locals: Paredit transformations
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
  (when (lispy-looking-back "\\s_")
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
                    (lispy-looking-back "\\s_")))
              (lispy--sub-slurp-forward arg)
            (lispy-dotimes arg
              (forward-sexp 1)))
        (if (or (lispy-looking-back "\\s_")
                (save-excursion
                  (goto-char (region-end))
                  (looking-at "\\s_")))
            (lispy--sub-slurp-backward arg)
          (lispy-dotimes arg
            (forward-sexp -1))))
    (if (lispy-right-p)
        (lispy-dotimes arg
          (lispy--slurp-forward))
      (if (lispy-left-p)
          (lispy-dotimes arg
            (lispy--slurp-backward))))
    (lispy--reindent)))

(defun lispy-down-slurp ()
  "Move current sexp or region into the next sexp."
  (interactive)
  (let ((bnd (lispy--bounds-dwim))
        (leftp (lispy--leftp))
        (regionp (region-active-p))
        deactivate-mark)
    (when (lispy-left-p)
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
        (endp (or (lispy-right-p)
                  (and (region-active-p) (= (point) (region-end)))))
        p-beg p-end
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
                (one-symbolp (lispy--symbolp str)))
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

        ((lispy-right-p)
         (lispy-dotimes arg
           (lispy--barf-backward)))

        ((lispy-left-p)
         (lispy-dotimes arg
           (lispy--barf-forward)))))

(defun lispy-splice (arg)
  "Splice ARG sexps into containing list."
  (interactive "p")
  (lispy-dotimes arg
    (let ((bnd (lispy--bounds-dwim))
          (deactivate-mark nil))
      (cond ((region-active-p)
             (save-excursion
               (goto-char (cdr bnd))
               (re-search-backward lispy-right)
               (delete-region (point) (cdr bnd)))
             (save-excursion
               (goto-char (car bnd))
               (re-search-forward lispy-left)
               (delete-region (car bnd) (point))))

            ((lispy-left-p)
             (save-excursion
               (goto-char (cdr bnd))
               (delete-char -1))
             (lispy--delete-quote-garbage)
             (delete-char 1)
             (lispy-forward 1)
             (lispy-backward 1))

            ((lispy-right-p)
             (setq bnd (lispy--bounds-dwim))
             (delete-char -1)
             (goto-char (car bnd))
             (let ((pt (point)))
               (re-search-forward lispy-left nil t)
               (delete-region pt (point)))
             (lispy-backward 1)
             (forward-list))

            (t
             (setq bnd (lispy--bounds-list))
             (save-excursion
               (goto-char (cdr bnd))
               (delete-char -1))
             (save-excursion
               (goto-char (car bnd))
               (delete-char 1)))))))

(defun lispy-reverse ()
  "Reverse the current list or region selection."
  (interactive)
  (let* ((leftp (lispy--leftp))
         (bnd (lispy--bounds-dwim))
         (expr (lispy--read (format "(%s)" (lispy--string-dwim bnd))))
         (deactivate-mark nil))
    (delete-region (car bnd) (cdr bnd))
    (if (eq (length expr) 1)
        (lispy--insert (nreverse (car expr)))
      (lispy--insert (nreverse expr))
      (lispy-splice 1))
    (when leftp
      (lispy-different))))

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

          ((lispy-left-p)
           (lispy--out-forward 1)
           (backward-char 1)
           (set-mark (point))
           (goto-char pt))

          ((lispy-right-p)
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
  (let ((deactivate-mark nil))
    (if (and (save-excursion
               (lispy--out-forward (1+ arg)))
             (save-excursion
               (lispy--out-backward (1+ arg))))
        (let (beg end)
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
           (lispy--out-forward 1)
           (lispy--swap-regions (cons beg end)
                                (cons (point) (point)))
           (ignore-errors
             (lispy-different))
           (lispy--reindent (1+ arg))))
      (error "Not enough depth to convolute"))))

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
  (funcall lispy-repeat--command))

(defun lispy-join ()
  "Join sexps."
  (interactive)
  (let ((pt (point))
        bnd)
    (cond ((lispy-right-p)
           (when (lispy-forward 1)
             (backward-list)
             (delete-char 1)
             (goto-char pt)
             (backward-delete-char 1)
             (lispy--out-forward 1)
             (lispy--reindent 1)))
          ((lispy-left-p)
           (when (lispy-backward 1)
             (forward-list)
             (backward-delete-char 1)
             (goto-char (1- pt))
             (delete-char 1)
             (lispy-save-excursion
               (forward-char 1)
               (lispy-left 2)
               (lispy--normalize-1))))
          ((and (setq bnd (lispy--bounds-string))
                (or (save-excursion
                      (goto-char (car bnd))
                      (skip-chars-backward " \t\n")
                      (when (eq (char-before) ?\")
                        (delete-region (1- (point))
                                       (1+ (car bnd)))
                        t))
                    (save-excursion
                      (goto-char (cdr bnd))
                      (skip-chars-forward " \t\n")
                      (when (looking-at "\"")
                        (delete-region (1- (cdr bnd))
                                       (1+ (point)))
                        t))))))))

(defun lispy-split ()
  "Split sexps."
  (interactive)
  (let (bnd)
    (cond ((lispy--in-comment-p)
           (indent-new-comment-line))
          ((and (setq bnd (lispy--bounds-string))
                (not (= (point) (car bnd))))
           (insert "\"\"")
           (when (eolp)
             (delete-char 1))
           (backward-char)
           (newline-and-indent))
          (t
           (when (save-excursion
                   (lispy--out-forward 1))
             (insert ")(")
             (backward-char 2)
             (lispy-right 1))
           (newline-and-indent)
           (when (lispy-left-p)
             (indent-sexp))))))

;;* Locals: more transformations
(defun lispy-move-up (arg)
  "Move current expression up ARG times.  Don't exit parent list."
  (interactive "p")
  (let ((at-start (lispy--leftp)))
    (unless at-start (lispy-different))
    (if (region-active-p)
        (if (= arg 1)
            (let ((pt (point))
                  (bnd1 (lispy--bounds-dwim))
                  (bnd0 (save-excursion
                          (deactivate-mark)
                          (if (ignore-errors (up-list) t)
                              (lispy--bounds-dwim)
                            (cons (point-min) (point-max)))))
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
                      (when (memq (char-after) '(?\) ?\] ?\}))
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
                  (bnd1 (lispy--bounds-dwim))
                  (bnd0 (save-excursion
                          (deactivate-mark)
                          (if (ignore-errors (up-list) t)
                              (lispy--bounds-dwim)
                            (cons (point-min) (point-max)))))
                  bnd2)
              (goto-char (cdr bnd1))
              (if (re-search-forward "[^ \t\n]" (1- (cdr bnd0)) t)
                  (progn
                    (deactivate-mark)
                    (if (lispy--in-comment-p)
                        (setq bnd2 (lispy--bounds-comment))
                      (when (memq (char-before) '(?\( ?\" ?\[ ?\{))
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
                    (lispy-bolp))
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
              (setq deactivate-mark nil)
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
                    (lispy-bolp))
               (delete-blank-lines))
              ((looking-at "\\([\n ]+\\)[^\n ;]")
               (delete-region (match-beginning 1)
                              (match-end 1))))
        (lispy--out-backward 1)
        (deactivate-mark)
        (lispy-different)
        (newline-and-indent)
        (setq pt (point))
        (insert str)
        (indent-region pt (point))
        (if regionp
            (progn
              (setq deactivate-mark nil)
              (set-mark pt)
              (when leftp
                (exchange-point-and-mark)))
          (when leftp
            (lispy-different)))))))

(defun lispy-clone (arg)
  "Clone sexp ARG times.
When the sexp is top-level, insert an additional newline."
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
                        (newline)
                        (insert str)
                        (indent-for-tab-command)))))
               (if (= (point) (region-end))
                   (doit)
                 (exchange-point-and-mark)
                 (doit)
                 (exchange-point-and-mark)))))
          ((lispy-left-p)
           (goto-char (car bnd))
           (if (and (bolp) (looking-at "(defun"))
               (lispy-dotimes arg
                 (insert str)
                 (newline)
                 (newline))
             (lispy-dotimes arg
               (insert str)
               (newline-and-indent)))
           (goto-char pt))
          ((lispy-right-p)
           (if (save-excursion
                 (backward-list)
                 (and (bolp) (looking-at "(defun")))
               (lispy-dotimes arg
                 (newline)
                 (newline-and-indent)
                 (insert str))
             (lispy-dotimes arg
               (newline-and-indent)
               (insert str))))
          (t
           (error "Unexpected")))))

(defvar lispy--oneline-comments nil
  "Collect comments for `lispy--oneline'.")

(defun lispy-mapcan-tree (func expr)
  "Reduce with FUNC all lists in EXPR."
  (cond ((null expr)
         nil)
        ((listp expr)
         (funcall func
                  (lispy-mapcan-tree func (car expr))
                  (lispy-mapcan-tree func (cdr expr))))
        (t
         expr)))

(defun lispy--oneline (expr &optional ignore-comments)
  "Remove newlines from EXPR.
When IGNORE-COMMENTS is not nil, don't remove comments.
Instead keep them, with a newline after each comment."
  (if (vectorp expr)
      (apply #'vector (lispy--oneline (mapcar #'identity expr)))
    (lispy-mapcan-tree
     (lambda (x y)
       (cond ((equal x '(ly-raw newline))
              y)
             ((lispy--raw-comment-p x)
              (if (null ignore-comments)
                  (progn
                    (push x lispy--oneline-comments)
                    y)
                (if (equal (car y) '(ly-raw newline))
                    (cons x y)
                  `(,x (ly-raw newline) ,@y))))
             ((and (lispy--raw-string-p x)
                   (null ignore-comments))
              (cons `(ly-raw string ,(replace-regexp-in-string "\n" "\\\\n" (caddr x)))
                    y))
             (t
              (cons x y))))
     expr)))

(defun lispy-oneline ()
  "Squeeze current sexp into one line.
Comments will be moved ahead of sexp."
  (interactive)
  (unless (or (lispy-left-p)
              (lispy-right-p))
    (lispy--out-backward 1))
  (let* ((bnd (lispy--bounds-dwim))
         (str (lispy--string-dwim bnd))
         (from-left (lispy-left-p))
         expr)
    (delete-region (car bnd) (cdr bnd))
    (when (region-active-p)
      (deactivate-mark))
    (setq lispy--oneline-comments nil)
    (if (setq expr (ignore-errors
                     (lispy--oneline
                      (lispy--read str))))
        (progn
          (mapc (lambda (x)
                  (lispy--insert x)
                  (newline))
                (nreverse lispy--oneline-comments))
          (lispy--insert expr))
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
    (when from-left
      (backward-list))))

(defun lispy-multiline (&optional arg)
  "Spread current sexp over multiple lines.
When ARG is `fill', do nothing for short expressions."
  (interactive "p")
  (unless (or (lispy-left-p)
              (lispy-right-p))
    (lispy--out-backward 1))
  (lispy-from-left
   (let* ((bnd (lispy--bounds-list))
          (str (lispy--string-dwim bnd))
          (plain-expr (read str))
          (expr (lispy--read str))
          res)
     (unless (and (eq arg 'fill)
                  (< (length str) 80))
       (unless (listp plain-expr)
         (setq plain-expr nil))
       (if (or (cl-some #'listp plain-expr)
               (member '(ly-raw newline) expr))
           (let ((pt (point)))
             (lispy-forward 1)
             (while (and (lispy-flow 1) (> (point) pt))
               (unless (looking-at "\]\\|)\\|\n")
                 (when (looking-at " *")
                   (replace-match "\n")
                   (backward-char 1))))
             (goto-char pt)
             (indent-sexp))
         (delete-region (car bnd) (cdr bnd))
         (setq res
               (butlast
                (cl-mapcan (lambda (y)
                             (if (memq y '(ly-raw clojure-map clojure-set))
                                 (list y)
                               (list y '(ly-raw newline))))
                           (lispy--read str))))
         (when (vectorp expr)
           (setq res (apply #'vector res)))
         (lispy--insert res))))))

(defvar-local lispy--multiline-take-3
    '(defvar defun defmacro defcustom defgroup defvar-local declare-function
      define-key nth throw define-error defadvice defhydra defsubst)
  "List of constructs for which the first 3 elements are on the first line.")

(setq-mode-local
 clojure-mode
 lispy--multiline-take-3 '())

(defvar lispy--multiline-take-3-arg
  '(defun defmacro declare-function define-error defadvice defhydra defsubst)
  "List of constructs for which the first 3 elements are on the first line.
The third one is assumed to be the arglist and will not be changed.")

(defvar-local lispy--multiline-take-2
    '(defface define-minor-mode
      condition-case while incf car
      cdr > >= < <= /= = eq equal incf
      decf cl-incf cl-decf catch
      require provide setq cons when
      if unless interactive assq delq
      assoc declare lambda remq
      make-variable-buffer-local
      bound-and-true-p
      called-interactively-p
      lispy-dotimes cond case cl-case
      defalias 1+ 1- dotimes dolist boundp fboundp macrop
      null consp oddp zerop plusp minusp kbd
      not pop listp or and)
  "List of constructs for which the first 2 elements are on the first line.")

(setq-mode-local
 clojure-mode
 lispy--multiline-take-2 '(loop recur let for fn def defn ns if -> ->>
                           + +' - -' * *' / > >= < <= = ==
                           or and not
                           assoc! assoc assoc-in concat))

(defvar lispy--multiline-take-2-arg '(declare lambda
                                      make-variable-buffer-local
                                      bound-and-true-p
                                      called-interactively-p
                                      lispy-dotimes dotimes)
  "List of constructs for which the first 2 elements are on the first line.
The second one will not be changed.")

(defun lispy-interleave (x lst)
  "Insert X in between each element of LST.
Don't insert X when it's already there."
  (let ((res (list (pop lst)))
        item)
    (while lst
      (unless (equal (car res) x)
        (push x res))
      (unless (equal (car res) (setq item (pop lst)))
        (push item res)))
    (nreverse res)))

(defcustom lispy-multiline-threshold 32
  "Don't multiline expresssions shorter than this when printed as a string.")

(defun lispy--multiline-1 (expr &optional quoted)
  "Transform a one-line EXPR into a multi-line.
When QUOTED is not nil, assume that EXPR is quoted and ignore some rules."
  (cond ((vectorp expr)
         (apply #'vector
                (lispy--multiline-1
                 (mapcar #'identity expr))))
        ((not (listp expr))
         expr)
        ((and lispy-multiline-threshold
              (< (length (lispy--prin1-to-string
                          expr 0 'emacs-lisp-mode))
                 lispy-multiline-threshold))
         expr)
        (t
         (let ((res nil)
               elt)
           (while expr
             (setq elt (pop expr))
             (cond
               ((eq elt 'ly-raw)
                (cl-case (car expr)
                  (empty
                   (setq res '(ly-raw empty)))
                  (raw
                   (setq res (cons elt expr)))
                  (dot
                   (setq res (cons elt expr)))
                  (newline
                   (setq res '(ly-raw newline)))
                  (comment
                   (setq res (cons elt expr)))
                  (string
                   (setq res
                         `(ly-raw string
                                  ,(replace-regexp-in-string
                                    "\\(?:[^\\]\\|^\\)\\(\\\\n\\)" "\n" (cadr expr) nil t 1))))
                  (t (unless (= (length expr) 2)
                       (error "Unexpected expr: %S" expr))
                     (unless (null res)
                       (error "Stray ly-raw in %S" expr))
                     (setq res (list 'ly-raw (car expr)
                                     (lispy--multiline-1
                                      (cadr expr)
                                      (car (memq (car expr) '(quote \` clojure-lambda))))))))
                (setq expr nil))
               ((vectorp elt)
                (push
                 (apply #'vector
                        (lispy--multiline-1
                         (mapcar #'identity elt)))
                 res)
                (push '(ly-raw newline) res))
               ((equal elt '(ly-raw dot))
                (when (equal (car res) '(ly-raw newline))
                  (pop res))
                (push elt res))
               ((equal elt '(ly-raw clojure-comma))
                ;; two sexps without newlines, then a comma with a newline
                (when (equal (car res) '(ly-raw newline))
                  (pop res))
                (when (equal (cadr res) '(ly-raw newline))
                  (setq res
                        (cons (car res)
                              (cddr res))))
                (push elt res)
                (push '(ly-raw newline) res))
               ((and (not quoted) (memq elt lispy--multiline-take-3))
                (push elt res)
                ;; name
                (when expr
                  (push (pop expr) res))
                ;; value
                (when expr
                  (if (memq elt lispy--multiline-take-3-arg)
                      (push (pop expr) res)
                    (push (car (lispy--multiline-1 (list (pop expr)))) res)))
                (push '(ly-raw newline) res))
               ((and (not quoted) (memq elt lispy--multiline-take-2))
                (push elt res)
                (when (memq elt lispy--multiline-take-2-arg)
                  (push (pop expr) res)
                  (push '(ly-raw newline) res)))
               ((and (memq elt '(let let*))
                     expr
                     (listp (car expr))
                     (listp (cdar expr)))
                (push elt res)
                (let ((body (pop expr)))
                  (push
                   (lispy-interleave
                    '(ly-raw newline)
                    (mapcar
                     (lambda (x)
                       (if (and (listp x)
                                (not (eq (car x) 'ly-raw)))
                           (cons (car x)
                                 (lispy--multiline-1 (cdr x)))
                         x))
                     body))
                   res))
                (push '(ly-raw newline) res))
               ((keywordp elt)
                (push elt res))
               ((not (listp elt))
                (push elt res)
                (unless (and (numberp elt) (eq quoted 'clojure-lambda))
                  (push '(ly-raw newline) res)))
               (t
                (setq elt (lispy--multiline-1 elt))
                (if (equal elt '(ly-raw newline))
                    (unless (equal elt (car res))
                      (push elt res))
                  (push elt res)
                  (push '(ly-raw newline) res)))))
           (cond ((equal (car res) 'ly-raw)
                  res)
                 ((equal (car res) '(ly-raw newline))
                  (if (and (cdr res)
                           (lispy--raw-comment-p (cadr res)))
                      (nreverse res)
                    (nreverse (cdr res))))
                 (t
                  (nreverse res)))))))

(defun lispy-alt-multiline (&optional silent)
  "Spread current sexp over multiple lines.
When SILENT is non-nil, don't issue messages."
  (interactive)
  (unless (or (lispy-left-p)
              (lispy-right-p))
    (lispy--out-backward 1))
  (let* ((bnd (lispy--bounds-dwim))
         (str (lispy--string-dwim bnd))
         (expr (lispy--read str))
         (expr-o (lispy--oneline expr t))
         (expr-m (lispy--multiline-1 expr-o))
         (leftp (lispy--leftp)))
    (cond ((equal expr expr-m)
           (unless silent
             (message "No change")))
          ((and (memq major-mode lispy-elisp-modes)
                (not
                 (condition-case nil
                     (equal (read str)
                            (read (lispy--prin1-to-string
                                   expr-m 0 major-mode)))
                   (error
                    (lispy-complain "Got an unreadable expr (probably overlay)")
                    t))))
           (error "Got a bad transform: %S" expr-m))
          (t
           (delete-region (car bnd) (cdr bnd))
           (lispy--insert expr-m)
           (when leftp
             (backward-list))))))

(defvar lispy-do-fill nil
  "If t, `lispy-insert-1' will try to fill.")

(defun lispy-fill ()
  "Fill current expression."
  (interactive)
  (if (or (lispy-left-p)
          (lispy-right-p))
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
              ((lispy-left-p)
               (setq bnd (lispy--bounds-dwim))
               (lispy-down 1)
               (comment-region (car bnd) (cdr bnd))
               (when (or (lispy--in-string-or-comment-p)
                         (looking-at ";"))
                 (lispy--out-backward 1)))
              ((bolp)
               (insert ";;"))
              ((lispy-right-p)
               (comment-dwim nil)
               (insert " "))
              ((eolp)
               (comment-dwim nil))
              ((lispy-bolp)
               (comment-dwim nil))
              ((setq bnd (save-excursion
                           (and (lispy--out-forward 1)
                                (point))))
               (let ((pt (point)))
                 (if (re-search-forward "\n" bnd t)
                     (if (= (count-matches lispy-left pt (point))
                            (count-matches lispy-right pt (point)))
                         (progn (comment-region pt (point))
                                (lispy-forward 1)
                                (lispy-backward 1))
                       (goto-char pt)
                       (re-search-forward lispy-left bnd t)
                       (backward-char 1)
                       (forward-list 1)
                       (comment-region pt (point))
                       (lispy-forward 1)
                       (lispy-backward 1))
                   (comment-region (point) (1- bnd))
                   (lispy--out-backward 1))))
              (t
               (self-insert-command 1)))))))

(defun lispy--quote-string (str &optional quote-newlines)
  "Quote the quotes and backslashes in STR.
Quote the newlines if QUOTE-NEWLINES is t."
  (setq str (replace-regexp-in-string "\\\\" "\\\\\\\\" str))
  (setq str (replace-regexp-in-string "\"" "\\\\\"" str))
  (if quote-newlines
      (replace-regexp-in-string "\n" "\\\\n" str)
    str))

(defun lispy-stringify (&optional arg)
  "Transform current sexp into a string.
Quote newlines if ARG isn't 1."
  (interactive "p")
  (setq arg (or arg 1))
  (let* ((bnd (lispy--bounds-dwim))
         (pt (point))
         (str-1 (buffer-substring-no-properties (car bnd) pt))
         (str-2 (buffer-substring-no-properties pt (cdr bnd)))
         (regionp (region-active-p))
         (leftp (lispy--leftp))
         deactivate-mark)
    (when (and regionp leftp)
      (exchange-point-and-mark))
    (if (lispy--in-string-p)
        (if regionp
            (progn
              (insert "\\\"")
              (exchange-point-and-mark)
              (insert "\\\"")
              (backward-char 2)
              (unless leftp
                (exchange-point-and-mark)))
          (lispy-complain "can't do anything useful here"))
      (deactivate-mark)
      (setq str-1 (lispy--quote-string str-1 (/= arg 1)))
      (setq str-2 (lispy--quote-string str-2 (/= arg 1)))
      (delete-region (car bnd) (cdr bnd))
      (insert "\"" str-1)
      (save-excursion (insert str-2 "\""))
      (when regionp
        (unless (looking-at "\"")
          (backward-char 1))
        (lispy-mark-symbol)
        (if (and leftp (= (point) (region-end)))
            (exchange-point-and-mark))))))

(defun lispy-unstringify ()
  "Unquote string at point."
  (interactive)
  (if (region-active-p)
      (if (lispy--string-markedp)
          (let (deactivate-mark
                (str (lispy--string-dwim))
                (leftp (lispy--leftp)))
            (delete-active-region)
            (set-mark (point))
            (insert (read str))
            (when leftp
              (lispy-different)))
        (lispy-complain "the current region isn't a string"))
    (let* ((bnd (lispy--bounds-string))
           (str (lispy--string-dwim bnd))
           (str-1 (concat (substring str 0 (- (point) (car bnd))) "\""))
           (offset (length (read str-1))))
      (delete-region (car bnd) (cdr bnd))
      (save-excursion (insert (read str)))
      (forward-char offset))))

(defun lispy-teleport (arg)
  "Move ARG sexps into a sexp determined by `lispy-ace-paren'."
  (interactive "p")
  (let ((beg (point))
        end endp regionp)
    (cond ((region-active-p)
           (setq endp (= (point) (region-end)))
           (setq regionp t)
           (lispy-different))
          ((lispy-left-p)
           (unless (lispy-dotimes arg
                     (forward-list 1))
             (error "Unexpected")))
          ((lispy-right-p)
           (setq endp t)
           (unless (lispy-dotimes arg
                     (backward-list arg))
             (error "Unexpected")))
          (t
           (error "Unexpected")))
    (setq end (point))
    (goto-char beg)
    (lispy-ace-paren)
    (forward-char 1)
    (unless (looking-at "(")
      (ignore-errors
        (forward-sexp)))
    (backward-char 1)
    (lispy--teleport beg end endp regionp)))

;;* Locals: tags
(defun lispy-goto (&optional arg)
  "Jump to symbol within files in current directory.
When ARG isn't nil, call `lispy-goto-projectile' instead."
  (interactive "p")
  (deactivate-mark)
  (lispy--select-candidate
   (mapcar #'lispy--format-tag-line
           (cl-case arg
             (1
              (lispy--fetch-tags))
             (2
              (let ((lispy-force-reparse t))
                (lispy--fetch-tags)))
             (t
              (lispy--fetch-tags-projectile))))
   #'lispy--action-jump))

(defun lispy-goto-recursive ()
  "Jump to symbol within files in current directory and its subdiretories."
  (interactive)
  (deactivate-mark)
  (let ((candidates (lispy--fetch-tags-recursive)))
    (lispy--select-candidate
     (if (> (length candidates) 30000)
         candidates
       (mapcar #'lispy--format-tag-line candidates))
     #'lispy--action-jump)))

(defun lispy-goto-local (&optional arg)
  "Jump to symbol within current file.
When ARG is non-nil, force a reparse."
  (interactive "P")
  (deactivate-mark)
  (let ((lispy-force-reparse arg))
    (lispy--select-candidate
     (mapcar #'lispy--format-tag-line
             (lispy--fetch-tags (list (buffer-file-name))))
     #'lispy--action-jump)))

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
  (lispy-ace-symbol arg)
  (call-interactively 'lispy-goto-symbol))

(when (version< emacs-version "25.1")
  (eval-after-load 'etags
    '(add-to-list 'byte-compile-not-obsolete-vars 'find-tag-marker-ring)))

(declare-function slime-edit-definition "ext:slime")
(declare-function lispy--clojure-resolve "le-clojure")
(declare-function lispy--clojure-jump "le-clojure")
(declare-function geiser-edit-symbol "geiser-edit")
(defun lispy-goto-symbol (symbol)
  "Go to definition of SYMBOL.
SYMBOL is a string."
  (interactive (list (or (thing-at-point 'symbol)
                         (lispy--current-function))))
  (let (rsymbol)
    (deactivate-mark)
    (ring-insert find-tag-marker-ring (point-marker))
    (cond ((and (memq major-mode lispy-elisp-modes)
                (setq symbol (intern-soft symbol)))
           (cond ((and current-prefix-arg (boundp symbol))
                  (find-variable symbol))
                 ((fboundp symbol)
                  (condition-case nil
                      (find-function symbol)
                    (error
                     (goto-char (point-min))
                     (if (re-search-forward (format "^(def.*%S" symbol) nil t)
                         (move-beginning-of-line 1)
                       (lispy-complain
                        (format "Don't know where `%S' is defined" symbol))
                       (pop-tag-mark)))))
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
                  (lispy--clojure-jump rsymbol))
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
           (slime-edit-definition symbol))
          ((eq major-mode 'scheme-mode)
           (require 'geiser)
           (geiser-edit-symbol (make-symbol symbol)))))
  ;; in case it's hidden in an outline
  (lispy--ensure-visible))

;;* Locals: dialect-related
(defun lispy-eval (arg)
  "Eval last sexp.
When ARG is 2, insert the result as a comment."
  (interactive "p")
  (if (eq arg 2)
      (lispy-eval-and-comment)
    (save-excursion
      (unless (or (lispy-right-p) (region-active-p))
        (lispy-forward 1))
      (message
       (replace-regexp-in-string
        "%" "%%" (lispy--eval (lispy--string-dwim) t))))))

(defvar lispy-do-pprint nil
  "Try a pretty-print when this ins't nil.")

(defun lispy-eval-and-insert (&optional arg)
  "Eval last sexp and insert the result.

When ARG isn't nil, try to pretty print the sexp."
  (interactive "P")
  (let ((lispy-do-pprint arg))
    (cl-labels
        ((doit ()
           (unless (or (lispy-right-p) (region-active-p))
             (lispy-forward 1))
           (let ((str (lispy--eval (lispy--string-dwim))))
             (newline-and-indent)
             (insert str)
             (when (lispy-right-p)
               (lispy-alt-multiline t)))))
      (if (lispy-left-p)
          (save-excursion
            (doit))
        (doit)))))

(defun lispy-eval-and-comment ()
  "Eval last sexp and insert the result as a comment."
  (interactive)
  (let ((str (lispy--eval (lispy--string-dwim)))
        bnd)
    (save-excursion
      (when (lispy-left-p)
        (lispy-different))
      (if (not (looking-at "\n;+ ?=>"))
          (newline)
        (goto-char (1+ (match-beginning 0)))
        (setq bnd (lispy--bounds-comment))
        (delete-region (car bnd) (cdr bnd)))
      (save-restriction
        (narrow-to-region (point) (point))
        (insert str)
        (if (lispy-right-p)
            (progn
              (lispy-alt-multiline t)
              (goto-char (point-min))
              (insert "=>\n"))
          (goto-char (point-min))
          (insert "=> "))
        (comment-region (point-min) (point-max))))))

(defun lispy-eval-and-replace ()
  "Eval last sexp and replace it with the result."
  (interactive)
  (let* ((leftp (lispy--leftp))
         (bnd (lispy--bounds-dwim))
         (str (lispy--string-dwim bnd)))
    (delete-region (car bnd) (cdr bnd))
    (deactivate-mark)
    (insert (lispy--eval str))
    (unless (or (lispy-left-p)
                (lispy-right-p))
      (lispy--out-backward 1))
    (when (and leftp (lispy-right-p))
      (lispy-different))))

(defconst lispy--eval-cond-msg
  (format "%s: nil" (propertize "cond" 'face 'font-lock-keyword-face))
  "Message to echo when the current `cond' branch is nil.")

(defvar lispy-eval-other--window nil
  "Target window for `lispy-eval-other-window'.")

(defvar lispy-eval-other--buffer nil
  "Target buffer for `lispy-eval-other-window'.")

(defvar lispy-eval-other--cfg nil
  "Last window configuration for `lispy-eval-other-window'.")

(defun lispy-eval--last-live-p ()
  "Return t if the last eval window is still live with same buffer."
  (and (window-live-p
        lispy-eval-other--window)
       (equal (window-buffer
               lispy-eval-other--window)
              lispy-eval-other--buffer)
       (equal (cl-mapcan #'window-list (frame-list))
              lispy-eval-other--cfg)))

(defvar lispy--eval-sym nil
  "Last set `dolist' sym.")

(defvar lispy--eval-data nil
  "List data for a `dolist' sym.")

(declare-function aw-select "ext:ace-window")
(defvar aw-dispatch-always)

(defun lispy--dolist-item-expr (expr)
  "Produce an eval expression for dolist-type EXPR.
EXPR is (SYM LST).
SYM will take on each value of LST with each eval."
  (let ((sym (car expr)))
    (unless (eq sym lispy--eval-sym)
      (setq lispy--eval-sym sym)
      (setq lispy--eval-data
            (lispy--eval-elisp-form (cadr expr) lexical-binding)))
    (if lispy--eval-data
        (let* ((popped (pop lispy--eval-data))
               (popped (if (symbolp popped)
                           `(quote ,popped)
                         popped)))
          (set sym popped))
      (setq lispy--eval-data
            (lispy--eval-elisp-form (cadr expr) lexical-binding))
      (set sym nil))))

(defun lispy-eval-other-window (&optional arg)
  "Eval current expression in the context of other window.
In case the point is on a let-bound variable, add a `setq'.
When ARG is non-nil, force select the window."
  (interactive "P")
  (require 'ace-window)
  (let* ((expr (lispy--setq-expression))
         (aw-dispatch-always nil)
         (target-window
          (if (and (null arg) (lispy-eval--last-live-p))
              lispy-eval-other--window
            (if (setq lispy-eval-other--window
                      (aw-select " Ace - Eval in Window"))
                (progn
                  (setq lispy-eval-other--buffer
                        (window-buffer lispy-eval-other--window))
                  (setq lispy-eval-other--cfg
                        (cl-mapcan #'window-list (frame-list)))
                  lispy-eval-other--window)
              (setq lispy-eval-other--buffer nil)
              (setq lispy-eval-other--cfg nil)
              (selected-window))))
         res)
    (with-selected-window target-window
      (setq res (lispy--eval-elisp-form expr lexical-binding))
      (if (equal res lispy--eval-cond-msg)
          (message res)
        (message "%S" res)))))

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
  (cond ((and (looking-at "^(")
              (memq last-command
                    '(lispy-beginning-of-defun
                      special-lispy-beginning-of-defun)))
         (if (consp lispy-bof-last-point)
             (progn
               (goto-char (car lispy-bof-last-point))
               (set-mark (cdr lispy-bof-last-point)))
           (goto-char lispy-bof-last-point)))
        ((looking-at "^(")
         (setq lispy-bof-last-point (point)))
        (t
         (if (region-active-p)
             (progn
               (setq lispy-bof-last-point
                     (cons (point) (mark)))
               (deactivate-mark))
           (setq lispy-bof-last-point (point)))
         (beginning-of-defun arg))))

;;* Locals: avy-jump
(declare-function avy--regex-candidates "avy")
(declare-function avy--goto "avy")
(declare-function avy--process "avy")
(declare-function avy--overlay-post "avy")

(defun lispy-ace-char ()
  "Visually select a char within the current defun."
  (interactive)
  (let ((avy-keys lispy-avy-keys))
    (avy--with-avy-keys lispy-ace-char
      (lispy--avy-do
       (string (read-char "Char: "))
       (save-excursion
         ;; `beginning-of-defun' won't work, since it can change sexp
         (lispy--out-backward 50)
         (lispy--bounds-dwim))
       (lambda () t)
       lispy-avy-style-char))))

(defun lispy-ace-paren ()
  "Jump to an open paren within the current defun."
  (interactive)
  (lispy--remember)
  (deactivate-mark)
  (let ((avy-keys lispy-avy-keys))
    (avy--with-avy-keys lispy-ace-paren
      (lispy--avy-do
       lispy-left
       (save-excursion
         (lispy--out-backward 50)
         (lispy--bounds-dwim))
       (lambda () (not (lispy--in-string-or-comment-p)))
       lispy-avy-style-paren))))

(defun lispy-ace-symbol (arg)
  "Jump to a symbol withing the current sexp and mark it.
Sexp is obtained by exiting the list ARG times."
  (interactive "p")
  (lispy--out-forward
   (if (region-active-p)
       (progn (deactivate-mark) arg)
     (1- arg)))
  (let ((avy-keys lispy-avy-keys))
    (avy--with-avy-keys lispy-ace-symbol
      (let ((avy--overlay-offset (if (eq lispy-avy-style-symbol 'at) -1 0)))
        (lispy--avy-do
         "[([{ ]\\(?:\\sw\\|\\s_\\|[\"'`#~,@]\\)"
         (lispy--bounds-dwim)
         (lambda () (or (not (lispy--in-string-or-comment-p))
                   (lispy-looking-back ".\"")))
         lispy-avy-style-symbol))))
  (unless (eq (char-after) ?\")
    (forward-char 1))
  (lispy-mark-symbol))

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
    (let ((avy--overlay-offset (if (eq lispy-avy-style-symbol 'at) 0 1))
          (avy-keys lispy-avy-keys))
      (avy--with-avy-keys 'lispy-ace-subword
        (lispy--avy-do
         "[([{ -]\\(?:\\sw\\|\\s_\\|\\s(\\|[\"'`#]\\)"
         (lispy--bounds-dwim)
         (lambda () (or (not (lispy--in-string-or-comment-p))
                   (lispy-looking-back ".\"")))
         lispy-avy-style-symbol)))
    (skip-chars-forward "-([{ `'#") (mark-word)))

(defun lispy--avy-do (regex bnd filter style)
  "Visually select a match to REGEX within BND.
Filter out the matches that don't match FILTER.
Use STYLE function to update the overlays."
  (lispy--recenter-bounds bnd)
  (let* ((cands (avy--regex-candidates
                 regex
                 (car bnd) (cdr bnd)
                 filter)))
    (dolist (x cands)
      (when (> (- (cdar x) (caar x)) 1)
        (cl-incf (caar x))))
    (avy--goto
     (avy--process
      cands
      (cl-case style
        (pre #'avy--overlay-pre)
        (at #'avy--overlay-at)
        (at-full #'avy--overlay-at-full)
        (post #'avy--overlay-post))))))

(defun lispy-ace-symbol-replace (arg)
  "Jump to a symbol withing the current sexp and delete it.
Sexp is obtained by exiting the list ARG times."
  (interactive "p")
  (lispy-ace-symbol arg)
  (lispy-delete 1))

;;* Locals: outline
(defun lispy-outline-level ()
  "Compute the outline level of the heading at point."
  (save-excursion
    (save-match-data
      (end-of-line)
      (if (re-search-backward lispy-outline nil t)
          (max (cl-count ?* (match-string 0)) 1)
        0))))

(defun lispy-outline-next (arg)
  "Call `outline-next-visible-heading' ARG times."
  (interactive "p")
  (lispy-dotimes arg
    (let ((pt (point)))
      (outline-next-visible-heading 1)
      (unless (looking-at outline-regexp)
        (goto-char pt)
        (error "Past last outline")))))

(defvar lispy-pre-outline-pos 1
  "Point position before moving to outline with `lispy-outline-prev'.")

(defun lispy-outline-prev (arg)
  "Call `outline-previous-visible-heading' ARG times."
  (interactive "p")
  (unless (looking-at lispy-outline)
    (setq lispy-pre-outline-pos (point)))
  (lispy-dotimes arg
    (let ((pt (point)))
      (outline-previous-visible-heading 1)
      (unless (looking-at outline-regexp)
        (goto-char pt)
        (error "Past first outline")))))

(defun lispy-outline-right ()
  "Promote current outline level by one."
  (interactive)
  (save-excursion
    (beginning-of-line)
    (when (looking-at lispy-outline)
      (goto-char (match-end 0))
      (insert "*"))))

(defun lispy-outline-left ()
  "Demote current outline level by one."
  (interactive)
  (save-excursion
    (beginning-of-line)
    (when (looking-at lispy-outline)
      (if (<= (- (match-end 0)
                 (match-beginning 0))
              3)
          (progn
            (setq this-command 'lispy-outline-left)
            (lispy-complain "Can't demote outline"))
        (goto-char (match-end 0))
        (delete-char -1))
      t)))

(defun lispy-outline-goto-child ()
  "Goto the first variable `lispy-left' of the current outline."
  (interactive)
  (let ((end (save-excursion
               (or (re-search-forward lispy-outline nil t 2)
                   (point-max)))))
    (if (re-search-forward lispy-left end t)
        (progn
          (backward-char 1)
          (lispy--ensure-visible))
      (lispy-complain "This outline has no children"))))

(declare-function org-cycle-internal-local "org")
(declare-function org-content "org")
(declare-function org-cycle-internal-global "org")

(defun lispy-tab ()
  "Indent code and hide/show outlines.
When region is active, call `lispy-mark-car'."
  (interactive)
  (if (region-active-p)
      (lispy-mark-car)
    (if (looking-at lispy-outline)
        (progn
          (outline-minor-mode 1)
          (condition-case e
              (lispy-flet (org-unlogged-message (&rest _x))
                (require 'org)
                (let ((org-outline-regexp outline-regexp))
                  (org-cycle-internal-local)))
            (error
             (if (string= (error-message-string e) "before first heading")
                 (outline-next-visible-heading 1)
               (signal (car e) (cdr e))))))
      (lispy--normalize-1))))

(defun lispy-shifttab (arg)
  "Hide/show outline summary.
When ARG isn't nil, show table of contents."
  (interactive "P")
  (require 'org)
  (outline-minor-mode 1)
  (let ((org-outline-regexp outline-regexp))
    (lispy-flet (org-unlogged-message (&rest _x))
      (if arg
          (org-content)
        (when (eq org-cycle-global-status 'overview)
          (setq org-cycle-global-status 'contents))
        (org-cycle-internal-global))))
  (recenter))

;;* Locals: refactoring
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
        (when (eq (char-before) ?\))
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

(declare-function lispy-flatten--clojure "le-clojure")
(defun lispy-flatten (arg)
  "Inline a function at the point of its call.
Pass the ARG along."
  (interactive "P")
  (cond ((memq major-mode lispy-elisp-modes)
         (lispy-flatten--elisp arg))

        ((memq major-mode '(clojure-mode
                            nrepl-repl-mode
                            cider-clojure-interaction-mode))
         (require 'le-clojure)
         (lispy-flatten--clojure arg))

        (t
         (lispy-complain
          (format "%S isn't currently supported" major-mode)))))

(defun lispy-flatten--elisp (arg)
  "Inline an Elisp function at the point of its call.
The function body is obtained from `find-function-noselect'.
With ARG, use the contents of `lispy-store-region-and-buffer' instead."
  (let* ((begp (if (lispy-left-p)
                   t
                 (if (lispy-right-p)
                     (progn (backward-list)
                            nil)
                   (lispy-left 1))))
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
      (goto-char (car bnd))
      (delete-region (car bnd) (cdr bnd))
      (if (macrop (car expr))
          (progn
            (save-excursion
              (insert (pp-to-string (macroexpand (read str))))
              (when (bolp)
                (delete-char -1)))
            (indent-sexp))
        (let* ((e-args (cl-remove-if #'lispy--whitespacep (cdr expr)))
               (body (lispy--flatten-function fstr e-args))
               (print-quoted t))
          (lispy--insert body)))
      (lispy-alt-multiline)
      (when begp
        (goto-char (car bnd))))))

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
  (lispy-flet (message (&rest _x))
    (iedit-mode 0))
  (lispy-mark-symbol)
  (lispy-move-down 1)
  (iedit-mode)
  (deactivate-mark)
  (lispy-left 1)
  (lispy-delete 1)
  (save-excursion
    (lispy--out-backward 1)
    (lispy--normalize-1)))

(defun lispy-bind-variable ()
  "Bind current expression as variable."
  (interactive)
  (let* ((bnd (lispy--bounds-dwim))
         (str (lispy--string-dwim bnd)))
    (deactivate-mark)
    (setq lispy-bind-var-in-progress t)
    (delete-region (car bnd)
                   (cdr bnd))
    (insert (format "(let ((foobar %s)))" str))
    (backward-char 1)
    (newline-and-indent)
    (insert "foobar")
    (iedit-mode 1)
    (backward-delete-char 6)))

;;* Locals: multiple cursors
(declare-function mc/create-fake-cursor-at-point "ext:multiple-cursors-core")
(declare-function mc/all-fake-cursors "ext:multiple-cursors-core")
(declare-function mc/maybe-multiple-cursors-mode "ext:multiple-cursors-core")
(declare-function mc/mark-lines "ext:mc-mark-more")
(declare-function mc/remove-fake-cursors "ext:multiple-cursors-core")

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
    (if (lispy-left-p)
        (lispy-dotimes arg
          (mc/create-fake-cursor-at-point)
          (loop do (lispy-down 1)
             while (mc/all-fake-cursors (point) (1+ (point)))))
      (mc/mark-lines arg 'forwards))
    (mc/maybe-multiple-cursors-mode)))

(defun lispy-cursor-ace ()
  "Add a cursor at a visually selected paren.
Currently, only one cursor can be added with local binding.
Any amount can be added with a global binding."
  (interactive)
  (require 'multiple-cursors)
  (mc/create-fake-cursor-at-point)
  (lispy--avy-do
   "("
   (cons (window-start) (window-end))
   (lambda () (not (lispy--in-string-or-comment-p)))
   lispy-avy-style-paren)
  (mc/maybe-multiple-cursors-mode))

;;* Locals: ediff
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

;;* Locals: marking
(defun lispy-mark-right (arg)
  "Go right ARG times and mark."
  (interactive "p")
  (let* ((pt (point))
         (mk (mark))
         (lispy-ignore-whitespace t)
         (r (lispy--out-forward arg)))
    (deactivate-mark)
    (if (or (= pt (point))
            (= mk (point))
            (and (region-active-p)
                 (= (region-beginning)
                    (region-end))))
        (progn
          (lispy-complain "can't go any further")
          (if (> mk pt)
              (lispy--mark (cons pt mk))
            (lispy--mark (cons mk pt)))
          nil)
      (lispy--mark
       (lispy--bounds-dwim))
      r)))

(defun lispy-mark-left (arg)
  "Go left ARG times and mark."
  (interactive "p")
  (if (lispy-mark-right arg)
      (lispy-different)
    (when (= (point) (region-end))
      (exchange-point-and-mark))))

(defun lispy-mark-car ()
  "Mark the car of current thing."
  (interactive)
  (when (called-interactively-p 'interactive)
    (lispy--remember))
  (let ((bnd-1 (lispy--bounds-dwim))
        bnd-2)
    (cond ((and (eq (char-after (car bnd-1)) ?\")
                (eq (char-before (cdr bnd-1)) ?\"))
           (lispy--mark (cons (1+ (car bnd-1))
                              (1- (cdr bnd-1)))))

          ((and (eq (char-after (car bnd-1)) ?`)
                (eq (char-before (cdr bnd-1)) ?'))
           (lispy--mark (cons (1+ (car bnd-1))
                              (1- (cdr bnd-1)))))

          ((save-excursion
             (goto-char (car bnd-1))
             (looking-at "\\(['`,@]+\\)\\w"))
           (set-mark (match-end 1))
           (goto-char (cdr bnd-1)))

          (t
           (goto-char (car bnd-1))
           (while (and (equal bnd-1 (setq bnd-2 (bounds-of-thing-at-point 'sexp)))
                       (< (point) (cdr bnd-1)))
             (forward-char)
             (skip-chars-forward " "))
           (if bnd-2
               (lispy--mark bnd-2)
             (lispy-complain "can't descend further"))))))

;;* Locals: edebug
(defun lispy-edebug-stop ()
  "Stop edebugging, while saving current function arguments."
  (interactive)
  (if (bound-and-true-p edebug-active)
      (save-excursion
        (lispy-left 99)
        (if (looking-at
             "(\\(?:cl-\\)?def\\(?:un\\|macro\\)")
            (progn
              (goto-char (match-end 0))
              (search-forward "(")
              (backward-char 1)
              (forward-sexp 1)
              (let ((sexps
                     (mapcar
                      (lambda (x!)
                        (when (consp x!)
                          (setq x! (car x!)))
                        (cons x!
                              (let ((expr x!))
                                (edebug-eval expr))))
                      (delq '&key (delq '&optional (delq '&rest (lispy--preceding-sexp))))))
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
         (if (memq major-mode lispy-elisp-modes)
             (edebug-defun)
           (if (eq major-mode 'clojure-mode)
               (cider-debug-defun-at-point)
             (error "Can't debug for %S" major-mode))))
        ((= arg 2)
         (eval-defun nil))
        (t
         (let* ((expr (lispy--read (lispy--string-dwim)))
                (fun (car expr)))
           (if (fboundp fun)
               (let* ((fnd (find-definition-noselect fun nil))
                      (buf (car fnd))
                      (pt (cdr fnd)))
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
  (cond ((memq major-mode lispy-elisp-modes)
         (let* ((ldsi-sxp (lispy--setq-expression))
                (ldsi-fun (car ldsi-sxp)))
           (if (or (functionp ldsi-fun)
                   (macrop ldsi-fun))
               (let ((ldsi-args
                      (copy-seq
                       (help-function-arglist
                        (if (ad-is-advised ldsi-fun)
                            (ad-get-orig-definition ldsi-fun)
                          ldsi-fun)
                        t)))
                     (ldsi-vals (cdr ldsi-sxp))
                     ldsi-arg
                     ldsi-val)
                 (catch 'done
                   (while (setq ldsi-arg (pop ldsi-args))
                     (cond ((eq ldsi-arg '&optional)
                            (setq ldsi-arg (pop ldsi-args))
                            (set ldsi-arg (eval (pop ldsi-vals))))
                           ((eq ldsi-arg '&rest)
                            (setq ldsi-arg (pop ldsi-args))
                            (set ldsi-arg
                                 (if (functionp ldsi-fun)
                                     (mapcar #'eval ldsi-vals)
                                   ldsi-vals))
                            (throw 'done t))
                           (t
                            (setq ldsi-val (pop ldsi-vals))
                            (set ldsi-arg
                                 (if (functionp ldsi-fun)
                                     (eval ldsi-val)
                                   ldsi-val))))))
                 (lispy-goto-symbol ldsi-fun))
             (lispy-complain
              (format "%S isn't a function" ldsi-fun)))))
        (t
         (lispy-complain
          (format "%S isn't currently supported" major-mode)))))

(defvar cl--bind-lets)
(defvar cl--bind-forms)
(defvar cl--bind-defs)
(defvar cl--bind-block)
(defvar cl--bind-enquote)

(defmacro lispy-destructuring-setq (args expr)
  "Set ARGS to parts of EXPR.
An equivalent of `cl-destructuring-bind'."
  (declare (indent 2))
  (let* ((cl--bind-lets nil)
         (cl--bind-forms nil)
         (cl--bind-defs nil)
         (cl--bind-block (quote cl-none))
         (cl--bind-enquote nil)
         res)
    (cl--do-arglist (or args (quote (&aux))) expr)
    (setq res
          (nreverse cl--bind-lets))
    (cons 'progn
          (mapcar (lambda (x)
                    (cons 'setq x))
                  res))))

;;* Locals: miscellanea
(defun lispy-describe-bindings-C-4 ()
  "Describe bindings that start with \"C-4\"."
  (interactive)
  (describe-bindings (kbd "C-4")))

(defvar lispy-mode-map-x
  (let ((map (make-sparse-keymap)))
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
    (define-key map "v" 'lispy-view-test)
    (define-key map "B" 'lispy-store-region-and-buffer)
    (define-key map "R" 'lispy-reverse)
    (define-key map (char-to-string help-char) 'lispy-describe-bindings-C-4)
    map))

(defun lispy-x ()
  "Forward to `lispy-mode-map-x'."
  (interactive)
  (let ((char (read-char))
        fun)
    (if (setq fun (cdr (assoc char lispy-mode-map-x)))
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

(defun lispy-setq ()
  "Set the current variable, with completion."
  (interactive)
  (let ((sym (intern-soft (thing-at-point 'symbol)))
        sym-type
        cands)
    (when (and (boundp sym)
               (setq sym-type (get sym 'custom-type)))
      (cl-case sym-type
        (choice
         (setq cands
               (mapcar (lambda (x)
                         (setq x (car (last x)))
                         (cons (prin1-to-string x)
                               (if (symbolp x)
                                   (list 'quote x)
                                 x)))
                       (cdr sym-type))))
        (boolean
         (setq cands
               '(("nil" . nil) ("t" . t))))
        (t
         (error "Unrecognized custom type")))
      (let ((res (ivy-read (format "Set (%S): " sym) cands)))
        (when res
          (setq res
                (if (assoc res cands)
                    (cdr (assoc res cands))
                  (read res)))
          (eval `(setq ,sym ,res)))))))

(unless (fboundp 'macrop)
  (defun macrop (object)
    "Non-nil if and only if OBJECT is a macro."
    (let ((def (indirect-function object)))
      (when (consp def)
        (or (eq 'macro (car def))
            (and (autoloadp def) (memq (nth 4 def) '(macro t))))))))

(defalias 'lispy--preceding-sexp
    (if (fboundp 'elisp--preceding-sexp)
        'elisp--preceding-sexp
      'preceding-sexp))

(declare-function projectile-find-file "ext:projectile")
(declare-function projectile-find-file-other-window "ext:projectile")
(declare-function projectile-project-root "ext:projectile")
(defvar projectile-mode)
(declare-function find-file-in-project "ext:find-file-in-project")

(defun lispy-visit (arg)
  "Forward to find file in project depending on ARG."
  (interactive "p")
  (if (eq lispy-visit-method 'ffip)
      (find-file-in-project)
    (unless projectile-mode
      (projectile-global-mode 1))
    (cond ((= arg 1)
           (projectile-find-file nil))
          ((= arg 2)
           (projectile-find-file-other-window))
          (t
           (projectile-find-file arg)))))

(defun lispy-narrow (arg)
  "Narrow ARG sexps or region."
  (interactive "p")
  (cond ((region-active-p)
         (narrow-to-region (region-beginning) (region-end)))
        ((lispy-left-p)
         (narrow-to-region (point)
                           (save-excursion
                             (lispy-forward arg)
                             (point))))
        ((lispy-right-p)
         (narrow-to-region (point)
                           (save-excursion
                             (lispy-backward arg)
                             (point))))
        ((looking-at lispy-outline)
         (save-excursion
           (outline-back-to-heading)
           (narrow-to-region
            (point)
            (progn
              (outline-next-heading)
              (1- (point))))))))

(defun lispy-widen ()
  "Forward to `widen'."
  (interactive)
  (widen))

(defun lispy-other-space ()
  "Alternative to `lispy-space'."
  (interactive)
  (cond ((lispy-right-p)
         (backward-char 1)
         (insert " "))
        ((lispy-left-p)
         (insert " ")
         (backward-char 1))))

(defun lispy-paste (arg)
  "Forward to `yank'.
If the region is active, replace instead of yanking.
When ARG is given, paste at that place in the current list."
  (interactive "p")
  (cond ((region-active-p)
         (let ((bnd (lispy--bounds-dwim)))
           (deactivate-mark)
           (delete-region (car bnd)
                          (cdr bnd))
           (yank)))
        ((> arg 1)
         (lispy-mark-car)
         (lispy-down (- arg 2))
         (deactivate-mark)
         (just-one-space)
         (yank)
         (unless (or (eolp) (looking-at lispy-right))
           (just-one-space)
           (forward-char -1)))
        (t
         (if (and (lispy-right-p)
                  (save-excursion
                    (forward-list -1)
                    (bolp)))
             (newline)
           (when (bolp)
             (open-line 1)))
         (yank)
         (when (and (lispy-right-p)
                    (lispy-left-p))
           (insert " ")))))

(defalias 'lispy-font-lock-ensure
    (if (fboundp 'font-lock-ensure)
        'font-lock-ensure
      'font-lock-fontify-buffer))

(defun lispy--fontify (str)
  "Return STR fontified in `emacs-lisp-mode'."
  (with-temp-buffer
    (emacs-lisp-mode)
    (show-paren-mode)
    (insert str)
    (lispy-font-lock-ensure)
    (let ((color-paren (face-attribute 'show-paren-match :background))
          (color-cursor-fg (face-attribute 'lispy-cursor-face :foreground))
          (color-cursor-bg (face-attribute 'lispy-cursor-face :background))
          pt mk p1 p2)
      (goto-char (point-min))
      (when (search-forward "|" nil t)
        (backward-delete-char 1)
        (setq pt (point))
        (when (eolp)
          (insert " ")))
      (goto-char (point-min))
      (when (search-forward "~" nil t)
        (backward-delete-char 1)
        (setq mk (point))
        (when (< mk pt)
          (decf pt)))
      (goto-char pt)
      (cond ((lispy-right-p)
             (setq p2 (1- (point)))
             (lispy-different)
             (setq p1 (point)))
            ((lispy-left-p)
             (setq p1 (point))
             (lispy-different)
             (setq p2 (1- (point)))))
      (setq str (buffer-string))
      (add-face-text-property 0 (length str) '(face 'lispy-test-face) t str)
      (when pt
        (when mk
          (if (< mk pt)
              (progn
                (add-text-properties (1- mk) (1- pt) '(face region) str)
                (set-text-properties (1- pt) pt '(face cursor) str))
            (add-text-properties (1- (min pt mk)) (1- (max pt mk)) '(face region) str)
            (set-text-properties (1- pt) pt '(face cursor) str)))
        (when p1
          (add-text-properties
           (1- p1) p1
           `(face (:background
                   ,color-paren
                   :foreground
                   ,(if (and mk
                             (>= p1 (min pt mk))
                             (<= p1 (max pt mk)))
                        color-cursor-fg
                        color-cursor-bg))) str))
        (when p2
          (add-text-properties
           (1- p2) p2
           `(face (:background
                   ,color-paren
                   :foreground
                   ,(if (and mk
                             (>= p2 (min pt mk))
                             (<= p2 (max pt mk)))
                        color-cursor-fg
                        color-cursor-bg)))
           str))
        (add-text-properties
         (1- pt) pt
         `(face (:background
                 ,color-cursor-bg
                 :foreground
                 ,(if (eq pt p1)
                      color-paren
                      color-cursor-fg)))
         str)
        str))))

(defun lispy-view-test ()
  "View better the test at point."
  (interactive)
  (cond ((and (overlayp lispy-overlay)
              (eq (point) (get 'lispy-overlay 'last-point)))
         (delete-overlay lispy-overlay)
         (setq lispy-overlay nil))

        ((looking-at "(should (string=")
         (setq lispy-hint-pos (point))
         (let* ((expr (cadr (read (lispy--string-dwim))))
                (str1 (cadr (cadr expr)))
                (str2 (caddr expr))
                (keys (cddadr expr))
                (sep (make-string (- (window-width)
                                     (current-column)) ?-)))
           (lispy--show
            (concat "\n"
                    (lispy--fontify str1)
                    "\n" sep "\n"
                    (substring (prin1-to-string keys) 1 -1)
                    "\n" sep "\n"
                    (lispy--fontify str2)
                    "\n"))))

        (t
         (lispy-complain "should position point before (should (string="))))

;;* Predicates
(defun lispy--in-string-p ()
  "Test if point is inside a string.
Return start of string it is."
  (let ((syn (syntax-ppss)))
    (or (and (nth 3 syn)
             (nth 8 syn))
        (and (eq (char-after) ?\")
             (not (eq ?\\ (char-before)))
             (point)))))

(defun lispy--in-comment-p ()
  "Test if point is inside a comment."
  (save-excursion
    (unless (eolp)
      (forward-char 1))
    (nth 4 (syntax-ppss))))

(defun lispy--in-string-or-comment-p ()
  "Test if point is inside a string or a comment."
  (let* ((sp (syntax-ppss))
         (beg (nth 8 sp)))
    (when (or (eq (char-after beg) ?\")
              (nth 4 sp))
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
    (or (lispy-left-p)
        (looking-at lispy-outline))))

(defun lispy--symbolp (str)
  "Return t if STR is a symbol."
  (string-match "\\`\\(?:\\sw\\|\\s_\\)+\\'" str))

(defun lispy--string-markedp ()
  "Return t if the current active region is a string."
  (and (region-active-p)
       (eq ?\" (char-after (region-beginning)))
       (eq ?\" (char-before (region-end)))))

(defsubst lispy-looking-back (regexp)
  "Forward to (`looking-back' REGEXP)."
  (looking-back regexp (line-beginning-position)))

(defun lispy-bolp ()
  "Return t if point is at beginning of line, after optional spaces."
  (save-excursion
    (skip-chars-backward " ")
    (bolp)))

(defun lispy-after-string-p (str)
  "Return t if the string before point is STR."
  (string=
   (buffer-substring
    (max
     (- (point) (length str))
     1)
    (point))
   str))

;;* Pure
(defun lispy--bounds-dwim ()
  "Return a cons of region bounds if it's active.
Otherwise return cons of current string, symbol or list bounds."
  (let (bnd)
    (cond ((region-active-p)
           (cons (region-beginning)
                 (region-end)))
          ((lispy-right-p)
           (backward-list)
           (prog1 (bounds-of-thing-at-point 'sexp)
             (forward-list)))
          ((or (looking-at (format "[`'#]*%s" lispy-left))
               (looking-at "[`'#]"))
           (bounds-of-thing-at-point 'sexp))
          ((and (setq bnd (lispy--bounds-string))
                (or (eq (point) (car bnd))
                    (eq (point) (1- (cdr bnd)))))
           bnd)
          (t
           (let ((res (ignore-errors
                        (bounds-of-thing-at-point 'sexp))))
             (if res
                 (save-excursion
                   (goto-char (cdr res))
                   (lispy--in-string-or-comment-p)
                   (skip-chars-backward "[.,]")
                   (cons (car res) (point)))
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
                  (bounds-of-thing-at-point 'symbol)))))))))

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
  (unless (lispy--in-comment-p)
    (let ((beg (or (nth 8 (syntax-ppss))
                   (and (eq (char-after (point)) ?\")
                        (not (eq ?\\ (char-before)))
                        (point)))))
      (when (and beg (not (comment-only-p beg (1+ (point)))))
        (ignore-errors
          (cons beg (save-excursion
                      (goto-char beg)
                      (forward-sexp)
                      (point))))))))

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
                              (lispy-looking-back "^\\s-*")))
               (setq pt (point)))
             (goto-char pt)
             (end-of-line)
             (cons beg (point)))))))

(defun lispy--string-dwim (&optional bounds)
  "Return the string that corresponds to BOUNDS.
`lispy--bounds-dwim' is used if BOUNDS is nil."
  (setq bounds (or bounds (lispy--bounds-dwim)))
  (buffer-substring-no-properties (car bounds) (cdr bounds)))

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

;;* Utilities: movement
(defun lispy--out-forward (arg)
  "Move outside list forwards ARG times.
Return nil on failure, (point) otherwise."
  (lispy--exit-string)
  (catch 'break
    (dotimes (_i arg)
      (if (ignore-errors (up-list) t)
          (if buffer-read-only
              (deactivate-mark)
            (unless lispy-ignore-whitespace
              (lispy--remove-gaps)
              (lispy--indent-for-tab)))
        (when (lispy-left-p)
          (forward-list))
        (throw 'break nil)))
    (point)))

(defun lispy--out-backward (arg)
  "Move outside list forwards ARG times.
Return nil on failure, t otherwise."
  (let ((oldpt (point))
        newpt)
    (lispy--out-forward arg)
    (when (lispy-right-p)
      (forward-list -1))
    (if (= oldpt (setq newpt (point)))
        nil
      newpt)))

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

;;* Utilities: evaluation
(declare-function lispy--eval-clojure "le-clojure")
(defun lispy--eval (e-str &optional add-output)
  "Eval E-STR according to current `major-mode'.
The result is a string.

When ADD-OUTPUT is t, append the output to the result."
  (funcall
   (cond ((memq major-mode lispy-elisp-modes)
          'lispy--eval-elisp)
         ((memq major-mode '(clojure-mode
                             nrepl-repl-mode
                             cider-clojure-interaction-mode))
          (require 'le-clojure)
          (lambda (x)
            (lispy--eval-clojure x add-output t)))
         ((eq major-mode 'scheme-mode)
          (require 'le-scheme)
          'lispy--eval-scheme)
         ((eq major-mode 'lisp-mode)
          (require 'le-lisp)
          'lispy--eval-lisp)
         (t (error "%s isn't supported currently" major-mode)))
   e-str))

(defvar lispy-eval-match-data nil)

(defun lispy--eval-elisp-form (lispy-form lexical)
  "Eval LISPY-FORM and return its value.
If LEXICAL is t, evaluate using lexical scoping.
Restore and save `lispy-eval-match-data' appropriately,
so that no other packages disturb the match data."
  (let (val)
    (unwind-protect
         (progn
           (fset '\, #'identity)
           (set-match-data lispy-eval-match-data)
           (setq val (eval lispy-form lexical))
           (setq lispy-eval-match-data (match-data)))
      (fset '\, nil))
    val))

(defun lispy--eval-elisp (e-str)
  "Eval E-STR as Elisp code."
  (let ((e-sexp (read e-str)))
    (when (consp e-sexp)
      (if (and (memq (car e-sexp) '(defvar defcustom defvar-local))
               (consp (cdr e-sexp))
               (boundp (cadr e-sexp)))
          (set (cadr e-sexp) (eval (caddr e-sexp)))
        (if (memq (car e-sexp) '(\, \,@))
            (setq e-sexp (cadr e-sexp)))))
    (condition-case e
        (prin1-to-string
         (lispy--eval-elisp-form e-sexp lexical-binding))
      (error
       (progn
         (fset '\, nil)
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

;;* Utilities: tags
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
     (csetq . 2)
     (setq-default . 2)
     (add-to-list . 2)
     (add-hook . 2)
     (load . 1)
     (load-file . 1)
     (define-key . 3)
     (ert-deftest . 1)
     (declare-function . 1)
     (defalias . 2)
     (defvaralias . 2)
     (defvar-local . 1)
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
     (defstruct . 1)
     (cl-defstruct . 1)
     ;; org-mode specific
     (org-defkey . 3)
     ;; use-package specific
     (use-package . 1)
     ;; lispy-specific
     (lispy-defverb . 1)
     ;; misc
     (defhydra . 1)))
  "Alist of tag arities for supported modes.")

(defun lispy--fetch-this-file-tags (&optional file)
  "Fetch tags for FILE."
  (setq file (or file (buffer-file-name)))
  (let ((tags (semantic-parse-region (point-min) (point-max))))
    (when (memq major-mode (cons 'lisp-mode lispy-elisp-modes))
      (lexical-let ((arity (cdr (assoc major-mode lispy-tag-arity)))
                    (tag-regex (lispy--tag-regexp)))
        (mapc (lambda (x) (lispy--modify-tag x tag-regex arity file)) tags)))
    tags))

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
        ((memq major-mode lispy-elisp-modes)
         (concat
          "^([ \t\n]*\\_<"
          "\\("
          (regexp-opt
           (mapcar (lambda (x) (symbol-name (car x)))
                   (cdr (assoc mode lispy-tag-arity))))
          "\\)"
          "\\_>"))
        ((eq major-mode 'clojure-mode)
         "^(\\([a-z-A-Z0-0]+\\)")
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
                 (t 'font-lock-variable-name-face)))))

(defun lispy--modify-tag (x regex arity-alist file)
  "Re-parse X and modify it accordingly.
REGEX selects the symbol is 1st place of sexp.
ARITY-ALIST combines strings that REGEX matches and their arities.
FILE is the file where X is defined."
  (let* ((overlay (nth 4 x))
         (buffer (find-file-noselect file))
         (start (cond ((overlayp overlay)
                       (overlay-start overlay))
                      ((vectorp overlay)
                       (aref overlay 0))
                      (t
                       (error "Unexpected")))))
    (with-current-buffer buffer
      (save-excursion
        (goto-char (or start (point-min)))
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

(defun lispy--tag-sexp-elisp (x &optional file)
  "Get the actual sexp from semantic tag X in FILE."
  (let ((ov (nth 4 x))
        buf end)
    (if (overlayp ov)
        (setq buf (overlay-buffer ov)
              end (overlay-end ov))
      (if (vectorp ov)
          (setq buf (find-file-noselect
                     (or file
                         (aref ov 2)))
                end (aref ov 1))
        (error "Unexpected")))
    (with-current-buffer buf
      (save-excursion
        (goto-char end)
        (ignore-errors
          (lispy--preceding-sexp))))))

(defun lispy--tag-name-elisp (x &optional file)
  "Build tag name for Elisp tag X in FILE."
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
         (let ((sexp (lispy--tag-sexp-elisp x file)))
           (if (and sexp (listp sexp))
               (lispy--propertize-tag
                "define-derived-mode"
                (list (format "%s %s"
                              (cadr sexp)
                              (caddr sexp))))
             "define-derived-mode")))
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

(defun lispy--tag-name (x &optional file)
  "Given a semantic tag X in FILE, return its string representation.
This is `semantic-tag-name', amended with extra info.
For example, a `setq' statement is amended with variable name that it uses."
  (let ((str (cond ((memq major-mode lispy-elisp-modes)
                    (lispy--tag-name-elisp x file))
                   ((eq major-mode 'clojure-mode)
                    (lispy--tag-name-clojure x))
                   ((eq major-mode 'scheme-mode)
                    ;; (lispy--tag-name-scheme x)
                    (car x))
                   ((eq major-mode 'lisp-mode)
                    (lispy--tag-name-lisp x))
                   (t nil))))
    (when str
      (setq str (replace-regexp-in-string "\t" "    " str))
      (let ((width (car lispy-helm-columns)))
        (if (> (length str) width)
            (concat (substring str 0 (- width 4)) " ...")
          str)))))

(defun lispy--fetch-tags-recursive ()
  "Fetch all tags in current directory recursively."
  (lispy--fetch-tags
   (split-string
    (shell-command-to-string
     (format "find %s -type f -regex \".*\\.%s\" ! -regex \".*\\(\\.git\\|\\.cask\\).*\""
             default-directory
             (file-name-extension (buffer-file-name))))
    "\n"
    t)))

(defun lispy--fetch-tags-projectile ()
  "Fetch all tags in the projectile directory recursively."
  (require 'projectile)
  (let ((default-directory (projectile-project-root)))
    (lispy--fetch-tags-recursive)))

(defun lispy--goto (fun)
  "Jump to symbol selected from (FUN)."
  (let ((semantic-on (bound-and-true-p semantic-mode)))
    (semantic-mode 1)
    (let ((candidates (funcall fun)))
      (lispy--select-candidate
       (mapcar #'lispy--format-tag-line candidates)
       #'lispy--action-jump))
    (when (and lispy-no-permanent-semantic
               (not semantic-on))
      (semantic-mode -1))))

;;* Utilities: slurping and barfing
(defun lispy--slurp-forward ()
  "Grow current sexp forward by one sexp."
  (let ((pt (point))
        (char (char-before)))
    (skip-chars-forward " \t")
    (delete-region pt (point))
    (unless (or (lispy-after-string-p "()") (eolp))
      (insert " "))
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
    (unless (lispy-after-string-p "()")
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

;;* Utilities: source transformation
(defvar lispy--braces-table
  (let ((table (make-char-table 'syntax-table nil)))
    (modify-syntax-entry ?\{ "(}  " table)
    (modify-syntax-entry ?\} "){  " table)
    (modify-syntax-entry ?\[ "(]  " table)
    (modify-syntax-entry ?\] ")[  " table)
    (modify-syntax-entry ?\( "()  " table)
    (modify-syntax-entry ?\) ")(  " table)
    table)
  "Syntax table for paired braces.")

(defun lispy--read (str)
  "Read STR including comments and newlines."
  (let* ((deactivate-mark nil)
         (mode major-mode)
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
                      (if (lispy-after-string-p "ly-raw comment \"")
                          (goto-char (cdr cbnd))
                        (setq str (lispy--string-dwim cbnd))
                        (delete-region (car cbnd) (cdr cbnd))
                        (insert (format "(ly-raw string %S)" str))))))
                ;; ——— newlines ———————————————
                (lispy--replace-regexp-in-code "\n" " (ly-raw newline)")
                ;; ——— () —————————————————————
                (goto-char (point-min))
                (while (re-search-forward "[^\\]\\(()\\)" nil t)
                  (unless (lispy--in-string-or-comment-p)
                    (replace-match "(ly-raw empty)" nil nil nil 1)))
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
                    (forward-sexp)
                    (insert ")")
                    (replace-match "(ly-raw function ")))
                ;; ——— #{ or { or #( ——————————
                (goto-char (point-min))
                (while (re-search-forward "#(\\|{\\|#{" nil t)
                  (let ((class
                         (cond ((string= (match-string 0) "#{")
                                "clojure-set")
                               ((string= (match-string 0) "{")
                                "clojure-map")
                               ((string= (match-string 0) "#(")
                                "clojure-lambda")
                               (t
                                (error "Expected set or map or lambda")))))
                    (unless (lispy--in-string-or-comment-p)
                      (backward-char 1)
                      (save-excursion
                        (with-syntax-table lispy--braces-table
                          (forward-list 1))
                        (delete-char -1)
                        (insert "))"))
                      (delete-region (match-beginning 0) (match-end 0))
                      (insert "(ly-raw " class " ("))))
                ;; ——— 123# ———————————————————
                (goto-char (point-min))
                (while (re-search-forward "[0-9]+\\(#\\)" nil t)
                  (replace-match "\\#" nil t nil 1))
                ;; ——— #1 —————————————————————
                ;; Elisp syntax for circular lists
                (goto-char (point-min))
                (while (re-search-forward "\\(?:^\\|\\s-\\|\\s(\\)\\(#[0-9]+\\)" nil t)
                  (replace-match (format "(ly-raw reference %S)"
                                         (substring-no-properties
                                          (match-string 1)))
                                 nil nil nil 1))
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
                (while (re-search-forward "\\(?:[^\\]\\|^\\)`" nil t)
                  (unless (lispy--in-string-or-comment-p)
                    (when (looking-at "(")
                      (delete-char -1)
                      (insert "(ly-raw \\` ")
                      (forward-list 1)
                      (insert ")")
                      (backward-list 1)
                      (forward-char 7))))
                ;; ——— , ——————————————————————
                (goto-char (point-min))
                (while (re-search-forward "[^\\],[^@\"]" nil t)
                  (unless (lispy--in-string-or-comment-p)
                    (backward-char 2)
                    (if (eq major-mode 'clojure-mode)
                        (progn
                          (delete-char 1)
                          (insert "(ly-raw clojure-comma)"))
                      (let ((beg (point))
                            (sxp (ignore-errors (read (current-buffer)))))
                        (when (and (consp sxp)
                                   (eq (car sxp) '\,))
                          (insert ")")
                          (goto-char beg)
                          (delete-char 1)
                          (insert "(ly-raw \\, "))))))
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
                ;; ——— angle syntax —————————
                ;; used for markers/buffers/windows/overlays
                (goto-char (point-min))
                (while (re-search-forward "#<" nil t)
                  (unless (lispy--in-string-or-comment-p)
                    (delete-region (match-beginning 0) (match-end 0))
                    (insert "(ly-raw angle \"")
                    (re-search-forward ">")
                    (backward-delete-char 1)
                    (insert "\")")))
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
          (let* ((fnd
                  (save-window-excursion
                    (save-excursion
                      (find-function-noselect fun))))
                 (buf (car fnd))
                 (pt (cdr fnd)))
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
    (cond ((eq (car body) 'lambda)
           (setq body (cons 'defun body)))
          ((eq (car body) 'closure)
           (setq body `(defun noname ,@(cddr body))))
          ((eq (car body) 'defsubst)
           (setq body (cons 'defun (cdr body)))))
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
      (setq f-args nil))
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
          (setq ifs1 nil))))
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

;;* Utilities: error reporting
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

;;* Utilities: rest
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
      (lispy-flet (message (&rest _x))
        (indent-for-tab-command)))))

(defun lispy--remove-gaps ()
  "Remove dangling `\\s)'."
  (when (and (lispy-right-p)
             (looking-back "[^ \t\n]\\([ \t\n]+\\)\\s)"
                           (condition-case nil
                               (save-excursion
                                 (backward-list)
                                 (point))
                             (error (point-min))))
             (not (eq ?\\ (aref (match-string 0) 0))))
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
    (deactivate-mark)
    (indent-region beg (+ 2 end))))

(defun lispy--mark (bnd)
  "Mark BND.  BND is a cons of beginning and end positions."
  (setq deactivate-mark nil)
  (set-mark (car bnd))
  (goto-char (cdr bnd)))

(defun lispy--space-unless (context)
  "Insert one space.
Unless inside string or comment, or `looking-back' at CONTEXT."
  (unless (or lispy-no-space
              (bolp)
              (and (window-minibuffer-p)
                   (eq (point) (minibuffer-prompt-end)))
              (lispy--in-string-or-comment-p)
              (lispy-looking-back context))
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

        ((lispy-right-p)
         (save-excursion
           (backward-list)
           (indent-sexp)))

        ((lispy-left-p)
         (indent-sexp))

        (t
         (save-excursion
           (lispy--out-forward 1)
           (backward-list)
           (indent-sexp)))))

(defun lispy--delete ()
  "Delete one sexp."
  (unless (lispy-left-p)
    (error "Bad position"))
  (let ((bnd (lispy--bounds-list)))
    (delete-region (car bnd) (cdr bnd))
    (cond ((looking-at (concat "\n+" lispy-left))
           (delete-region (match-beginning 0)
                          (1- (match-end 0))))
          ((looking-at "\n\n+"))
          ((looking-at "[\n ]+")
           (delete-region (match-beginning 0)
                          (match-end 0)))
          ((looking-at lispy-right))

          (t
           (just-one-space)
           (when (lispy-after-string-p "( ")
             (backward-delete-char 1))))))

(defun lispy--current-tag ()
  "Forward to `semantic-current-tag'.
Try to refresh if nil is returned."
  (save-excursion
    (lispy-beginning-of-defun)
    (let ((tag (semantic-current-tag)))
      (setq tag
            (or (and tag (lispy--tag-name tag))
                (semantic-tag-name tag)
                (when (looking-at "(def")
                  (goto-char (match-end 0))
                  (forward-sexp 2)
                  (backward-char 1)
                  (thing-at-point 'sexp))
                (lispy--fancy-tag)))
      (when tag
        (regexp-quote tag)))))

(defun lispy--fancy-tag ()
  "Return a fancy tag name using `lispy-tag-arity'."
  (let ((arity-alist (cdr (assoc major-mode lispy-tag-arity)))
        (regex (lispy--tag-regexp)))
    (if (looking-at regex)
        (progn
          (goto-char (match-end 0))
          (let ((tag-head (match-string 1))
                beg arity)
            (skip-chars-forward " \n")
            (if (setq arity (cdr (assoc (intern tag-head) arity-alist)))
                (progn
                  (setq beg (point))
                  (condition-case nil
                      (forward-sexp arity)
                    (error
                     (forward-sexp 1)))
                  (concat tag-head " "
                          (replace-regexp-in-string
                           "\n" " " (buffer-substring-no-properties beg (point)))))
              tag-head)))
      (save-excursion
        (forward-char 1)
        (thing-at-point 'sexp)))))

(defvar helm-update-blacklist-regexps)
(defvar helm-candidate-number-limit)

(defun lispy--select-candidate (candidates action)
  "Select from CANDIDATES list with `helm'.
ACTION is called for the selected candidate."
  (let (strs)
    (cond ((eq lispy-completion-method 'helm)
           (require 'helm-help)
           ;; allows restriction with space
           (require 'helm-match-plugin)
           (let (helm-update-blacklist-regexps
                 helm-candidate-number-limit)
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
                   :preselect (lispy--current-tag)
                   :buffer "*lispy-goto*")))
          ((progn
             (setq strs (mapcar #'car candidates))
             (eq lispy-completion-method 'ivy))
           (ivy-read "tag: " strs
                     :require-match t
                     :preselect (lispy--current-tag)
                     :action (lambda (x)
                               (funcall action
                                        (cdr (assoc x candidates))))))
          (t
           (let ((res
                  (cl-case lispy-completion-method
                    (ido
                     (ido-completing-read "tag: " strs))
                    (t
                     (completing-read "tag: " strs)))))
             (funcall action (assoc res candidates)))))))

(defun lispy--action-jump (tag)
  "Jump to TAG."
  (if (eq (length tag) 3)
      (progn
        (push-mark)
        (find-file (cadr tag))
        (goto-char
         (let ((ov (cl-caddr tag)))
           (if (overlayp ov)
               (overlay-start ov)
             (aref ov 0))))
        (when (and (eq major-mode 'clojure-mode)
                   (not (looking-at "(")))
          (forward-char -1))
        (lispy--ensure-visible))
    (error "Unexpected tag: %S" tag)))

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

(defun lispy--prin1-to-string (expr offset mode)
  "Return the string representation of EXPR.
EXPR is indented first, with OFFSET being the column position of
the first character of EXPR.
MODE is the major mode for indenting EXPR."
  (with-temp-buffer
    (funcall mode)
    (dotimes (_i offset)
      (insert ?\ ))
    (lispy--insert expr)
    (buffer-substring-no-properties
     (+ (point-min) offset)
     (point-max))))

(defun lispy--splice-to-str (sexp)
  "Return the printed representation of SEXP.
The outer delimiters are stripped."
  (if sexp
      (substring
       (prin1-to-string sexp) 1 -1)
    ""))

(defun lispy--insert (expr)
  "Insert the EXPR read by `lispy--read'."
  (let ((start-pt (point))
        beg
        sxp type)
    (prin1 expr (current-buffer))
    (save-restriction
      (narrow-to-region start-pt (point))
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
           (prin1 (cons 'ly-raw (cddr sxp))
                  (current-buffer))
           (backward-list)
           (forward-char 7))
          (quote
           (delete-region beg (point))
           (insert "'")
           (let ((it (caddr sxp)))
             (if it
                 (prin1 it (current-buffer))
               (insert "()")))
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
           (insert (format "#%S" (cl-caddr sxp)))
           (goto-char beg))
          (clojure-set
           (delete-region beg (point))
           (insert (format "#{%s}" (lispy--splice-to-str (cl-caddr sxp))))
           (goto-char beg))
          (clojure-map
           (delete-region beg (point))
           (insert (format "{%s}" (lispy--splice-to-str (cl-caddr sxp))))
           (goto-char beg))
          (clojure-comma
           (delete-region beg (point))
           (delete-horizontal-space)
           (insert ", "))
          (angle
           (delete-region beg (point))
           (insert (format "#<%s>" (cl-caddr sxp)))
           (goto-char beg))
          (reference
           (delete-region beg (point))
           (insert (cl-caddr sxp)))
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
        (unless (save-match-data
                  (lispy--in-string-p))
          (replace-match ".")))
      (goto-char (point-min))
      (while (re-search-forward "[0-9]+\\(\\\\#\\)" nil t)
        (replace-match "#" nil t nil 1))
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
  (when (and (lispy-right-p)
             (not (lispy--in-comment-p)))
    (backward-list)
    (indent-sexp)
    (forward-list)))

(defun lispy--normalize-1 ()
  "Normalize/prettify current sexp."
  (let* ((bnd (lispy--bounds-dwim))
         (str (lispy--string-dwim bnd))
         (offset (save-excursion
                   (goto-char (car bnd))
                   (current-column)))
         (was-left (lispy-left-p)))
    (if (or (and (memq major-mode '(clojure-mode))
                 (string-match "\\^" str))
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
              (bolp))
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
      (unless (lispy-looking-back "[ (]")
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
        ov expose)
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

(defvar ediff-temp-indirect-buffer)
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

(defvar macrostep-keymap)
(defvar lispy--compat-cmd nil
  "Store the looked up compat command.")

(defun lispy--insert-or-call (def plist)
  "Return a lambda to call DEF if position is special.
Otherwise call `self-insert-command'.
PLIST currently accepts:
- :disable with a mode to disable
- :override with a lambda to conditionally abort command"
  (let ((disable (plist-get plist :disable))
        (override (plist-get plist :override)))
    `(lambda ()
       ,(format "Call `%s' when special, self-insert otherwise.\n\n%s"
                (symbol-name def) (documentation def))
       (interactive)
       ,@(when disable `((,disable -1)))
       (unless (looking-at lispy-outline)
         (lispy--ensure-visible))
       (cond ,@(cond ((null override) nil)
                     ((functionp override)
                      `((funcall ,override)))
                     ((eq (car override) 'cond)
                      (cdr override))
                     (t
                      (error "Unexpected :override %S" override)))

             ,@(when (memq 'edebug lispy-compat)
                 '(((lispy--edebug-commandp)
                    (call-interactively
                     lispy--edebug-command))))

             ,@(when (memq 'god-mode lispy-compat)
                 '(((and (bound-and-true-p god-global-mode))
                    (call-interactively 'god-mode-self-insert))))

             ,@(when (memq 'macrostep lispy-compat)
                 '(((and (bound-and-true-p macrostep-mode)
                     (setq lispy--compat-cmd (lookup-key macrostep-keymap (this-command-keys))))
                    (call-interactively lispy--compat-cmd))))

             ((region-active-p)
              (call-interactively ',def))

             ((lispy--in-string-or-comment-p)
              (call-interactively 'self-insert-command))

             ((or (lispy-left-p)
                  (lispy-right-p)
                  (and (lispy-bolp)
                       (looking-at ";")))
              (call-interactively ',def))

             (t
              (call-interactively 'self-insert-command))))))

(defun lispy--setq-expression ()
  "Return the smallest list to contain point.
Return an appropriate `setq' expression when in `let', `dolist',
`labels', `cond'."
  (save-excursion
    (let ((origin (point))
          (tsexp
           (ignore-errors
             (cond ((lispy-left-p)
                    (forward-list))
                   ((lispy-right-p))
                   ((region-active-p)
                    (lispy-complain "Unimplemented"))
                   (t
                    (up-list)))
             (lispy--preceding-sexp))))
      (when tsexp
        (lispy-different)
        (cond
          ((looking-back "(\\(?:lexical-\\)?let\\(?:\\*\\|-when-compile\\)?[ \t\n]*"
                         (line-beginning-position 0))
           (cons 'setq
                 (cl-mapcan
                  (lambda (x) (unless (listp x) (list x nil)))
                  tsexp)))

          ((lispy-after-string-p "(dolist ")
           `(lispy--dolist-item-expr ',tsexp))

          ;; point moves
          ((progn
             (lispy--out-backward 1)
             (looking-back
              "(\\(?:lexical-\\)?let\\(?:\\*\\|-when-compile\\)?[ \t\n]*"
              (line-beginning-position 0)))
           (cons 'setq tsexp))

          ((looking-back
            "(\\(?:cl-\\)?labels[ \t\n]*"
            (line-beginning-position 0))
           (cons 'defun tsexp))

          ((looking-at
            "(cond\\b")
           (let ((re tsexp))
             `(if ,(car re)
                  (progn
                    ,@(cdr re))
                lispy--eval-cond-msg)))
          ((and (looking-at "(\\(?:cl-\\)?\\(?:defun\\|defmacro\\)")
                (save-excursion
                  (lispy-flow 1)
                  (eq (point) origin)))
           (let* ((fn-name (save-excursion
                             (forward-char)
                             (forward-sexp 2)
                             (lispy--preceding-sexp)))
                  (int-form
                   (and (fboundp fn-name)
                        (interactive-form fn-name)))
                  (int-form (when (and (eq (car int-form) 'interactive)
                                       (listp (cadr int-form)))
                              (cadr int-form))))
             (if int-form
                 `(lispy-destructuring-setq ,tsexp
                      ,int-form)
               `(progn
                  ,@(mapcar
                     (lambda (x)
                       (list 'setq x nil))
                     (delq '&key (delq '&optional (delq '&rest tsexp))))))))
          (t tsexp))))))

;;* Key definitions
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

(defun lispy-define-key (keymap key def &rest plist)
  "Forward to (`define-key' KEYMAP KEY FUNC).
FUNC is obtained from (`lispy--insert-or-call' DEF PLIST)."
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

(defhydra lh-knight ()
  "knight"
  ("j" lispy-knight-down)
  ("k" lispy-knight-up)
  ("z" nil))

(defvar lispy-mode-map-special
  (let ((map (make-sparse-keymap)))
    ;; navigation
    (lispy-define-key map "l" 'lispy-right)
    (lispy-define-key map "h" 'lispy-left)
    (lispy-define-key map "f" 'lispy-flow)
    (lispy-define-key map "j" 'lispy-down)
    (lispy-define-key map "k" 'lispy-up)
    (lispy-define-key map "d" 'lispy-different)
    (lispy-define-key map "o" 'lispy-other-mode)
    (lispy-define-key map "p" 'lispy-eval-other-window)
    (lispy-define-key map "P" 'lispy-paste)
    (lispy-define-key map "y" 'lispy-occur)
    (lispy-define-key map "z" 'lh-knight/body)
    ;; outline
    (lispy-define-key map "J" 'lispy-outline-next)
    (lispy-define-key map "K" 'lispy-outline-prev)
    (lispy-define-key map "L" 'lispy-outline-goto-child)
    ;; Paredit transformations
    (lispy-define-key map ">" 'lispy-slurp)
    (lispy-define-key map "<" 'lispy-barf)
    (lispy-define-key map "/" 'lispy-splice)
    (lispy-define-key map "r" 'lispy-raise)
    (lispy-define-key map "R" 'lispy-raise-some)
    (lispy-define-key map "+" 'lispy-join)
    ;; more transformations
    (lispy-define-key map "C" 'lispy-convolute)
    (lispy-define-key map "w" 'lispy-move-up)
    (lispy-define-key map "s" 'lispy-move-down)
    (lispy-define-key map "O" 'lispy-oneline)
    (lispy-define-key map "M" 'lispy-alt-multiline)
    (lispy-define-key map "S" 'lispy-stringify)
    ;; marking
    (lispy-define-key map "a" 'lispy-ace-symbol
      :override '(cond ((looking-at lispy-outline)
                        (lispy-meta-return))))
    (lispy-define-key map "H" 'lispy-ace-symbol-replace)
    (lispy-define-key map "m" 'lispy-mark-list)
    ;; dialect-specific
    (lispy-define-key map "e" 'lispy-eval)
    (lispy-define-key map "E" 'lispy-eval-and-insert)
    (lispy-define-key map "G" 'lispy-goto-local)
    (lispy-define-key map "g" 'lispy-goto)
    (lispy-define-key map "F" 'lispy-follow t)
    (lispy-define-key map "D" 'pop-tag-mark)
    (lispy-define-key map "A" 'lispy-beginning-of-defun)
    ;; miscellanea
    (lispy-define-key map "SPC" 'lispy-space)
    (lispy-define-key map "i" 'lispy-tab)
    (lispy-define-key map "I" 'lispy-shifttab)
    (lispy-define-key map "N" 'lispy-narrow)
    (lispy-define-key map "W" 'lispy-widen)
    (lispy-define-key map "c" 'lispy-clone)
    (lispy-define-key map "u" 'lispy-undo)
    (lispy-define-key map "q" 'lispy-ace-paren
      :override '(cond ((bound-and-true-p view-mode)
                        (View-quit))))
    (lispy-define-key map "Q" 'lispy-ace-char)
    (lispy-define-key map "v" 'lispy-view)
    (lispy-define-key map "t" 'lispy-teleport
      :override '(cond ((looking-at lispy-outline)
                        (end-of-line))))
    (lispy-define-key map "n" 'lispy-new-copy)
    (lispy-define-key map "b" 'lispy-back)
    (lispy-define-key map "B" 'lispy-ediff-regions)
    (lispy-define-key map "x" 'lispy-x)
    (lispy-define-key map "Z" 'lispy-edebug-stop)
    (lispy-define-key map "V" 'lispy-visit)
    (lispy-define-key map "-" 'lispy-ace-subword)
    (lispy-define-key map "." 'lispy-repeat)
    (lispy-define-key map "~" 'lispy-tilde)
    ;; digit argument
    (mapc (lambda (x) (lispy-define-key map (format "%d" x) 'digit-argument))
          (number-sequence 0 9))
    map))

;;* Paredit compat
(defun lispy-close-round-and-newline (arg)
  "Forward to (`lispy-out-forward-newline' ARG).
Insert \")\" in strings and comments."
  (interactive "p")
  (if (or (lispy--in-string-or-comment-p)
          (lispy-after-string-p "?\\"))
      (insert ")")
    (lispy-out-forward-newline arg)))

(defun lispy-open-square (arg)
  "Forward to (`lispy-brackets' ARG).
Insert \"[\" in strings and comments."
  (interactive "p")
  (if (lispy--in-string-or-comment-p)
      (insert "[")
    (lispy-brackets arg)))

(defun lispy-open-curly (arg)
  "Forward to( `lispy-braces' ARG).
Insert \"{\" in strings and comments."
  (interactive "p")
  (if (lispy--in-string-or-comment-p)
      (insert "{")
    (lispy-braces arg)))

(defun lispy-close-square (arg)
  "Forward to function `lispy-right' with ARG.
Insert \"]\" in strings and comments."
  (interactive "p")
  (if (lispy--in-string-or-comment-p)
      (insert "]")
    (lispy-right arg)))

(defun lispy-close-curly (arg)
  "Forward to function `lispy-right' with ARG.
Insert \"}\" in strings and comments."
  (interactive "p")
  (if (lispy--in-string-or-comment-p)
      (insert "}")
    (lispy-right arg)))

(defun lispy-doublequote (arg)
  "Insert a pair of quotes around the point.
When ARG is non-nil, unquote the current string."
  (interactive "P")
  (let (bnd)
    (cond ((region-active-p)
           (if arg
               (lispy-unstringify)
             (lispy-stringify)))

          ((and (setq bnd (lispy--bounds-string))
                (not (= (point) (car bnd))))
           (if (= (point) (1- (cdr bnd)))
               (forward-char 1)
             (if arg
                 (lispy-unstringify)
               (insert "\\\""))))

          (arg
           (lispy-stringify))

          ((lispy-after-string-p "?\\")
           (self-insert-command 1))

          ((lispy--in-comment-p)
           (insert "\""))

          (t
           (lispy--space-unless "^\\|\\s-\\|\\s(\\|[#]")
           (insert "\"\"")
           (unless (looking-at "\n\\|)\\|}\\|\\]\\|$")
             (just-one-space)
             (backward-char 1))
           (backward-char)))))

(defun lispy-meta-doublequote (arg)
  "Stringify current expression or forward to (`lispy-meta-doublequote' ARG)."
  (interactive "P")
  (let ((bnd (lispy--bounds-string)))
    (if bnd
        (goto-char (cdr bnd))
      (if (lispy-left-p)
          (lispy-stringify)
        (lispy-doublequote arg)))))

(defun lispy-forward-delete (arg)
  "Delete ARG sexps."
  (interactive "p")
  (let (bnd)
    (cond ((lispy-left-p)
           (forward-char 1))
          ((looking-at lispy-right)
           (forward-char 1)
           (setq bnd (lispy--bounds-dwim))
           (delete-region (car bnd) (cdr bnd)))
          ((and (setq bnd (lispy--bounds-string))
                (eq (point) (car bnd)))
           (forward-char 1))
          (t
           (lispy-delete arg)))))

(defun lispy-backward-delete (arg)
  "Delete ARG sexps backward."
  (interactive "p")
  (cond ((and (eq (char-before) ?\")
              (null (lispy--bounds-string)))
         (backward-char 1))
        ((lispy-looking-back lispy-left)
         (lispy-delete-backward arg)
         (insert " "))
        ((lispy-right-p)
         (backward-char 1))
        (t (lispy-delete-backward arg))))

(defun lispy-wrap-round ()
  "Forward to `lispy-parens'."
  (interactive)
  (lispy-parens 2))

(defun lispy-splice-sexp-killing-backward ()
  "Forward to `lispy-raise'."
  (interactive)
  (let ((bnd (lispy--bounds-list)))
    (if (eq (point) (car bnd))
        (lispy-raise 1)
      (lispy--mark (cons (1+ (car bnd)) (point)))
      (lispy-raise 1)
      (deactivate-mark))))

(defun lispy-raise-sexp ()
  "Forward to `lispy-raise'."
  (interactive)
  (if (lispy-left-p)
      (lispy-raise 1)
    (lispy-mark-symbol)
    (lispy-different)
    (lispy-raise 1)
    (deactivate-mark)))

(defun lispy-convolute-sexp ()
  "Forward to `lispy-convolute'."
  (interactive)
  (unless (lispy-left-p)
    (lispy--out-backward 1))
  (lispy-convolute 1)
  (lispy--out-backward 1))

(defun lispy-forward-slurp-sexp (arg)
  "Forward to (`lispy-slurp' ARG)."
  (interactive "p")
  (save-excursion
    (unless (lispy-right-p)
      (lispy--out-forward 1))
    (lispy-slurp arg)))

(defun lispy-forward-barf-sexp (arg)
  "Forward to (`lispy-barf' ARG)."
  (interactive "p")
  (save-excursion
    (unless (lispy-left-p)
      (lispy--out-forward 1))
    (lispy-barf arg)))

(defun lispy-backward-slurp-sexp (arg)
  "Forward to (`lispy-slurp' ARG)."
  (interactive "p")
  (save-excursion
    (unless (lispy-left-p)
      (lispy--out-backward 1))
    (lispy-slurp arg)))

(defun lispy-backward-barf-sexp (arg)
  "Forward to (`lispy-barf' ARG)."
  (interactive "p")
  (save-excursion
    (unless (lispy-left-p)
      (lispy--out-backward 1))
    (lispy-barf arg)))

(defvar lispy-mode-map-base
  (let ((map (make-sparse-keymap)))
    ;; navigation
    (define-key map (kbd "C-a") 'lispy-move-beginning-of-line)
    (define-key map (kbd "C-e") 'lispy-move-end-of-line)
    ;; killing
    (define-key map (kbd "C-k") 'lispy-kill)
    (define-key map (kbd "M-d") 'lispy-kill-word)
    (define-key map (kbd "M-DEL") 'lispy-backward-kill-word)
    ;; misc
    (define-key map (kbd "(") 'lispy-parens)
    (define-key map (kbd ";") 'lispy-comment)
    (define-key map (kbd "M-q") 'lispy-fill)
    (define-key map (kbd "C-j") 'lispy-newline-and-indent)
    (define-key map (kbd "RET") 'lispy-newline-and-indent-plain)
    ;; tags
    (define-key map (kbd "M-.") 'lispy-goto-symbol)
    (define-key map (kbd "M-,") 'pop-tag-mark)
    map))

(defvar lispy-mode-map-paredit
  (let ((map (copy-keymap lispy-mode-map-base)))
    (define-key map (kbd "M-)") 'lispy-close-round-and-newline)
    (define-key map (kbd "C-M-n") 'lispy-forward)
    (define-key map (kbd "C-M-p") 'lispy-backward)
    (define-key map (kbd "[") 'lispy-open-square)
    (define-key map (kbd "]") 'lispy-close-square)
    (define-key map (kbd "{") 'lispy-open-curly)
    (define-key map (kbd "}") 'lispy-close-curly)
    (define-key map (kbd ")") 'lispy-right-nostring)
    (define-key map (kbd "\"") 'lispy-doublequote)
    (define-key map (kbd "M-\"") 'lispy-meta-doublequote)
    (define-key map (kbd "C-d") 'lispy-forward-delete)
    (define-key map (kbd "DEL") 'lispy-backward-delete)
    (define-key map (kbd "C-M-f") 'lispy-forward)
    (define-key map (kbd "C-M-b") 'lispy-backward)
    (define-key map (kbd "M-(") 'lispy-wrap-round)
    (define-key map (kbd "M-s") 'lispy-splice)
    (define-key map (kbd "M-<up>") 'lispy-splice-sexp-killing-backward)
    (define-key map (kbd "M-<down>") 'lispy-splice-sexp-killing-backward)
    (define-key map (kbd "M-r") 'lispy-raise-sexp)
    (define-key map (kbd "M-?") 'lispy-convolute-sexp)
    (define-key map (kbd "C-)") 'lispy-forward-slurp-sexp)
    (define-key map (kbd "C-<right>") 'lispy-forward-slurp-sexp)
    (define-key map (kbd "C-}") 'lispy-forward-barf-sexp)
    (define-key map (kbd "C-<left>") 'lispy-forward-barf-sexp)
    (define-key map (kbd "C-(") 'lispy-backward-slurp-sexp)
    (define-key map (kbd "C-M-<left>") 'lispy-backward-slurp-sexp)
    (define-key map (kbd "C-M-<right>") 'lispy-backward-barf-sexp)
    (define-key map (kbd "C-{") 'lispy-backward-barf-sexp)
    (define-key map (kbd "M-S") 'lispy-split)
    (define-key map (kbd "M-J") 'lispy-join)
    (define-key map (kbd "C-M-u") 'lispy-left)
    (define-key map (kbd "C-M-n") 'lispy-right)
    map))

(defvar lispy-mode-map-c-digits
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-1") 'lispy-describe-inline)
    (define-key map (kbd "C-2") 'lispy-arglist-inline)
    (define-key map (kbd "C-3") 'lispy-right)
    (define-key map (kbd "C-4") lispy-mode-map-x)
    (define-key map (kbd "C-7") 'lispy-cursor-down)
    (define-key map (kbd "C-8") 'lispy-parens-down)
    (define-key map (kbd "C-9") 'lispy-out-forward-newline)
    map))

(declare-function View-quit "view")
(defvar lispy-mode-map-lispy
  (let ((map (copy-keymap lispy-mode-map-base)))
    ;; navigation
    (define-key map (kbd "]") 'lispy-forward)
    (define-key map (kbd "[") 'lispy-backward)
    (define-key map (kbd ")") 'lispy-right-nostring)
    ;; kill-related
    (define-key map (kbd "C-y") 'lispy-yank)
    (define-key map (kbd "C-d") 'lispy-delete)
    (define-key map (kbd "DEL") 'lispy-delete-backward)
    (define-key map (kbd "M-k") 'lispy-kill-sentence)
    (define-key map (kbd "M-m") 'lispy-mark-symbol)
    (define-key map (kbd "C-,") 'lispy-kill-at-point)
    (define-key map (kbd "C-M-,") 'lispy-mark)
    ;; pairs
    (define-key map (kbd "{") 'lispy-braces)
    (define-key map (kbd "}") 'lispy-brackets)
    (define-key map (kbd "\"") 'lispy-quotes)
    ;; insert
    (define-key map (kbd ":") 'lispy-colon)
    (define-key map (kbd "^") 'lispy-hat)
    (define-key map (kbd "'") 'lispy-tick)
    (define-key map (kbd "`") 'lispy-backtick)
    (define-key map (kbd "#") 'lispy-hash)
    (define-key map (kbd "M-j") 'lispy-split)
    (define-key map (kbd "M-J") 'lispy-join)
    (define-key map (kbd "<C-return>") 'lispy-open-line)
    (define-key map (kbd "<M-return>") 'lispy-meta-return)
    ;; misc
    (define-key map (kbd "M-o") 'lispy-string-oneline)
    (define-key map (kbd "M-i") 'lispy-iedit)
    (define-key map (kbd "<backtab>") 'lispy-shifttab)
    ;; outline
    (define-key map (kbd "M-<left>") 'lispy-outline-left)
    (define-key map (kbd "M-<right>") 'lispy-outline-right)
    map))

(defvar lispy-mode-map-oleh
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "φ") 'lispy-parens)
    (define-key map (kbd "σ") 'lispy-braces)
    (define-key map (kbd "ρ") 'lispy-brackets)
    (define-key map (kbd "θ") 'lispy-quotes)
    (define-key map (kbd "C-φ") 'lispy-parens-down)
    (define-key map (kbd "χ") 'lispy-right)
    (define-key map (kbd "C-M-a") 'lispy-beginning-of-defun)
    (define-key map (kbd "C-x C-j") 'lispy-debug-step-in)
    (define-key map (kbd "<return>") 'lispy-alt-line)
    (define-key map (kbd "RET") 'lispy-newline-and-indent-plain)
    (lispy-define-key map "Y" 'swiper)
    map))

(defun lispy-set-key-theme (theme)
  "Set `lispy-mode-map' for according to THEME.
THEME is a list of choices: 'special, 'lispy, 'paredit, 'c-digits."
  (setq lispy-mode-map
        (make-composed-keymap
         (delq nil
               (list
                (when (memq 'special theme) lispy-mode-map-special)
                (when (memq 'lispy theme) lispy-mode-map-lispy)
                (when (memq 'paredit theme) lispy-mode-map-paredit)
                (when (memq 'c-digits theme) lispy-mode-map-c-digits)
                (when (memq 'oleh theme) lispy-mode-map-oleh)))))
  (setcdr
   (assq 'lispy-mode minor-mode-map-alist)
   lispy-mode-map))

(if (equal user-mail-address "ohwoeowho@gmail.com")
    (lispy-set-key-theme '(oleh special lispy c-digits))
  (lispy-set-key-theme '(special lispy c-digits)))

(provide 'lispy)

;;; lispy.el ends here
