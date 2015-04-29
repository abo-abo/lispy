;;; lispy-inline.el --- inline arglist and documentation. -*- lexical-binding: t -*-

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
;; Display current function arguments or docstring in an in-place
;; overlay.

;;; Code:

(if (version< emacs-version "24.4")
    (progn
      (defsubst string-trim-left (string)
        "Remove leading whitespace from STRING."
        (if (string-match "\\`[ \t\n\r]+" string)
            (replace-match "" t t string)
          string))
      (defsubst string-trim-right (string)
        "Remove trailing whitespace from STRING."
        (if (string-match "[ \t\n\r]+\\'" string)
            (replace-match "" t t string)
          string))
      (defsubst string-trim (string)
        "Remove leading and trailing whitespace from STRING."
        (string-trim-left (string-trim-right string))))
  (require 'subr-x))

(defgroup lispy-faces nil
  "Font-lock faces for `lispy'."
  :group 'lispy
  :prefix "lispy-face-")

(defface lispy-face-hint
  '((((class color) (background light))
     :background "#fff3bc" :foreground "black")
    (((class color) (background dark))
     :background "black" :foreground "#fff3bc"))
  "Basic hint face."
  :group 'lispy-faces)

(defface lispy-face-req-nosel
  '((t (:inherit lispy-face-hint)))
  "Face for required unselected args."
  :group 'lispy-faces)

(defface lispy-face-req-sel
  '((t (:inherit lispy-face-req-nosel :bold t)))
  "Face for required selected args."
  :group 'lispy-faces)

(defface lispy-face-opt-nosel
  '((t (:inherit lispy-face-hint :foreground "#666666" :slant italic)))
  "Face for optional unselected args."
  :group 'lispy-faces)

(defface lispy-face-opt-sel
  '((t (:inherit lispy-face-opt-nosel :bold t)))
  "Face for optional selected args."
  :group 'lispy-faces)

(defface lispy-face-rst-nosel
  '((t (:inherit lispy-face-hint)))
  "Face for rest unselected args."
  :group 'lispy-faces)

(defface lispy-face-rst-sel
  '((t (:inherit lispy-face-rst-nosel :bold t)))
  "Face for rest selected args."
  :group 'lispy-faces)

(defcustom lispy-window-height-ratio 0.65
  "`lispy--show' will fail with string taller than window height times this.
The caller of `lispy--show' might use a substitute e.g. `describe-function'."
  :type 'float
  :group 'lispy)

(defvar lispy-elisp-modes
  '(emacs-lisp-mode lisp-interaction-mode eltex-mode minibuffer-inactive-mode)
  "Modes for which `lispy--eval-elisp' and related functions are appropriate.")

(defvar lispy-overlay nil
  "Hint overlay instance.")

(defvar lispy-hint-pos nil
  "Point position where the hint should be (re-) displayed.")

(declare-function lispy--eval-clojure "le-clojure")
(declare-function lispy--clojure-args "le-clojure")
(declare-function lispy--clojure-resolve "le-clojure")
(declare-function lispy--describe-clojure-java "le-clojure")
(declare-function lispy--eval-scheme "le-scheme")
(declare-function lispy--eval-lisp "le-lisp")
(declare-function lispy--lisp-args "le-lisp")
(declare-function lispy--lisp-describe "le-lisp")
(declare-function lispy--back-to-paren "lispy")
(declare-function lispy--current-function "lispy")

;; ——— Commands ————————————————————————————————————————————————————————————————
(defun lispy-arglist-inline ()
  "Display arglist for `lispy--current-function' inline."
  (interactive)
  (save-excursion
    (lispy--back-to-paren)
    (unless (and (when (overlayp lispy-overlay)
                   (delete-overlay lispy-overlay)
                   (setq lispy-overlay nil)
                   t)
                 (= lispy-hint-pos (point)))
      (cond ((memq major-mode lispy-elisp-modes)
             (let ((sym (intern-soft (lispy--current-function))))
               (cond ((fboundp sym)
                      (setq lispy-hint-pos (point))
                      (lispy--show (lispy--pretty-args sym))))))
            ((memq major-mode '(clojure-mode cider-repl-mode))
             (require 'le-clojure)
             (setq lispy-hint-pos (point))
             (lispy--show (lispy--clojure-args (lispy--current-function))))

            ((eq major-mode 'lisp-mode)
             (require 'le-lisp)
             (setq lispy-hint-pos (point))
             (lispy--show (lispy--lisp-args (lispy--current-function))))

            (t (error "%s isn't supported currently" major-mode))))))

(defun lispy--delete-help-windows ()
  "Delete help windows.
Return t if at least one was deleted."
  (let (deleted)
    (mapc (lambda (window)
            (when (eq (with-current-buffer (window-buffer window)
                        major-mode)
                      'help-mode)
              (delete-window window)
              (setq deleted t)))
          (window-list))
    deleted))

(defun lispy-describe-inline ()
  "Display documentation for `lispy--current-function' inline."
  (interactive)
  (let ((deleted (lispy--delete-help-windows))
        (sym (lispy--current-function))
        (pt (point)))
    (when (overlayp lispy-overlay)
      (delete-overlay lispy-overlay)
      (setq lispy-overlay nil)
      (setq deleted t))
    (save-excursion
      (if (region-active-p)
          (goto-char (region-beginning))
        (lispy--back-to-paren))
      (when (or (not deleted) (not (= lispy-hint-pos (point))))
        (setq lispy-hint-pos (point))
        (let* ((doc
                (cond
                  ((memq major-mode lispy-elisp-modes)
                   (let (dc)
                     (setq sym (intern-soft sym))
                     (cond ((fboundp sym)
                            (if (lispy--show-fits-p
                                 (setq dc (or (documentation sym)
                                              "undocumented")))
                                dc
                              (goto-char pt)
                              (describe-function sym)
                              nil))
                           ((boundp sym)
                            (if (lispy--show-fits-p
                                 (setq dc (or (documentation-property
                                               sym 'variable-documentation)
                                              "undocumented")))
                                dc
                              (goto-char pt)
                              (describe-variable sym)
                              nil))
                           (t "unbound"))))
                  ((memq major-mode '(clojure-mode cider-repl-mode))
                   (require 'le-clojure)
                   (let ((rsymbol (lispy--clojure-resolve sym)))
                     (string-trim-left
                      (replace-regexp-in-string
                       "^\\(?:-+\n\\|\n*.*$.*@.*\n*\\)" ""
                       (cond ((stringp rsymbol)
                              (read
                               (lispy--eval-clojure
                                (format "(with-out-str (clojure.repl/doc %s))" rsymbol))))
                             ((eq rsymbol 'special)
                              (read
                               (lispy--eval-clojure
                                (format "(with-out-str (clojure.repl/doc %s))" sym))))
                             ((eq rsymbol 'keyword)
                              "No docs for keywords")
                             ((and (listp rsymbol)
                                   (eq (car rsymbol) 'variable))
                              (cadr rsymbol))
                             (t
                              (or (lispy--describe-clojure-java sym)
                                  (format "Could't resolve '%s" sym))))))))
                  ((eq major-mode 'lisp-mode)
                   (require 'le-lisp)
                   (lispy--lisp-describe sym))
                  (t
                   (format "%s isn't supported currently" major-mode)))))
          (when doc
            (lispy--show (propertize doc 'face 'lispy-face-hint))))))))

;; ——— Utilities ———————————————————————————————————————————————————————————————
(defun lispy--arglist (symbol)
  "Get arglist for SYMBOL."
  (let (doc)
    (if (setq doc (help-split-fundoc (documentation symbol t) symbol))
        (car doc)
      (prin1-to-string
       (cons symbol (help-function-arglist symbol t))))))

(defun lispy--join-pad (strs width)
  "Join STRS padding each line with WIDTH spaces."
  (let ((padding (make-string width ?\ )))
    (mapconcat (lambda (x) (concat padding x))
               strs
               "\n")))

(defun lispy--show-fits-p (str)
  "Return nil if window isn't large enough to display STR whole."
  (let ((strs (split-string str "\n")))
    (when (< (length strs) (* lispy-window-height-ratio (window-height)))
      strs)))

(defun lispy--show (str)
  "Show STR hint when `lispy--show-fits-p' is t."
  (let ((last-point (point))
        (strs (lispy--show-fits-p str)))
    (when strs
      (setq str (lispy--join-pad
                 strs
                 (string-width (buffer-substring
                                (line-beginning-position)
                                (point)))))
      (save-excursion
        (goto-char lispy-hint-pos)
        (if (= -1 (forward-line -1))
            (setq str (concat str "\n"))
          (end-of-line)
          (setq str (concat "\n" str)))
        (setq str (concat str (make-string 1 (char-after))))
        (font-lock-unfontify-region (point) (+ (point) 1))
        (if lispy-overlay
            (progn
              (move-overlay lispy-overlay (point) (+ (point) 1))
              (overlay-put lispy-overlay 'invisible nil))
          (setq lispy-overlay (make-overlay (point) (+ (point) 1)))
          (overlay-put lispy-overlay 'priority 9999))
        (overlay-put lispy-overlay 'display str)
        (overlay-put lispy-overlay 'after-string "")
        (put 'lispy-overlay 'last-point last-point))
      t)))

(defun lispy--pretty-args (symbol)
  "Return a vector of fontified strings for function SYMBOL."
  (let* ((args (cdr (split-string (lispy--arglist symbol) "[( )]" t)))
         (p-opt (cl-position "&optional" args :test 'equal))
         (p-rst (cl-position "&rest" args :test 'equal))
         (a-req (cl-subseq args 0 (or p-opt p-rst (length args))))
         (a-opt (and p-opt
                     (cl-subseq args (1+ p-opt) (or p-rst (length args)))))
         (a-rst (and p-rst (last args))))
    (format
     "(%s)"
     (mapconcat
      #'identity
      (append
       (list (propertize (symbol-name symbol) 'face 'lispy-face-hint))
       (mapcar
        (lambda (x)
          (propertize (downcase x) 'face 'lispy-face-req-nosel))
        a-req)
       (mapcar
        (lambda (x)
          (propertize (downcase x) 'face 'lispy-face-opt-nosel))
        a-opt)
       (mapcar
        (lambda (x)
          (propertize (concat (downcase x) "...") 'face 'lispy-face-rst-nosel))
        a-rst))
      " "))))

(provide 'lispy-inline)

;;; Local Variables:
;;; outline-regexp: ";; ———"
;;; End:

;;; lispy-inline.el ends here
