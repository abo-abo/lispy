;;; lispy-inline.el --- inline arglist and documentation.

;; Copyright (C) 2014 Oleh Krehel

;; Author: Oleh Krehel <ohwoeowho@gmail.com>
;; URL: https://github.com/abo-abo/lispy
;; Version: 0.5
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
;; Display current function arguments or docstring in an in-place
;; overlay.

;;; Code:

(require 's)

(defgroup lispy-faces nil
  "Font-lock faces for `lispy'."
  :group 'lispy
  :prefix "lispy-face-")

(defface lispy-face-hint
  '((t (:background "#fff3bc" :foreground "black")))
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

(defcustom lispy-window-height-ratio 0.75
  "`lispy--show' will fail with string taller than window height times this.
The caller of `lispy--show' might use a substitute e.g. `describe-function'."
  :type 'float
  :group 'lispy)

(defvar lispy-overlay nil
  "Hint overlay instance.")

(defvar lispy-hint-pos nil
  "Point position where the hint should be (re-) displayed.")

;; ——— Commands ————————————————————————————————————————————————————————————————
(defun lispy-arglist-inline ()
  "Display arglist for `lispy--current-function' inline."
  (interactive)
  (let (deleted)
    (when (overlayp lispy-overlay)
      (delete-overlay lispy-overlay)
      (setq lispy-overlay nil)
      (setq deleted t))
    (save-excursion
      (lispy--back-to-paren)
      (when (or (not deleted) (not (= lispy-hint-pos (point))))
        (cond ((memq major-mode '(emacs-lisp-mode lisp-interaction-mode))
               (let ((sym (intern-soft (lispy--current-function))))
                 (cond ((fboundp sym)
                        (setq lispy-hint-pos (point))
                        (lispy--show (lispy--pretty-args sym))))))
              ((eq major-mode 'clojure-mode)
               (setq lispy-hint-pos (point))
               (lispy--show (lispy--clojure-args (lispy--current-function))))

              (t (error "Only elisp is supported currently")))))))

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
                         (first (keep #(ns-resolve %% '%s) (all-ns)))))
                 (if (keyword? '%s)
                     'keyword
                   'unknown))"
               symbol
               symbol
               symbol
               symbol
               symbol))))
    (if (string-match "^#'\\(.*\\)$" str)
        (match-string 1 str)
      (intern str))))

(defun lispy--clojure-args (symbol)
  "Return a pretty string with arguments for SYMBOL.
Besides functions, handles specials, keywords, maps, vectors and sets."
  (let* ((sym (lispy--clojure-resolve symbol))
         (args (cond
                 ((eq sym 'special)
                  (read
                   (lispy--eval-clojure
                    (format
                     "(->> (with-out-str (doc %s))
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
    (format
     "(%s %s)"
     (propertize symbol 'face 'lispy-face-hint)
     (mapconcat
      #'identity
      (mapcar (lambda(x) (propertize (downcase x)
                                'face 'lispy-face-req-nosel)) args)
      (concat "\n"
              (make-string (+ 2 (length symbol)) ? ))))))

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

(defun lispy-describe-inline ()
  "Display documentation for `lispy--current-function' inline."
  (interactive)
  (let ((deleted (lispy--delete-help-windows)))
    (when (overlayp lispy-overlay)
      (delete-overlay lispy-overlay)
      (setq lispy-overlay nil)
      (setq deleted t))
    (save-excursion
      (lispy--back-to-paren)
      (when (or (not deleted) (not (= lispy-hint-pos (point))))
        (setq lispy-hint-pos (point))
        (let* ((sym (lispy--current-function))
               (doc
                (cond
                  ((memq major-mode '(emacs-lisp-mode lisp-interaction-mode))
                   (let (dc)
                     (if (fboundp (setq sym (intern-soft sym)))
                         (if (lispy--show-fits-p
                              (setq dc (or (documentation sym) "undocumented")))
                             dc
                           (describe-function sym)
                           nil)
                       "unbound")))
                  ((eq major-mode 'clojure-mode)
                   (let ((rsymbol (lispy--clojure-resolve sym)))
                     (s-trim
                      (replace-regexp-in-string
                       "^\\(?:-+\n\\|\n*.*$.*@.*\n*\\)" ""
                       (cond ((stringp rsymbol)
                              (read
                               (lispy--eval-clojure
                                (format "(with-out-str (doc %s))" rsymbol)))
                              rsymbol)
                             ((eq rsymbol 'special)
                              (read
                               (lispy--eval-clojure
                                (format "(with-out-str (doc %s))" sym))))
                             ((eq rsymbol 'keyword)
                              (error "No docs for keywords"))
                             (t
                              (or (lispy--describe-clojure-java sym)
                                  (error
                                   "Could't resolve '%s" sym))))))))

                  (t
                   (error "%s isn't supported currently" major-mode)))))
          (when doc
            (lispy--show (propertize doc 'face 'lispy-face-hint))))))))

;; ——— Utilities ———————————————————————————————————————————————————————————————
(defun lispy--arglist (symbol)
  "Get arglist for SYMBOL."
  (let (doc)
    (if (setq doc (help-split-fundoc (documentation symbol t) symbol))
        (car doc)
      (prin1-to-string
       (cons symbol (help-function-arglist symbol))))))

(defun lispy--join-pad (strs width)
  "Join STRS padding each line with WIDTH spaces."
  (let ((padding (make-string width ?\ )))
    (mapconcat (lambda(x) (concat padding x))
               strs
               "\n")))

(defun lispy--show-fits-p (str)
  "Return nil if window isn't large enough to display STR whole."
  (let ((strs (split-string str "\n")))
    (when (< (length strs) (* lispy-window-height-ratio (window-height)))
      strs)))

(defun lispy--show (str)
  "Show STR hint when `lispy--show-fits-p' is t."
  (let ((strs (lispy--show-fits-p str)))
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
        (overlay-put lispy-overlay 'after-string ""))
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
       (mapcar (lambda(x) (propertize (downcase x)
                                 'face 'lispy-face-req-nosel)) a-req)
       (mapcar (lambda(x) (propertize (downcase x)
                                 'face 'lispy-face-opt-nosel)) a-opt)
       (mapcar (lambda(x) (propertize (concat (downcase x) "...")
                                 'face 'lispy-face-rst-nosel)) a-rst))
      " "))))

(provide 'lispy-inline)

;;; Local Variables:
;;; outline-regexp: ";; ———"
;;; End:

;;; lispy-inline.el ends here
