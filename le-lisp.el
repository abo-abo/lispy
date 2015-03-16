;;; le-lisp.el --- lispy support for Common Lisp. -*- lexical-binding: t -*-

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

;;; Code:

(eval-and-compile
  (ignore-errors (require 'slime)))

(declare-function slime-output-buffer "ext:slime-repl")

(defun lispy--eval-lisp (str)
  "Eval STR as Common Lisp code."
  (require 'slime-repl)
  (unless (slime-current-connection)
    (let ((wnd (current-window-configuration)))
      (slime)
      (while (not (and (slime-current-connection)
                       (get-buffer-window (slime-output-buffer))))
        (sit-for 0.2))
      (set-window-configuration wnd)))
  (let (deactivate-mark)
    (cadr (slime-eval `(swank:eval-and-grab-output ,str)))))

(defun lispy--lisp-args (symbol)
  "Return a pretty string with arguments for SYMBOL."
  (let ((args
         (list
          (mapconcat
           #'prin1-to-string
           (read (lispy--eval-lisp
                  (format "(swank-backend:arglist #'%s)" symbol)))
           " "))))
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

(defun lispy--lisp-describe (symbol)
  "Return documentation for SYMBOL."
  (read
   (lispy--eval-lisp
    (substring-no-properties
     (format
      "(let ((x '%s))
        (or (if (boundp x)
                (documentation x 'variable)
              (documentation x 'function))
            \"undocumented\"))"
      symbol)))))


(provide 'le-lisp)

;;; le-lisp.el ends here
