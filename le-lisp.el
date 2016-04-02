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
  (ignore-errors (require 'slime))
  (ignore-errors (require 'sly)))

(declare-function slime-output-buffer "ext:slime-repl")
(declare-function slime "ext:slime")
(declare-function slime-current-connection "ext:slime")
(declare-function slime-eval "ext:slime")
(declare-function slime-edit-definition "ext:slime")
(declare-function sly-mrepl--find-buffer "ext:sly-mrepl")
(declare-function sly "ext:sly")
(declare-function sly-current-connection "ext:sly")
(declare-function sly-eval "ext:sly")
(declare-function sly-edit-definition "ext:sly")

(defcustom lispy-use-sly nil
  "Whether to use SLY instead of SLIME."
  :group 'lispy
  :type 'boolean)

(defun lispy--eval-lisp (str)
  "Eval STR as Common Lisp code."
  (unless lispy-use-sly
    (require 'slime-repl))
  (unless (if lispy-use-sly
              (sly-current-connection)
            (slime-current-connection))
    (let ((wnd (current-window-configuration)))
      (if lispy-use-sly
          (sly)
        (slime))
      (while (not (if lispy-use-sly
                      (when-let ((connection (sly-current-connection)))
                        (sly-mrepl--find-buffer connection))
                    (and
                     (slime-current-connection)
                     (get-buffer-window (slime-output-buffer)))))
        (sit-for 0.2))
      (set-window-configuration wnd)))
  (let* ((deactivate-mark nil)
         (result (if lispy-use-sly
                     (sly-eval `(slynk:eval-and-grab-output ,str))
                   (slime-eval `(swank:eval-and-grab-output ,str)))))
    (if (equal (car result) "")
        (cadr result)
      (concat (propertize (substring (car result) 1)
                          'face 'font-lock-string-face)
              "\n\n"
              (cadr result)))))

(defun lispy--lisp-args (symbol)
  "Return a pretty string with arguments for SYMBOL."
  (let ((args
         (list
          (mapconcat
           #'prin1-to-string
           (read (lispy--eval-lisp
                  (format (if lispy-use-sly
                              "(slynk-backend:arglist #'%s)"
                            "(swank-backend:arglist #'%s)")
                          symbol)))
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

(defun lispy-goto-symbol-lisp (symbol)
  (if lispy-use-sly
      (sly-edit-definition symbol)
    (slime-edit-definition symbol)))

(provide 'le-lisp)

;;; le-lisp.el ends here
