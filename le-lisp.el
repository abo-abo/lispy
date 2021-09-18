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

(defun lispy--use-sly-p ()
  (if lispy-use-sly
      (require 'sly)
    (unless (require 'slime nil t)
      (require 'sly)
      (setq lispy-use-sly t))))

(defun lispy--eval-lisp (str)
  "Eval STR as Common Lisp code."
  (let* ((deactivate-mark nil)
         (result (if (lispy--use-sly-p)
                     (with-current-buffer (process-buffer (lispy--cl-process))
                       (sly-eval `(slynk:eval-and-grab-output ,str)))
                   (slime-eval `(swank:eval-and-grab-output ,str)))))
    (pcase result
      (`("" "") "(ok)")
      (`("" ,val) val)
      (`(,out ,val)
       (concat (propertize (string-trim-left out) 'face 'font-lock-string-face) "\n\n" val)))))

(defun lispy--cl-process ()
  (unless (lispy--use-sly-p)
    (require 'slime-repl))
  (or (if (lispy--use-sly-p)
          (sly-current-connection)
        (slime-current-connection))
      (let (conn)
        (let ((wnd (current-window-configuration)))
          (if (lispy--use-sly-p)
              (sly)
            (slime))
          (while (not (if (lispy--use-sly-p)
                          (and (setq conn (sly-current-connection))
                               (sly-mrepl--find-buffer conn))
                        (and
                         (setq conn (slime-current-connection))
                         (get-buffer-window (slime-output-buffer)))))
            (sit-for 0.2))
          (set-window-configuration wnd)
          conn))))

(defun lispy--lisp-args (symbol)
  "Return a pretty string with arguments for SYMBOL."
  (let ((args
         (list
          (mapconcat
           #'prin1-to-string
           (read (lispy--eval-lisp
                  (format (if (lispy--use-sly-p)
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

(defun lispy-flatten--lisp ()
  (let* ((bnd (lispy--bounds-list))
         (str (lispy--string-dwim bnd))
         (expr (read str))
         (fexpr (read (lispy--eval-lisp
                       (format "(function-lambda-expression #'%S)" (car expr))))))
    (if (not (eq (car-safe fexpr) 'SB-INT:NAMED-LAMBDA))
        (error "Could not find the body of %S" (car expr))
      (setq fexpr (downcase
                   (prin1-to-string
                    `(lambda ,(nth 2 fexpr) ,(cl-caddr (nth 3 fexpr))))))
      (goto-char (car bnd))
      (delete-region (car bnd) (cdr bnd))
      (let* ((e-args (cdr expr))
             (body (lispy--flatten-function fexpr e-args)))
        (lispy--insert body)))))

(defun lispy-goto-symbol-lisp (symbol)
  ;; start SLY or SLIME if necessary
  (lispy--cl-process)
  (if (lispy--use-sly-p)
      (sly-edit-definition symbol)
    (slime-edit-definition symbol)))

(provide 'le-lisp)

;;; le-lisp.el ends here
