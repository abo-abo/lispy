;;; le-scheme.el --- lispy support for Scheme. -*- lexical-binding: t -*-

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
  (require 'geiser-eval nil t))

(require 'lispy-inline)

(defvar geiser-impl--implementation)
(declare-function geiser-repl--connection* "geiser-repl")
(declare-function run-geiser "geiser-repl")
(declare-function geiser-repl--buffer-name "geiser-repl")
(declare-function geiser-eval--send/wait "geiser-eval")
(declare-function geiser-eval--retort-error "geiser-eval")
(declare-function geiser-mode "geiser-mode")
(declare-function geiser-edit-symbol "geiser-edit")

(defun lispy--eval-scheme (str)
  "Eval STR as Scheme code."
  (unless (geiser-repl--connection*)
    (save-window-excursion
      (if geiser-impl--implementation
          (run-geiser geiser-impl--implementation)
        (call-interactively 'run-geiser))
      (geiser-mode 1)))
  (when (string-match "(\\(?:define\\|set!\\|struct\\)[ (]+\\(\\(?:\\w\\|\\s_\\)+\\)" str)
    (let ((name (match-string 1 str)))
      (setq str (format "(begin %s %s)" str name))))
  (with-current-buffer (geiser-repl--buffer-name geiser-impl--implementation)
    (let* ((code `(:eval (:scm ,str)))
           (ret (geiser-eval--send/wait code))
           (err (geiser-eval--retort-error ret))
           (output-str (cdr (assoc 'output ret)))
           (result-str (cadr (assoc 'result ret))))
      (cond (err
             (format "Error: %s" (string-trim output-str)))
            ((not (equal "" output-str))
             (concat
              (propertize
               output-str
               'face 'font-lock-string-face)
              "\n"
              result-str))
            (t
             result-str)))))

(defun lispy-goto-symbol-scheme (symbol)
  (geiser-edit-symbol (make-symbol symbol)))

(provide 'le-scheme)

;;; le-scheme.el ends here
