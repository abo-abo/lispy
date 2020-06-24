;;; le-julia.el --- lispy support for Julia. -*- lexical-binding: t -*-

;; Copyright (C) 2016-2019 Oleh Krehel

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

(require 'julia-mode nil t)
(require 'julia-shell nil t)

(declare-function julia-shell-collect-command-output "ext:julia-shell")

(defun lispy--eval-julia (str)
  "Eval STR as Julia code."
  (string-trim-right (julia-shell-collect-command-output str)))

;; TODO: simplify
(defun lispy-eval-julia-str (&optional bnd)
  (save-excursion
    (cond ((region-active-p)
           ;; get rid of "unexpected indent"
           (replace-regexp-in-string
            (concat
             "^"
             (save-excursion
               (goto-char (region-beginning))
               (buffer-substring-no-properties
                (line-beginning-position)
                (point))))
            "" (lispy--string-dwim)))
          ((looking-at lispy-outline)
           (string-trim-right
            (lispy--string-dwim
             (lispy--bounds-dwim))))
          ((lispy-bolp)
           (lispy--string-dwim
            (lispy--bounds-c-toplevel)))
          (t
           (cond ((lispy-left-p))
                 ((lispy-right-p)
                  (backward-list))
                 (t
                  (error "Unexpected")))
           (setq bnd (lispy--bounds-dwim))
           (ignore-errors (backward-sexp))
           (while (eq (char-before) ?.)
             (backward-sexp))
           (setcar bnd (point))
           (lispy--string-dwim bnd)))))

(defun lispy-eval-julia ()
  (lispy--eval-julia (lispy-eval-julia-str)))

(provide 'le-julia)

;;; le-julia.el ends here
