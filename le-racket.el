;;; le-racket.el --- lispy support for Racket. -*- lexical-binding: t -*-

;; Copyright (C) 2014-2019 Oleh Krehel

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

(require 'racket-mode nil t)

(declare-function racket-lispy-visit-symbol-definition "ext:racket-edit")
(declare-function racket--cmd/async "exit:racket-repl")

(defun lispy-goto-symbol-racket (symbol)
  (racket-lispy-visit-symbol-definition symbol))

(defun lispy--eval-racket (str)
  (let* ((awaiting 'RACKET-REPL-AWAITING)
         (response awaiting))
    (racket--cmd/async
     `(eval ,str)
     (lambda (v)
       (setq response v)))
    (with-timeout (1
                   (error "racket-command process timeout"))
      (while (eq response awaiting)
        (accept-process-output nil 0.001))
      (substring response 1 -2))))

(defun lispy-eval-racket (&optional _plain)
  (lispy--eval-racket (lispy--string-dwim)))

(provide 'le-racket)

;;; le-racket.el ends here
