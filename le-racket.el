;;; le-racket.el --- lispy support for Racket. -*- lexical-binding: t -*-

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

(require 'racket-mode nil t)

(declare-function racket-lispy-visit-symbol-definition "racket-edit")

(defun lispy-goto-symbol-racket (symbol)
  (racket-lispy-visit-symbol-definition symbol))

(provide 'le-racket)

;;; le-racket.el ends here
