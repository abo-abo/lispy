;;; lispy-clojure.clj --- lispy support for Clojure.

;; Copyright (C) 2015 Oleh Krehel

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

(ns lispy-clojure
  (:require (clojure [repl :as repl])))

(defn symbol-function
  "Return the source code for function SYM."
  [sym]
  (read-string
   (repl/source-fn
    sym)))

(defn flatten-expr
  "Flatten a function call EXPR by substituting the arguments."
  [expr]
  (let [func (first expr)
        args (rest expr)
        func-def (symbol-function func)
        func-body (nth func-def 4)
        func-args (first func-body)
        func-impl (rest func-body)]
    (cons 'let
          (cons (vec (interleave func-args args))
                func-impl))))
