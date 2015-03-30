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
  (:require [clojure.repl :as repl]
            [clojure.java.io :as io])
  (:import (java.io File LineNumberReader InputStreamReader
                    PushbackReader FileInputStream)
           (clojure.lang RT Reflector)))

(defn expand-home
  [path]
  (if (.startsWith path "~")
    (let [sep (.indexOf path File/separator)]
      (str (io/file (System/getProperty "user.home")
                    (subs path (inc sep)))))
    path))

(defn source-fn
  "Returns a string of the source code for the given symbol, if it can
  find it.  This requires that the symbol resolve to a Var defined in
  a namespace for which the .clj is in the classpath.  Returns nil if
  it can't find the source.

  Example: (source-fn 'filter)"
  [x]
  (when-let [v (resolve x)]
    (when-let [filepath (expand-home (:file (meta v)))]
      (when-let [strm (or (.getResourceAsStream (RT/baseLoader) filepath)
                          (FileInputStream. filepath))]
        (with-open [rdr (LineNumberReader. (InputStreamReader. strm))]
          (dotimes [_ (dec (:line (meta v)))] (.readLine rdr))
          (let [text (StringBuilder.)
                pbr (proxy [PushbackReader] [rdr]
                      (read [] (let [i (proxy-super read)]
                                 (.append text (char i))
                                 i)))]
            (if (= :unknown *read-eval*)
              (throw (IllegalStateException. "Unable to read source while *read-eval* is :unknown."))
              (read (PushbackReader. pbr)))
            (str text)))))))

(defn symbol-function
  "Return the source code for function SYM."
  [sym]
  (read-string
   (source-fn
    sym)))

(defn arity [args]
  (if (some #{'&} args)
    1000
    (count args)))

(defn flatten-expr
  "Flatten a function call EXPR by substituting the arguments."
  [expr]
  (let [func-name (first expr)
        args (rest expr)
        func-def (lispy-clojure/symbol-function func-name)
        func-doc (when (string? (nth func-def 2))
                   (nth func-def 2))
        func-rest (drop (if func-doc 3 2) func-def)
        func-rest (if (map? (first func-rest))
                    (rest func-rest)
                    func-rest)
        func-bodies (if (vector? (first func-rest))
                      (list func-rest)
                      func-rest)
        func-body (first (filter #(>= (lispy-clojure/arity (first %)) (count args))
                                 (sort (fn [a b] (< (lispy-clojure/arity (first a))
                                                    (lispy-clojure/arity (first b))))
                                       func-bodies)))
        func-args (first func-body)
        func-impl (rest func-body)]
    (cons 'let
          (cons (vec (if (some #{'&} func-args)
                       (vector func-args (vec args))
                       (interleave func-args args)))
                func-impl))))
