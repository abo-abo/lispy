;;; lispy-clojure.clj --- lispy support for Clojure.

;; Copyright (C) 2015-2017 Oleh Krehel

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
            [clojure.pprint]
            [clojure.java.io :as io])
  (:use [clojure.test :only (is deftest)]
        [cemerick.pomegranate :only (add-dependencies)])
  (:import (java.io File LineNumberReader InputStreamReader
                    PushbackReader FileInputStream)
           (clojure.lang RT Reflector)))

(defn use-package [name version]
  (add-dependencies
    :coordinates [[name version]]
    :repositories (merge cemerick.pomegranate.aether/maven-central
                         {"clojars" "https://clojars.org/repo"})))

(use-package 'compliment "0.3.5")
(require '[compliment.core :as compliment])

(use-package 'me.raynes/fs "1.4.6")
(require '[me.raynes.fs :as fs])

(defmacro xcond [& clauses]
  "Common Lisp style `cond'.

It's more structured than `cond', thus exprs that use it are lot more
malleable to refactoring."
  (when clauses
    (let [clause (first clauses)]
      (if (= (count clause) 1)
        `(or ~(first clause)
             (xcond
               ~@(next clauses)))
        `(if ~(first clause)
           (do ~@(next clause))
           (xcond
             ~@(next clauses)))))))

(defn fetch-packages []
  (xcond ((fs/exists? "deps.edn")
          (println "fixme"))
         ((fs/exists? "project.clj")
          (let [deps (->> (slurp "project.clj")
                          (read-string)
                          (drop 3)
                          (partition 2)
                          (map vec)
                          (into {})
                          :dependencies)]
            (doseq [[name ver] deps]
              (use-package name ver))))
         (:else
          (throw
            (ex-info "Found no project.clj or deps.edn"
                     {:cwd fs/*cwd*})))))

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

(defn macro? [x]
  (:macro (meta (resolve x))))

(defn arity [args]
  (if (some #{'&} args)
    1000
    (count args)))

(defn flatten-expr
  "Flatten a function call EXPR by substituting the arguments."
  [expr]
  (let [func-name (first expr)
        args (rest expr)
        func-def (symbol-function func-name)
        func-doc (when (string? (nth func-def 2))
                   (nth func-def 2))
        func-rest (drop (if func-doc 3 2) func-def)
        func-rest (if (map? (first func-rest))
                    (rest func-rest)
                    func-rest)
        func-bodies (if (vector? (first func-rest))
                      (list func-rest)
                      func-rest)
        func-body (first (filter #(>= (arity (first %)) (count args))
                                 (sort (fn [a b] (< (arity (first a))
                                                    (arity (first b))))
                                       func-bodies)))
        func-args (first func-body)
        func-impl (rest func-body)]
    (cons 'let
          (cons (vec (if (some #{'&} [func-args])
                       (vector func-args (vec args))
                       (apply concat
                              (filter (fn [[a b]]
                                        (not (= a b)))
                                      (partition
                                        2 (interleave func-args args))))))
                func-impl))))

(defn quote-maybe
  "Quote X that isn't self-quoting, like symbol or list."
  [x]
  (if (fn? x)
    x
    (if (or (symbol? x)
            (list? x))
      (list 'quote x)
      x)))

(defn dest
  "Transform `let'-style BINDINGS into a sequence of `def's."
  [bindings]
  (let [bs (partition 2 (destructure bindings))
        as (filterv
             #(not (re-matches #"^(vec|map|seq|first)__.*" (name %)))
             (map first bs))]
    (concat '(do)
            (map (fn [[name val]]
                   `(def ~name ~val))
                 bs)
            [(zipmap (map keyword as) as)])))

(defn get-func-args-defn [func-def n-args]
  (let [func-doc (when (string? (nth func-def 2))
                   (nth func-def 2))
        func-rest (drop (if func-doc 3 2) func-def)
        func-rest (if (map? (first func-rest))
                    (rest func-rest)
                    func-rest)
        func-bodies (if (vector? (first func-rest))
                      (list func-rest)
                      func-rest)
        func-body (first (filter #(>= (arity (first %)) n-args)
                                 (sort (fn [a b] (< (arity (first a))
                                                    (arity (first b))))
                                       func-bodies)))
        func-args (first func-body)]
    func-args))

(defn get-func-args-def [func-def n-args]
  (get-func-args-defn (nth func-def 2) n-args))

(defn get-func-args [func-def n-args]
  (xcond ((#{'defn 'defmacro} (first func-def) )
          (get-func-args-defn func-def n-args))
         ((= (first func-def) 'def)
          (get-func-args-def func-def n-args))))

(deftest get-func-args-test
  (is (= (get-func-args (symbol-function 'string?) 1) '[x]))
  (is (= (get-func-args (symbol-function 'to-array) 1) '[coll])))

(defn debug-step-in
  "Evaluate the function call arugments and sub them into function arguments."
  [expr]
  (let [func-name (first expr)
        func-ns (:ns (meta (resolve func-name)))
        args (rest expr)
        func-def (symbol-function func-name)
        func-args (get-func-args func-def (count args))
        eval-form (if (macro? func-name)
                    (cons 'do
                          (cons `(def ~'args ~(quote-maybe args))
                                (map (fn [[name val]]
                                       `(def ~name ~val))
                                     (partition 2 (destructure [func-args 'args])))))
                    (dest (vector func-args (vec (rest expr)))))]
    (if (= func-ns *ns*)
      eval-form
      (let [vals-map (eval eval-form)]
        `(do
           (in-ns '~(symbol (str func-ns)))
           ~@(map (fn [s] `(def ~s ~ ((keyword s) vals-map)))
                  func-args))))))

(defn object-methods [sym]
  (distinct
    (map #(.getName %)
         (xcond
           ((instance? java.lang.Class sym)
            (. sym getMethods))

           ((instance? java.lang.Object sym)
            (. (type sym) getMethods))))))

(defn object-fields [sym]
  (map #(str "-" (.getName %))
       (.getFields (type sym))))

(defn object-members [sym]
  (concat (object-fields sym)
          (object-methods sym)))

(deftest object-members-test
  (is ((into #{} (object-members Thread)) "run")))

(defn get-meth [obj method-name]
  (first (filter #(= (.getName %) method-name)
                 (.getMethods (type obj)))))

(defn method-signature [obj method-name]
  (str (get-meth obj method-name)))

(defn get-ctors [obj]
  (. obj getDeclaredConstructors))

(defn format-ctor [s]
  (let [[_ name args] (re-find #"public (.*)\((.*)\)" s)]
    (str name
         "."
         (if (= args "")
           ""
           (str " " (clojure.string/replace args #"," " "))))))

(defn ctor-args [sym]
  (clojure.string/join
    "\n"
    (map #(str "(" % ")")
         (map format-ctor
              (map str (get-ctors sym))))))

(defn resolve-sym [sym]
  (xcond
    [(symbol? sym)
     (if (special-symbol? sym)
       'special
       (or
         (resolve sym)
         (first (keep #(ns-resolve % sym) (all-ns)))
         (if-let [val (try (load-string (str sym)) (catch Exception e))]
           (list 'variable (str val)))))]

    [(keyword? sym) 'keyword]

    [:else 'unknown]))

(defn arglist [sym]
  (let [rsym (resolve-sym sym)]
    (xcond
      [(= 'special rsym)
       (->> (with-out-str
              (eval (list 'clojure.repl/doc sym)))
            (re-find #"\(.*\)")
            read-string rest
            (map str)
            (clojure.string/join " ")
            (format "[%s]")
            list)]
      [:else
       (let [args (map str (:arglists (meta rsym)))]
         (if (empty? args)
           (condp #(%1 %2) (eval sym)
             map? "[key]"
             set? "[key]"
             vector? "[idx]"
             "is uncallable")
           args))])))

(deftest dest-test
  (is (= (eval (dest '[[x y] (list 1 2 3)]))
         {:x 1, :y 2}))
  (is (= (eval (dest '[[x & y] [1 2 3]]))
         {:x 1, :y '(2 3)}))
  (is (= (eval (dest '[[x y] (list 1 2 3) [a b] [y x]]))
         {:x 1, :y 2, :a 2, :b 1}))
  (is (= (eval (dest '[[x y z] [1 2]]))
         {:x 1, :y 2, :z nil}))
  (is (= (eval (dest '[[x & tail :as all] [1 2 3]]))
         {:x 1,
          :tail '(2 3),
          :all [1 2 3]}))
  (is (= (eval (dest '[[x & tail :as all] "Clojure"]))
         {:x \C,
          :tail '(\l \o \j \u \r \e),
          :all "Clojure"}))
  (is (= (eval (dest '[{x 1 y 2} {1 "one" 2 "two" 3 "three"}]))
         {:x "one", :y "two"}))
  (is (= (eval (dest '[{x 1 y 2 :or {x "one" y "two"} :as all} {2 "three"}]))
         {:all {2 "three"},
          :x "one",
          :y "three"}))
  (is (= (eval (dest '[{:keys [x y]} {:x "one" :z "two"}]))
         {:x "one", :y nil}))
  (is (= (eval (dest '[{:strs [x y]} {"x" "one" "z" "two"}]))
         {:x "one", :y nil}))
  (is (= (eval (dest '[{:syms [x y]} {'x "one" 'z "two"}]))
         {:x "one", :y nil})))

(deftest debug-step-in-test
  (is (= (eval (debug-step-in
                 '(expand-home (str "/foo" "/bar"))))
         {:path "/foo/bar"})))

(defmacro ok
  "On getting an Exception, just print it."
  [& body]
  `(try
     (eval '~@body)
     (catch Exception ~'e (.getMessage ~'e))))

(defn classpath []
  (map #(.getAbsolutePath (java.io.File. (.toURI %)))
       (.getURLs (java.lang.ClassLoader/getSystemClassLoader))))

(defn reader=
  "Equality accounting for reader-generated symbols."
  [a b]
  (try
    (xcond
      ((and (symbol? a) (symbol? b))
       (or
         (= a b)
         (and
           (re-find #"[0-9]+#$" (name a))
           (re-find #"[0-9]+#$" (name b))
           true)))

      ((and (empty? a) (empty? b))
       true)

      (:else
       (and
         (reader= (first a) (first b))
         (reader= (rest a) (rest b)))))
    (catch Exception e
      (= a b))))

(deftest reader=-test
  (is (= (reader= '(map #(* % %) '(1 2 3))
                  '(map #(* % %) '(1 2 3))))))

(defn position [x coll equality]
  (letfn [(iter [i coll]
            (xcond
              ((empty? coll) nil)
              ((equality x (first coll))
               i)
              (:else
               (recur (inc i) (rest coll)))))]
    (iter 0 coll)))

(deftest position-test
  (let [x (read-string "(map #(* % %) as)")
        c (read-string "[as (range 10) bs (map #(* % %) as)]")]
    (is (= (position x c =) nil))
    (is (= (position x c reader=) 3))))

(defn guess-intent [expr context]
  (if (not (or (list? expr)
               (vector? expr)))
    expr
    (let [idx (position expr context reader=)]
      (xcond
        ((#{'defproject} (first expr))
         `(fetch-packages))
        ((nil? idx)
         expr)
        ((and (vector? context)
              (not (symbol? (context idx)))
              (or (symbol? (context (dec idx)))
                  (vector? (context (dec idx)))))
         (dest
           (take 2 (drop (- idx 1) context))))
        ((or (nil? context)
             (reader= expr context))
         expr)
        ((and (#{'doseq 'for} (first context))
              (vector? expr)
              (= 2 (count expr)))
         `(do (def ~(first expr) (first ~(second expr)))
              {~(keyword (first expr)) ~(first expr)}))
        ((and (#{'dotimes} (first context))
              (vector? expr)
              (= 2 (count expr)))
         `(do (def ~(first expr) 0)
              {~(keyword (first expr)) 0}))
        ((#{'-> '->> 'doto} (first context))
         (take (inc idx) context))
        (:t
         expr)))))

(defn add-location-to-defn [expr file line]
  (when (and (list? expr)
             (= 'defn (first expr))
             file line)
    (let [arglist-pos (first (keep-indexed
                               (fn [i x] (if (vector? x) i))
                               expr))
          expr-head (take arglist-pos expr)
          expr-tail (drop arglist-pos expr)
          expr-doc (or (first (filter string? expr-head)) "")
          expr-map (or (first (filter map? expr-head)) {})]
      `(defn ~(nth expr 1)
         ~expr-doc
         ~(merge {:l-file file
                  :l-line line}
                 expr-map)
         ~@expr-tail))))

(defn reval [e-str context-str & {:keys [file line]}]
  (let [expr (read-string e-str)
        context (try
                  (read-string context-str)
                  (catch Exception _))
        full-expr (read-string (format "[%s]" e-str))
        expr1 (xcond
                ((= (count full-expr) 2)
                 (dest full-expr))
                ((add-location-to-defn expr file line))
                (:else
                 (guess-intent expr context)))
        expr2 `(try
                 (do ~expr1)
                 (catch Exception ~'e
                   (clojure.core/str "error: " ~ 'e)))]
    (eval expr2)))

(defn location [symbol]
  (let [m (meta (resolve symbol))]
    (when (:l-file m)
      (list (:l-file m) (:l-line m)))))

(defn pp [expr]
  (with-out-str
    (clojure.pprint/pprint
      expr)))

(deftest guess-intent-test
  (is (= (guess-intent '(defproject) nil) '(lispy-clojure/fetch-packages)))
  (is (= (guess-intent 'x '[x y]) 'x))
  (is (= (guess-intent '*ns* '*ns*) '*ns*)))

(deftest reval-test
  (let [s "(->> 5
               (range)
               (map (fn [x] (* x x)))
               (map (fn [x] (+ x x))))"]
    (is (= (reval "(range)" s)
           '(0 1 2 3 4)))
    (is (= (reval "(map (fn [x] (* x x)))" s)
           '(0 1 4 9 16)))
    (is (= (reval "(map (fn [x] (+ x x)))" s)
           '(0 2 8 18 32))))
  (is (= (reval "x (+ 2 2)" "[x (+ 2 2)]") {:x 4}))
  (is (= (reval "(+ 2 2)" "[x (+ 2 2)]") {:x 4}))
  (is (= (reval "(+ 2 2)" "[x (+ 1 2) y (+ 2 2)]") {:y 4}))
  (is (= (reval "(range 5)" "[[x & y] (range 5)]")
         {:x 0, :y '(1 2 3 4)}))
  (is (= (reval "[c [(* 2 21) 0]]" "(doseq [c [(* 2 21) 0]]\n  ())") {:c 42}))
  (is (= (reval "[i 3]" "(dotimes [i 3])") {:i 0}))
  (is (= (reval "[a b] (range 5)" "[[a b] (range 5)]") {:a 0, :b 1}))
  (let [js "(doto (new java.util.HashMap) (.put \"a\" 1) (.put \"b\" 2))"]
    (is (= (reval "(.put \"a\" 1)" js) {"a" 1}))
    (is (= (reval "(.put \"b\" 2)" js) {"a" 1, "b" 2}))))

(defn all-docs [ns]
  (clojure.string/join
    "::"
    (->> (filter (fn [v]
                   (and (var? v)
                        (fn? (deref v))))
                 (vals (ns-map ns)))
         (map
           (fn [v]
             (let [m (meta v)]
               (str v "\n" (:arglists m) "\n" (:doc m))))))))

(defn complete [prefix]
  (compliment/completions
    prefix
    {:context :same :plain-candidates true}))

(clojure.test/run-tests 'lispy-clojure)
