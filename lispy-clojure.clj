;;; lispy-clojure.clj --- lispy support for Clojure.

;; Copyright (C) 2015-2018 Oleh Krehel

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

(ns lispy.clojure
  (:require [clojure.repl :as repl]
            [clojure.pprint]
            [clojure.java.io :as io]
            [clojure.string :as str])
  (:import
   (java.io File LineNumberReader InputStreamReader
            PushbackReader FileInputStream)
   (clojure.lang RT)))

(defmacro xcond
  "Common Lisp style `cond'.

It's more structured than `cond', thus exprs that use it are lot more
malleable to refactoring."
  [& clauses]
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

(defn file-exists? [f]
  (. (io/file f) exists))

(defn expand-file-name [name dir]
  (. (io/file dir name) getCanonicalPath))

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
  (let [v (resolve x)
        m (and v (meta v))
        file (or (:l-file m) (:file m))
        line (or (:l-line m) (:line m))]
    (when (and file line (> line 1))
      (let [filepath (expand-home file)
            strm (or (.getResourceAsStream (RT/baseLoader) filepath)
                     (FileInputStream. filepath))]
        (with-open [rdr (LineNumberReader. (InputStreamReader. strm))]
          (dotimes [_ (dec line)] (.readLine rdr))
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
  (let [body (nth func-def 2)]
    (assert (= (first body) 'fn))
    (let [args (first (filter vector? body))
          args-count (count (vec (remove '#{& &form &env} args)))]
      (assert (or (= args-count n-args)
                  (and (< args-count n-args)
                       ((set args) '&))))
      (vec (remove '#{&form &env} args)))))

(defn get-func-args [func-def n-args]
  (xcond ((#{'defn 'defmacro} (first func-def))
          (get-func-args-defn func-def n-args))
         ((= (first func-def) 'def)
          (get-func-args-def func-def n-args))))

(defn shadow-map []
  (or (ns-resolve *ns* 'shadows)
      (intern *ns* 'shadows {})))

(defn shadow-unmap [nspc]
  ;; (ns-unmap nspc 'shadows)
  (intern nspc 'shadows {}))

(defmacro with-shadows [& forms]
  `(let ~(vec (mapcat (fn [[k _]] [(symbol k) `((shadow-map) ~k)])
                      (deref (shadow-map))))
     ~@forms))

(defn shadow-def
  "Give SYM in *ns* shadow value EXPR.

  (with-shadows SYM) can be used to retrieve this value."
  [sym expr]
  (intern
    *ns*
    'shadows
    (assoc (deref (shadow-map)) (name sym) expr)))

(defn shadow-dest
  "Transform `let'-style BINDINGS into a sequence of `shadow-def's."
  ([bindings]
   (shadow-dest bindings *ns*))
  ([bindings nspc]
   (let [[_do & forms] (dest bindings)
         [defs out] (partition-by map? forms)]
     `(let ~(vec (mapcat (fn [[_ n v]] [n v]) defs))
        ~@(when (not= *ns* nspc)
            `((in-ns '~(ns-name nspc))))
        ~@(map
            (fn [x]
              `(shadow-def '~(second x) ~(second x)))
            defs)
        ~@out))))

(defn debug-step-in
  "Evaluate the function call arugments and sub them into function arguments."
  [expr]
  (let [func-name (first expr)
        args (vec (rest expr))
        func-def (symbol-function func-name)
        func-args (get-func-args func-def (count args))
        func-ns (:ns (meta (resolve func-name)))
        eval-form (shadow-dest
                    [func-args (if (macro? func-name)
                                 (list 'quote args)
                                 args)]
                    func-ns)]
    (eval
      `(with-shadows
         ~eval-form))))

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

(defmacro object-members [ob]
  `(with-shadows
     (concat (object-fields ~ob)
             (object-methods ~ob))))

(defn get-meth [obj method-name]
  (first (filter #(= (.getName %) method-name)
                 (.getMethods (type obj)))))

(defn method-signature [obj method-name]
  (str (get-meth obj method-name)))

(defn get-ctors [obj]
  (. obj getDeclaredConstructors))

(defn format-ctor [s]
  (let [[_ name args] (re-find #"(?:public|protected) (.*)\((.*)\)" s)]
    (str name
         "."
         (if (= args "")
           ""
           (str " " (str/replace args #"," " "))))))

(defn ctor-args [sym]
  (str/join
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
         (when-let [val (try (load-string (str sym)) (catch Exception _e))]
           (list 'variable (str val)))))]

    [(keyword? sym) 'keyword]

    [:else 'unknown]))

(defn class-name [cls]
  (str/replace (str cls) #"class " ""))

(defn class-method-static? [method]
  (java.lang.reflect.Modifier/isStatic (.getModifiers method)))

(defn class-methods [cname]
  (load-string (format "(.getMethods %s)" cname)))

(defn find-method [sym]
  (let [[cname mname] (str/split (str sym) #"/")
        methods (->>
                  (and cname
                       (class-methods cname))
                  (filter #(= (.getName %) mname)))]
    (first methods)))

(defn arglist [sym]
  (let [rsym (resolve-sym sym)]
    (xcond
      ((= 'special rsym)
       (->> (with-out-str
              (eval (list #'repl/doc sym)))
            (re-find #"\(.*\)")
            read-string rest
            (map str)
            (str/join " ")
            (format "[%s]")
            list))
      ((and (nil? rsym) (re-find #"/" (str sym)))
       (let [method (find-method sym)
             args (->> method
                       (.getParameterTypes)
                       (map class-name)
                       (str/join " "))]
         (format "(%s [%s]) -> %s" sym args
                 (class-name (. method getReturnType)))))
      (:else
       (let [args (map str (:arglists (meta rsym)))]
         (if (empty? args)
           (condp #(%1 %2) (eval sym)
             map? "[key]"
             set? "[key]"
             vector? "[idx]"
             "is uncallable")
           args))))))

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

      ((and (instance? java.util.regex.Pattern a)
            (instance? java.util.regex.Pattern b))
       (= (. a toString)
          (. b toString)))

      ((and (empty? a) (empty? b))
       true)

      (:else
       (and
         (reader= (first a) (first b))
         (reader= (rest a) (rest b)))))
    (catch Exception _e
      (= a b))))

(defn position [x coll equality]
  (letfn [(iter [i coll]
            (xcond
              ((empty? coll) nil)
              ((equality x (first coll))
               i)
              (:else
               (recur (inc i) (rest coll)))))]
    (iter 0 coll)))

(defn guess-intent [expr context]
  (if (not (or (list? expr)
               (vector? expr)))
    expr
    (let [idx (position expr context reader=)]
      (xcond
        ((nil? idx)
         expr)
        ;; [x |(+ 1 2) y (+ 3 4)] => {:x 3}
        ;; TODO: would be better to have 1 level higher context, so that we just check
        ;; (= (first context) 'let)
        ((and (vector? context)
              (= 0 (rem (count context) 2))
              (= 0 (rem (inc idx) 2))
              (every? (some-fn symbol? vector? map?) (take-nth 2 context)))
         (shadow-dest
           (take 2 (drop (- idx 1) context))))
        ((or (nil? context)
             (reader= expr context))
         expr)
        ((and (#{'doseq 'for} (first context))
              (vector? expr)
              (= 2 (count expr)))
         (shadow-dest
           [(first expr) (first (eval `(with-shadows ~(second expr))))]))
        ((and (#{'dotimes} (first context))
              (vector? expr)
              (= 2 (count expr)))
         (shadow-dest
           [(first expr) 0]))
        ((#{'-> '->> 'doto} (first context))
         (take (inc idx) context))
        (:t
         expr)))))

(defn add-location-to-defn [expr file line]
  (when (and (list? expr)
             (= 'defn (first expr))
             file line)
    (let [arglist-pos (first (keep-indexed
                               (fn [i x] (when (or (vector? x) (list? x))
                                           i))
                               expr))
          expr-head (take arglist-pos expr)
          expr-tail (drop arglist-pos expr)
          expr-doc (or (first (filter string? expr-head)) "")
          expr-map (or (first (filter map? expr-head)) {})]
      `(~'defn ~(nth expr 1)
        ~expr-doc
        ~(merge {:l-file file
                 :l-line line}
                expr-map)
        ~@expr-tail))))

(defn add-location-to-def
  [[_def name & args] file line]
  (apply list
         _def
         (with-meta
           name
           {:l-file file
            :l-line line})
         (if (> (count args) 1)
           args
           (cons "" args))))

(defn add-location-to-deflike [expr file line]
  (when (and file line (list? expr))
    (xcond ((= (first expr) 'def)
            (add-location-to-def expr file line))
           ((= (first expr) 'defn)
            (add-location-to-defn expr file line)))))

(defn read-string-all
  "Read all objects from the string S."
  [s]
  (let [reader (java.io.PushbackReader.
                 (java.io.StringReader. s))]
    (loop [res []]
      (if-let [x (try (read reader)
                      (catch Exception _e))]
        (recur (conj res x))
        res))))

(defn reval [e-str context-str & {:keys [file line]}]
  (let [expr (read-string e-str)
        context (try
                  (read-string context-str)
                  (catch Exception _))
        full-expr (read-string (format "[%s]" e-str))
        expr1 (xcond
                ((nil? context-str)
                 (cons 'do full-expr))
                ((= (count full-expr) 2)
                 (shadow-dest full-expr))
                ((add-location-to-deflike expr file line))
                (:else
                 (guess-intent expr context)))]
    (eval `(with-shadows
             (try
               (do ~expr1)
               (catch Exception ~'e
                 (clojure.core/str "error: " ~ 'e)))))))

(defn file->elisp [f]
  (if (file-exists? f)
    f
    (. (io/resource f) getPath)))

(defn location [sym]
  (let [rs (resolve sym)
        m (meta rs)]
    (xcond ((:l-file m)
            (list (:l-file m) (:l-line m)))
           ((and (:file m) (not (re-matches #"^/tmp/" (:file m))))
            (list (file->elisp (:file m)) (:line m))))))

(defn pp [expr]
  (with-out-str
    (clojure.pprint/pprint
      expr)))

(defn all-docs [ns]
  (str/join
    "::"
    (->> (filter (fn [v]
                   (and (var? v)
                        (fn? (deref v))))
                 (vals (ns-map ns)))
         (map
           (fn [v]
             (let [m (meta v)]
               (str v "\n" (:arglists m) "\n" (:doc m))))))))
