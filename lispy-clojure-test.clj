;;; lispy-clojure-test.clj --- lispy support for Clojure.

;; Copyright (C) 2018 Oleh Krehel

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

(ns lispy-clojure-test
  (:use [clojure.test :only [is deftest]]
        [lispy-clojure :only [add-location-to-defn
                              debug-step-in
                              dest
                              expand-home
                              get-func-args
                              get-func-args-def
                              guess-intent
                              object-members
                              position
                              reader=
                              reval
                              symbol-function]]))

(deftest get-func-args-test
  (is (= (get-func-args (symbol-function 'string?) 1) '[x]))
  (is (= (get-func-args (symbol-function 'to-array) 1) '[coll])))

(deftest get-func-args-def-test
  (is (= (get-func-args-def (symbol-function 'defn) 2)
         '[name & fdecl])))

(deftest object-members-test
  (is (= ((into #{} (object-members Thread)) "run"))))

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
  (is (= (debug-step-in
           '(expand-home (str "/foo" "/bar")))
         {:path "/foo/bar"}))
  (is
    (=
      ((juxt :file :line)
       (debug-step-in
         '(lispy-clojure/add-location-to-def '(def x 1) "/foo/bar.clj" 42)))
      ["/foo/bar.clj" 42])))

(deftest reader=-test
  (is (= (reader= '(map #(* % %) '(1 2 3))
                  '(map #(* % %) '(1 2 3)))))
  (is (= (reader= #"regex" #"regex")))
  (is (not (= #"regex" #"regex"))))

(deftest position-test
  (let [x (read-string "(map #(* % %) as)")
        c (read-string "[as (range 10) bs (map #(* % %) as)]")]
    (is (= (position x c =) nil))
    (is (= (position x c reader=) 3))))

(deftest add-location-to-defn-test
  (is (= (add-location-to-defn
           '(defn fun
              "doc"
              ([x1 x2])
              ([x1]))
           "/foo/bar.clj" 42)
         '(defn fun
            "doc"
            {:l-file "/foo/bar.clj",
             :l-line 42}
            ([x1 x2])
            ([x1]))))
  (is (= (add-location-to-defn
           '(defn fun
              "doc"
              [x]
              x)
           "/foo/bar.clj" 42)
         '(defn fun
            "doc"
            {:l-file "/foo/bar.clj",
             :l-line 42}
            [x]
            x)))
  (is (= (add-location-to-defn
           '(defn fun [x]
              x)
           "/foo/bar.clj" 42)
         '(defn fun
            ""
            {:l-file "/foo/bar.clj",
             :l-line 42}
            [x]
            x))))

(deftest add-location-to-def-test
  (let [e (lispy-clojure/add-location-to-def
            '(def asdf 1) "/foo/bar.clj" 42)]
    (is (= e '(def asdf "" 1)))
    (is (= ((juxt :l-file :l-line) (meta (eval e)))
           ["/foo/bar.clj" 42]))))

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

(clojure.test/run-tests 'lispy-clojure-test)
