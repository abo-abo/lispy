;;; lispy-clojure.cljs --- lispy support for ClojureScript.

;; Copyright (C) 2020 Oleh Krehel

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
  (:require
   [goog.object]))

(defn object-members [obj]
  (let [obj (if (string? obj)
              (js/Object.getPrototypeOf obj)
              obj)]
    (loop [o obj
           res ()]
      (if (nil? o)
        (into () (set res))
        (recur
          (js/Object.getPrototypeOf o)
          (concat
            res
            (map
              (fn [n]
                (let [tp (type (goog.object/get obj n))]
                  (if (= tp js/Function)
                    n
                    (str "-" n))))
              (remove
                (fn [x] (or (re-find #"__|constructor|[$]" x)
                            (re-matches #"[0-9]+" x)))
                (vec (js/Object.getOwnPropertyNames o))))))))))

(defn shadow-map []
  (or
    (aget js/window "shadows")
    (set! (. js/window -shadows) {})))

(defn shadow-unmap [nspc]
  (set! (. js/window -shadows) {}))

(defn shadow-def
  "Give SYM in *ns* shadow value EXPR."
  [sym expr]
  (set! (. js/window -shadows)
        (assoc (shadow-map) (name sym) expr))
  expr)
