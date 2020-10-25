;;; lispy-clojure.clj --- lispy support for ClojureScript.

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

(ns lispy-clojure
  (:require [clojure.repl :as repl]
            [clojure.pprint]
            [clojure.string :as str]))

(defn object-members [obj]
  (loop [o obj
         res ()]
    (if (nil? o)
      res
      (recur
        (js/Object.getPrototypeOf o)
        (concat
          res
          (remove
            (fn [x] (re-find #"__|constructor|[$]" x))
            (vec (js/Object.getOwnPropertyNames o))))))))
