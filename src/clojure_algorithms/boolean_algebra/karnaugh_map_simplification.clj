(ns clojure-algorithms.boolean-algebra.karnaugh-map-simplification
  "Naive two-variable Karnaugh map sum-of-products listing."
  (:require [clojure.string :as str]))

(defn- py-truthy?
  "Python-like truthiness for numbers, strings and nil."
  [x]
  (cond
    (nil? x) false
    (false? x) false
    (true? x) true
    (number? x) (not (zero? x))
    (string? x) (boolean (seq x))
    :else true))

(defn simplify-kmap
  "Lists the minterms set in a 2x2 K-map as e.g. \"A'B + AB' + AB\"."
  [kmap]
  (->> (for [[a row] (map-indexed vector kmap)
             [b item] (map-indexed vector row)
             :when (py-truthy? item)]
         (str (if (zero? a) "A'" "A") (if (zero? b) "B'" "B")))
       (str/join " + ")))
