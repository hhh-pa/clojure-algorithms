(ns clojure-algorithms.boolean-algebra.karnaugh-map-simplification
  "Naive two-variable Karnaugh map sum-of-products listing."
  (:require [clojure.string :as str]))

(defn- cell-set?
  "Mimics Python truthiness for a K-map cell: numbers count when non-zero,
  strings and collections count when non-empty, nil and false never count."
  [v]
  (cond (or (nil? v) (false? v)) false
        (number? v) (not (zero? v))
        (string? v) (not (empty? v))
        (coll? v) (not (empty? v))
        :else true))

(defn simplify-kmap
  "Lists the minterms set in a 2x2 K-map as e.g. \"A'B + AB' + AB\"."
  [kmap]
  (->> (for [[a row] (map-indexed vector kmap)
             [b item] (map-indexed vector row)
             :when (cell-set? item)]
         (str (if (zero? a) "A'" "A") (if (zero? b) "B'" "B")))
       (str/join " + ")))

(defn main
  "Prints a sample K-map and its simplified expression."
  []
  (let [kmap [[0 1] [1 1]]]
    (doseq [row kmap]
      (println (str "[" (str/join ", " row) "]")))
    (println "Simplified Expression:")
    (println (simplify-kmap kmap))))
