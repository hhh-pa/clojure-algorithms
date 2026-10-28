(ns clojure-algorithms.boolean-algebra.nor-gate
  "NOR gate with a printable truth table."
  (:require [clojure.string :as str]))

(defn nor-gate
  "Returns the NOR of two inputs (1 only when both are 0)."
  [input-1 input-2]
  (if (and (zero? input-1) (zero? input-2)) 1 0))

(defn- center
  [s w]
  (let [s (str s) pad (- w (count s)) l (quot pad 2) r (- pad l)]
    (str (apply str (repeat l " ")) s (apply str (repeat r " ")))))

(defn- table-row
  [items]
  (str "| " (str/join " | " (map #(center % 8) items)) " |"))

(defn truth-table
  "Renders the truth table of a two-input gate function."
  [func]
  (str/join "\n"
            (concat ["Truth Table of NOR Gate:"
                     (table-row ["Input 1" "Input 2" "Output"])]
                    (for [i [0 1] j [0 1]]
                      (table-row [i j (func i j)])))))
