(ns clojure-algorithms.project-euler.problem-099.sol1
  "Project Euler 99: largest exponential.

  Given base/exponent pairs, one per line, determine which line number
  has the greatest numerical value. Comparisons use x * log10(a) so the
  huge powers are never computed directly.

  This port takes the pairs as a string instead of reading base_exp.txt,
  so the pure logic can be tested with tiny data."
  (:require [clojure.string :as str]))

(defn solution
  "Returns the 1-based line number with the greatest numerical value in
  content."
  [content]
  (loop [lines (str/split-lines content) i 1 largest 0.0 result 0]
    (if (empty? lines)
      result
      (let [[a x] (map parse-long (str/split (first lines) #","))
            value (* x (Math/log10 a))]
        (if (> value largest)
          (recur (rest lines) (inc i) value i)
          (recur (rest lines) (inc i) largest result))))))
