(ns clojure-algorithms.project-euler.problem-081.sol1
  "Project Euler 81: path sum, two ways.

  Find the minimal path sum from the top left to the bottom right by
  only moving right and down.

  This port takes the matrix as a string (comma-separated rows) instead
  of reading matrix.txt, so the pure logic can be tested with tiny
  data."
  (:require [clojure.string :as str]))

(defn solution
  "Returns the minimal path sum from the top left to the bottom right
  of the matrix described by content, moving only right and down."
  [content]
  (let [grid (mapv #(mapv parse-long (str/split % #","))
                   (str/split-lines (str/trim content)))
        n (count (first grid))]
    (loop [i 0 prev nil]
      (if (= i (count grid))
        (peek prev)
        (let [row (nth grid i)
              cur (loop [j 0 cur []]
                    (if (= j n)
                      cur
                      (let [best (cond (and (zero? i) (zero? j)) 0
                                       (zero? i) (peek cur)
                                       (zero? j) (nth prev j)
                                       :else (min (peek cur) (nth prev j)))]
                        (recur (inc j) (conj cur (+ (nth row j) best))))))]
          (recur (inc i) cur))))))
