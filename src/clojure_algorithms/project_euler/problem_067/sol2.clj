(ns clojure-algorithms.project-euler.problem-067.sol2
  "Project Euler 67: maximum path sum II (bottom-up variant).

  By starting at the top of the triangle and moving to adjacent numbers
  on the row below, find the maximum total from top to bottom.

  This port takes the triangle as a string (one row per line) instead of
  reading triangle.txt, so the pure logic can be tested with tiny data."
  (:require [clojure.string :as str]))

(defn solution
  "Finds the maximum total from top to bottom in the triangle described
  by content, folding the triangle bottom-up: each cell absorbs the
  larger of the two cells below it."
  [content]
  (let [rows (mapv #(mapv parse-long (str/split (str/trim %) #"\s+"))
                   (str/split-lines content))]
    (loop [triangle rows]
      (if (= 1 (count triangle))
        (ffirst triangle)
        (let [last-row (peek triangle)
              curr-row (peek (pop triangle))
              updated (mapv (fn [j v]
                              (+ v (max (nth last-row j)
                                        (nth last-row (inc j)))))
                            (range) curr-row)]
          (recur (conj (pop (pop triangle)) updated)))))))
