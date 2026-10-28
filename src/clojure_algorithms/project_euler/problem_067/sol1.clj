(ns clojure-algorithms.project-euler.problem-067.sol1
  "Project Euler 67: maximum path sum II.

  By starting at the top of the triangle and moving to adjacent numbers
  on the row below, find the maximum total from top to bottom.

  This port takes the triangle as a string (one row per line) instead of
  reading triangle.txt, so the pure logic can be tested with tiny data."
  (:require [clojure.string :as str]))

(defn solution
  "Finds the maximum total from top to bottom in the triangle described
  by content, working top-down and accumulating the best total that can
  reach each position."
  [content]
  (let [rows (mapv #(mapv parse-long (str/split (str/trim %) #"\s+"))
                   (str/split-lines content))]
    (loop [prev (first rows) rest-rows (rest rows)]
      (if (empty? rest-rows)
        (apply max prev)
        (let [row (first rest-rows)
              cur (mapv (fn [j v]
                          (+ v (max (if (< j (count prev)) (nth prev j) 0)
                                    (if (pos? j) (nth prev (dec j)) 0))))
                        (range) row)]
          (recur cur (rest rest-rows)))))))
