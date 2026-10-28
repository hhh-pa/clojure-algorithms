(ns clojure-algorithms.project-euler.problem-018.solution
  "Project Euler Problem 18: maximum path sum I.

  By starting at the top of the triangle below and moving to adjacent
  numbers on the row below, the maximum total from top to bottom is 23.

      3
      7 4
      2 4 6
      8 5 9 3

  That is, 3 + 7 + 4 + 9 = 23.

  Find the maximum total from top to bottom of the triangle in the
  triangle file."
  (:require [clojure.string :as str]))

(defn solution
  "Finds the maximum total from top to bottom of the triangle described
  by content, where content is the text of the triangle file (one row
  per line, space-separated numbers)."
  [content]
  (let [a (mapv (fn [line] (mapv #(Integer/parseInt %) (str/split line #" ")))
                (str/split-lines content))]
    (loop [i 1 prev (nth a 0)]
      (if (>= i (count a))
        (apply max prev)
        (let [row (nth a i)
              cur (mapv (fn [j v]
                          (+ v (max (if (= j (count prev)) 0 (nth prev j))
                                    (if (> j 0) (nth prev (dec j)) 0))))
                        (range (count row)) row)]
          (recur (inc i) cur))))))
