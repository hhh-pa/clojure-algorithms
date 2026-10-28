(ns clojure-algorithms.project-euler.problem-206.sol1
  "Project Euler Problem 206: concealed square.

  Finds the unique positive integer whose square has the form
  1_2_3_4_5_6_7_8_9_0. Since the square ends in 0 the root ends in 0, and
  since the remaining square ends in 9 the root ends in 3 or 7. Searches
  down from 138902663 alternating steps so the last digit stays 3 or 7.")

(defn is-square-form
  "Determines if num is of the form 1_2_3_4_5_6_7_8_9."
  [num]
  (loop [n num
         digit 9]
    (if (<= n 0)
      true
      (if (not= (mod n 10) digit)
        false
        (recur (quot n 100) (dec digit))))))

(defn solution
  "Returns the first integer whose square is of the form
  1_2_3_4_5_6_7_8_9_0."
  []
  (loop [num 138902663]
    (if (is-square-form (* num num))
      (* num 10)
      (recur (if (= (mod num 10) 3) (- num 6) (- num 4))))))
