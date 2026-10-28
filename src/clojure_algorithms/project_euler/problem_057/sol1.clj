(ns clojure-algorithms.project-euler.problem-057.sol1
  "Euler Problem 57: in the first one-thousand expansions of the continued
  fraction of sqrt(2), how many fractions contain a numerator with more digits
  than the denominator?")

(defn solution
  "Returns the count of the first n continued-fraction expansions of sqrt(2)
  whose numerator has more digits than the denominator."
  ([] (solution 1000))
  ([n]
   (loop [i 1 prev-num 1N prev-den 1N total 0]
     (if (> i n)
       total
       (let [numerator (+ prev-num (* 2 prev-den))
             denominator (+ prev-num prev-den)]
         (recur (inc i) numerator denominator
                (if (> (count (str numerator)) (count (str denominator)))
                  (inc total)
                  total)))))))
