(ns clojure-algorithms.project-euler.problem-031.sol2
  "Euler Problem 31: in how many different ways can 2 pounds be made using any
  number of coins? Solved here with bottom-up dynamic programming.")

(defn solution
  "Returns the number of different ways to make pence using any number of
  1, 2, 5, 10, 20, 50, 100 and 200p coins."
  ([] (solution 200))
  ([pence]
   (let [coins [1 2 5 10 20 50 100 200]
         ways (long-array (inc pence))]
     (aset ways 0 1)
     (doseq [coin coins
             i (range coin (inc pence))]
       (aset ways i (+ (aget ways i) (aget ways (- i coin)))))
     (aget ways pence))))
