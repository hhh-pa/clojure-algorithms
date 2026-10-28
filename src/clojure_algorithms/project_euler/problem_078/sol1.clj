(ns clojure-algorithms.project-euler.problem-078.sol1
  "Project Euler 78: coin partitions.

  Let p(n) represent the number of different ways in which n coins can
  be separated into piles. Find the least value of n for which p(n) is
  divisible by one million. Uses Euler's pentagonal recurrence for the
  partition numbers, computed modulo number.")

(defn solution
  "Returns the least value of n for which p(n) is divisible by number."
  ([] (solution 1000000))
  ([number]
   (loop [i 1 partitions [1]]
     (let [item (loop [j 1 item 0]
                  (let [sign (if (even? j) -1 1)
                        index (quot (- (* 3 j j) j) 2)]
                    (if (> index i)
                      item
                      (let [item (mod (+ item
                                         (* (nth partitions (- i index))
                                            sign))
                                      number)
                            index (+ index j)]
                        (if (> index i)
                          item
                          (recur (inc j)
                                 (mod (+ item
                                         (* (nth partitions (- i index))
                                            sign))
                                      number)))))))]
       (if (zero? item)
         i
         (recur (inc i) (conj partitions item)))))))
