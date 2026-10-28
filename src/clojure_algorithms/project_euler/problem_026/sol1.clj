(ns clojure-algorithms.project-euler.problem-026.sol1
  "Euler Problem 26: find the value of d < 1000 for which 1/d contains the
  longest recurring cycle in its decimal fraction part.")

(defn solution
  "Returns the denominator in [numerator, digit] whose unit fraction has the
  longest recurring cycle, found by simulating long division remainders."
  ([] (solution 1 1000))
  ([numerator] (solution numerator 1000))
  ([numerator digit]
   (loop [divide-by numerator
          the-digit 1
          longest 0]
     (if (> divide-by digit)
       the-digit
       (let [[cycle-len _]
             (loop [i 1 seen [] now-divide numerator]
               (cond
                 (> i digit) [nil now-divide]
                 (some #{now-divide} seen) [(count seen) now-divide]
                 :else (recur (inc i)
                              (conj seen now-divide)
                              (mod (* now-divide 10) divide-by))))]
         (if (and cycle-len (> cycle-len longest))
           (recur (inc divide-by) divide-by cycle-len)
           (recur (inc divide-by) the-digit longest)))))))
