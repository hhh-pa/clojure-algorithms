(ns clojure-algorithms.bit-manipulation.missing-number
  "Find the missing number in a list of consecutive integers using XOR.")

(defn find-missing-number
  "Returns the missing integer from `nums`, a list of consecutive integers
  with exactly one gap. Works for negative ranges too."
  [nums]
  (let [low (apply min nums)
        high (apply max nums)]
    (reduce (fn [acc i] (bit-xor acc i (nth nums (- i low))))
            high
            (range low high))))
