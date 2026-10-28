(ns clojure-algorithms.maths.aliquot-sum
  "Aliquot sum of a positive integer.
  See https://en.wikipedia.org/wiki/Aliquot_sum")

(defn aliquot-sum
  "Returns the aliquot sum of `input-num`: the sum of all natural numbers
  less than `input-num` that divide it evenly. For example, the aliquot
  sum of 15 is 1 + 3 + 5 = 9."
  [input-num]
  (when-not (integer? input-num)
    (throw (ex-info "Input must be an integer" {:type :ValueError})))
  (when (<= input-num 0)
    (throw (ex-info "Input must be positive" {:type :ValueError})))
  (reduce + 0 (filter #(zero? (rem input-num %)) (range 1 (inc (quot input-num 2))))))
