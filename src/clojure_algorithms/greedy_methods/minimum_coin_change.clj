(ns clojure-algorithms.greedy-methods.minimum-coin-change
  "Greedy minimum coin change: repeatedly take the largest denomination
  that fits the remaining value.")

(defn find-minimum-change
  "Returns the list of coins (largest first) summing to value.
  Returns [] for zero or negative values."
  [denominations value]
  (let [total (int value)]
    (if (<= total 0)
      []
      (loop [remaining total
             denoms (reverse (sort denominations))
             answer []]
        (if (or (empty? denoms) (zero? remaining))
          answer
          (let [d (first denoms)]
            (if (> d remaining)
              (recur remaining (rest denoms) answer)
              (recur (- remaining d) denoms (conj answer d)))))))))
