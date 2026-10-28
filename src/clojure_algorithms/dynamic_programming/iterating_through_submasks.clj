(ns clojure-algorithms.dynamic-programming.iterating-through-submasks
  "List all non-zero submasks of bitmask `mask`.
  Ported from TheAlgorithms/Python
  dynamic_programming/iterating_through_submasks.py.")

(defn list-of-submasks
  "Vector of submasks of positive integer `mask`, mask itself first."
  [mask]
  (assert (and (integer? mask) (pos? mask))
          (str "mask needs to be positive integer, your input " mask))
  (loop [sub mask acc []]
    (if (zero? sub)
      acc
      (recur (bit-and (dec sub) mask) (conj acc sub)))))
