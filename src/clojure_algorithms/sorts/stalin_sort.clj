(ns clojure-algorithms.sorts.stalin-sort
  "Stalin sort: elements out of order are eliminated.
  Ported from TheAlgorithms/Python.")

(defn stalin-sort
  "Keeps the longest non-decreasing prefix subsequence of `coll`.
  Returns a vector."
  [coll]
  (let [v (vec coll)]
    (if (empty? v)
      []
      (reduce (fn [acc x]
                (if (not (neg? (compare x (peek acc))))
                  (conj acc x)
                  acc))
              [(first v)] (rest v)))))
