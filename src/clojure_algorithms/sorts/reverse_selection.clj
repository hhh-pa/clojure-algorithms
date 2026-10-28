(ns clojure-algorithms.sorts.reverse-selection
  "Reverse selection sort: brings each minimum to the front by reversing
  the subarray in between. Ported from TheAlgorithms/Python.")

(defn reverse-subarray
  "Reverses the inclusive range [start, end] of vector `v`.
  Returns the updated vector."
  [v start end]
  (loop [a (vec v) s start e end]
    (if (>= s e)
      a
      (recur (assoc a s (nth a e) e (nth a s)) (inc s) (dec e)))))

(defn reverse-selection-sort
  "Sorts `coll` with reverse selection sort. Returns a sorted vector."
  [coll]
  (let [v (vec coll)
        n (count v)]
    (loop [a v i 0]
      (if (>= i (dec n))
        a
        (let [min-idx (reduce (fn [mi j]
                                (if (neg? (compare (nth a j) (nth a mi)))
                                  j mi))
                              i (range (inc i) n))]
          (recur (if (= min-idx i) a (reverse-subarray a i min-idx))
                 (inc i)))))))
