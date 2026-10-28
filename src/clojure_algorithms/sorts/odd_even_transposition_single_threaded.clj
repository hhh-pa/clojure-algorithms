(ns clojure-algorithms.sorts.odd-even-transposition-single-threaded
  "Single-threaded odd-even transposition sort.
  Ported from TheAlgorithms/Python.")

(defn odd-even-transposition
  "Sorts `coll` with n alternating even/odd transposition passes.
  Returns a sorted vector."
  [coll]
  (let [v (vec coll)
        n (count v)]
    (loop [a v p 0]
      (if (>= p n)
        a
        (recur (loop [b a i (mod p 2)]
                 (if (>= i (dec n))
                   b
                   (recur (if (neg? (compare (nth b (inc i)) (nth b i)))
                            (assoc b i (nth b (inc i)) (inc i) (nth b i))
                            b)
                          (+ i 2))))
               (inc p))))))
