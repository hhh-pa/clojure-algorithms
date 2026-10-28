(ns clojure-algorithms.dynamic-programming.sum-of-subset
  "Whether any subset of `arr` sums to `required-sum`.
  Ported from TheAlgorithms/Python dynamic_programming/sum_of_subset.py.")

(defn sum-subset?
  "True when some subset of `arr` sums to `required-sum`."
  [arr required-sum]
  (let [a (vec arr)
        n (count a)
        subset (atom (vec (repeat (inc n) (vec (repeat (inc required-sum)
                                                       false)))))]
    (doseq [i (range (inc n))]
      (swap! subset assoc-in [i 0] true))
    (doseq [i (range 1 (inc n))
            j (range 1 (inc required-sum))]
      (swap! subset assoc-in [i j]
             (if (> (a (dec i)) j)
               (get-in @subset [(dec i) j])
               (or (get-in @subset [(dec i) j])
                   (get-in @subset [(dec i) (- j (a (dec i)))])))))
    (get-in @subset [n required-sum])))
