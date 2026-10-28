(ns clojure-algorithms.dynamic-programming.integer-partition
  "Partition function p(m): ways to write m as a sum of positive integers.
  Ported from TheAlgorithms/Python dynamic_programming/integer_partition.py.")

(defn partition-count
  "Number of integer partitions of positive integer `m`."
  [m]
  (when-not (integer? m)
    (throw (IllegalArgumentException. "partition-count takes an integer")))
  (when (<= m 0)
    (throw (IndexOutOfBoundsException. "partition-count needs m >= 1")))
  (let [memo (atom (vec (repeat (inc m) (vec (repeat m 0)))))]
    (doseq [i (range (inc m))]
      (swap! memo assoc-in [i 0] 1))
    (doseq [n (range (inc m))
            k (range 1 m)]
      (swap! memo update-in [n k] + (get-in @memo [n (dec k)]))
      (when (pos? (- n k))
        (swap! memo update-in [n k] + (get-in @memo [(- n k 1) k]))))
    (get-in @memo [m (dec m)])))
