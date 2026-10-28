(ns clojure-algorithms.dynamic-programming.minimum-partition
  "Minimum possible difference between sums of two subsets.
  Ported from TheAlgorithms/Python dynamic_programming/minimum_partition.py.")

(defn find-min
  "Minimum subset-sum difference for non-negative integers `numbers`."
  [numbers]
  (let [nums (vec numbers)
        n (count nums)
        s (reduce + 0 nums)]
    (when (or (neg? s) (some neg? nums))
      (throw (IndexOutOfBoundsException.
              "minimum partition needs non-negative numbers")))
    (let [dp (atom (vec (repeat (inc n) (vec (repeat (inc s) false)))))]
      (doseq [i (range (inc n))]
        (swap! dp assoc-in [i 0] true))
      (doseq [i (range 1 (inc n))
              j (range 1 (inc s))]
        (swap! dp assoc-in [i j]
               (let [without (get-in @dp [(dec i) j])]
                 (if (<= (nums (dec i)) j)
                   (or without (get-in @dp [(dec i) (- j (nums (dec i)))]))
                   without))))
      (let [j (first (filter #(get-in @dp [n %]) (range (quot s 2) -1 -1)))]
        (- s (* 2 j))))))
