(ns clojure-algorithms.divide-and-conquer.kth-order-statistic
  "kth smallest element in expected linear time (quickselect).

  A random pivot partitions the input; recursion continues on the side
  containing the kth element.
  Ported from TheAlgorithms/Python divide_and_conquer/kth_order_statistic.py.")

(defn random-pivot
  "Choose a random pivot element from `coll`."
  [coll]
  (rand-nth (vec coll)))

(defn kth-number
  "Return the kth smallest number in `coll` (1-based `k`)."
  [coll k]
  (let [pivot (random-pivot coll)
        small (filterv #(< % pivot) coll)
        big (filterv #(> % pivot) coll)]
    (cond
      (= (count small) (dec k)) pivot
      (< (count small) (dec k)) (kth-number big (- k (count small) 1))
      :else (kth-number small k))))
