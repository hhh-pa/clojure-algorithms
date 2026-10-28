(ns clojure-algorithms.sorts.radix-sort
  "LSD radix sort (base 10) for non-negative integers.
  Ported from TheAlgorithms/Python.")

(def ^:const radix 10)

(defn radix-sort
  "Sorts non-negative integers `coll` with LSD radix sort.
  Returns a sorted vector. Throws IllegalArgumentException on negatives."
  [coll]
  (let [v (vec coll)]
    (cond
      (empty? v) []
      (some neg? v)
      (throw (IllegalArgumentException.
              "All elements in list_of_ints must be non-negative integers"))
      :else
      (let [max-digit (apply max v)]
        (loop [a v placement 1]
          (if (> placement max-digit)
            a
            (let [buckets (reduce (fn [bs x]
                                    (let [d (int (mod (/ x placement) radix))]
                                      (update bs d conj x)))
                                  (vec (repeat radix [])) a)]
              (recur (vec (mapcat identity buckets)) (* placement radix)))))))))
