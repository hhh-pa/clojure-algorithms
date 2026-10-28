(ns clojure-algorithms.dynamic-programming.max-product-subarray
  "Maximum product of any contiguous subarray.
  Ported from TheAlgorithms/Python dynamic_programming/max_product_subarray.py.")

(defn max-product-subarray
  "Maximum subarray product of integer collection `numbers` (0 when empty)."
  [numbers]
  (if (empty? numbers)
    0
    (do
      (when-not (and (sequential? numbers)
                     (every? integer? numbers))
        (throw (IllegalArgumentException.
                "numbers must be an iterable of integers")))
      (let [arr (vec numbers)]
        (loop [i 1 mx (arr 0) mn (arr 0) best (arr 0)]
          (if (>= i (count arr))
            best
            (let [number (arr i)
                  [mx mn] (if (neg? number) [mn mx] [mx mn])
                  mx (max number (* mx number))
                  mn (min number (* mn number))]
              (recur (inc i) mx mn (max best mx)))))))))
