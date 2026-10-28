(ns clojure-algorithms.maths.integer-square-root
  "Integer Square Root Algorithm -- An efficient method to calculate the square root of a
  non-negative integer 'num' rounded down to the nearest integer. It uses a binary search
  approach to find the integer square root without using any built-in exponent functions
  or operators.
  * https://en.wikipedia.org/wiki/Integer_square_root
  Note:
      - This algorithm is designed for non-negative integers only.
      - The result is rounded down to the nearest integer.
      - The algorithm has a time complexity of O(log(x)).")

(defn integer-square-root
  "Returns the integer square root of the non-negative integer `num`,
  rounded down."
  [num]
  (when (or (not (integer? num)) (neg? num))
    (throw (ex-info "num must be non-negative integer" {:type :ValueError})))
  (if (< num 2)
    num
    (loop [lo 0, hi (quot num 2)]
      (if (> lo hi)
        hi
        (let [mid (+ lo (quot (- hi lo) 2))
              sq (* mid mid)]
          (cond
            (== sq num) mid
            (< sq num) (recur (inc mid) hi)
            :else (recur lo (dec mid))))))))
