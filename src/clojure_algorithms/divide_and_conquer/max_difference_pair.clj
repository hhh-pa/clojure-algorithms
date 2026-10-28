(ns clojure-algorithms.divide-and-conquer.max-difference-pair
  "Largest A[j] - A[i] with i <= j, via divide and conquer.

  Returns the [small big] pair of values achieving the maximum difference.
  Ported from TheAlgorithms/Python divide_and_conquer/max_difference_pair.py.")

(defn max-difference
  "Return [small big] maximizing big - small with small at-or-before big."
  [coll]
  (let [a (vec coll)]
    (if (= (count a) 1)
      [(a 0) (a 0)]
      (let [mid (quot (count a) 2)
            first-half (subvec a 0 mid)
            second-half (subvec a mid)
            [small1 big1] (max-difference first-half)
            [small2 big2] (max-difference second-half)
            min-first (apply min first-half)
            max-second (apply max second-half)]
        (cond
          (and (> (- big2 small2) (- max-second min-first))
               (> (- big2 small2) (- big1 small1)))
          [small2 big2]
          (> (- big1 small1) (- max-second min-first)) [small1 big1]
          :else [min-first max-second])))))
