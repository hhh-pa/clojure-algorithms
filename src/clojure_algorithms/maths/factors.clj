(ns clojure-algorithms.maths.factors
  "Port of maths/factors.py")

(defn factors-of-a-number
  "Return the sorted factors of num as a vector.
  Returns an empty vector for numbers less than 1."
  [num]
  (if (< num 1)
    []
    (let [root (long (Math/sqrt (double num)))]
      (vec (sort (set (concat [1 num]
                              (mapcat (fn [i]
                                        (when (zero? (rem num i))
                                          [i (quot num i)]))
                                      (range 2 (inc root))))))))))
