(ns clojure-algorithms.dynamic-programming.minimum-squares-to-represent-a-number
  "Fewest perfect squares summing to `number` (Lagrange's theorem DP).
  Ported from TheAlgorithms/Python
  dynamic_programming/minimum_squares_to_represent_a_number.py.")

(defn minimum-squares-to-represent-a-number
  "Minimum count of squares summing to natural number `number`."
  [number]
  (when-not (integer? number)
    (throw (IllegalArgumentException.
            "the value of input must be a natural number")))
  (when (neg? number)
    (throw (IllegalArgumentException.
            "the value of input must not be a negative number")))
  (if (zero? number)
    1
    (let [answers (atom (assoc (vec (repeat (inc number) -1)) 0 0))]
      (doseq [i (range 1 (inc number))]
        (let [root (int (Math/sqrt i))]
          (swap! answers assoc i
                 (reduce min (for [j (range 1 (inc root))]
                               (inc (@answers (- i (* j j)))))))))
      (@answers number))))
