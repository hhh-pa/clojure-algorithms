(ns clojure-algorithms.dynamic-programming.tribonacci
  "First n Tribonacci numbers (T(0)=T(1)=0, T(2)=1).
  Ported from TheAlgorithms/Python dynamic_programming/tribonacci.py.")

(defn tribonacci
  "Vector of the first `n` Tribonacci numbers."
  [n]
  (cond
    (<= n 0) []
    (<= n 2) (vec (take n [0 0]))
    :else (loop [i 3 dp [0 0 1]]
            (if (>= i n)
              dp
              (recur (inc i)
                     (conj dp (+ (peek dp)
                                 (nth dp (- (count dp) 2))
                                 (nth dp (- (count dp) 3)))))))))
