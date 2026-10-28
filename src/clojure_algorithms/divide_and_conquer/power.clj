(ns clojure-algorithms.divide-and-conquer.power
  "Integer exponentiation by squaring (divide and conquer).
  Ported from TheAlgorithms/Python divide_and_conquer/power.py.")

(defn actual-power
  "Return a^b for integer base `a` and non-negative integer `b`."
  [a b]
  (if (zero? b)
    1
    (let [half (actual-power a (quot b 2))]
      (if (even? b)
        (* half half)
        (* a half half)))))

(defn power
  "Return a^b, supporting negative exponents (returned as a ratio/double)."
  [a b]
  (if (neg? b)
    (/ 1 (actual-power a (- b)))
    (actual-power a b)))
