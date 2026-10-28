(ns clojure-algorithms.maths.base-neg2-conversion
  "Port of maths/base_neg2_conversion.py")

(defn decimal-to-negative-base-2
  "Convert a decimal integer to its negative-base-2 representation,
  returned as an integer whose decimal digits are the base -2 digits."
  [num]
  (if (zero? num)
    0
    (loop [n (bigint num)
           place (bigint 1)
           result (bigint 0)]
      (if (zero? n)
        (if (<= (bigint Long/MIN_VALUE) result (bigint Long/MAX_VALUE))
          (long result)
          result)
        (let [r (mod n 2)]
          (recur (quot (- n r) -2)
                 (*' place 10)
                 (+' result (*' r place))))))))
