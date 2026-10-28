(ns clojure-algorithms.conversions.int-to-negative-binary-base
  "Port of conversions/int_to_negative_binary_base.py.
  Converts a decimal integer to negabinary (base -2) digits.")

(defn decimal-to-negative-binary
  "Convert decimal NUMBER to base -2 digits read back as an integer."
  [number]
  (if (zero? number)
    0
    (loop [n (long number) acc ""]
      (if (zero? n)
        (Long/parseLong acc)
        (let [q (Math/floorDiv n -2)
              r (Math/floorMod n -2)
              [q2 r2] (if (neg? r) [(inc q) (+ r 2)] [q r])]
          (recur q2 (str r2 acc)))))))
