(ns clojure-algorithms.conversions.negative-binary-base-to-int
  "Port of conversions/negative_binary_base_to_int.py.
  Converts negabinary (base -2) digits to a decimal integer.")

(defn negative-binary-to-int
  "Convert negabinary NEGATIVE-BINARY (digits) to a decimal integer."
  [negative-binary]
  (let [digits (reverse (str negative-binary))]
    (reduce + (map-indexed (fn [i c]
                             (* (- (int c) (int \0))
                                (long (Math/pow -2 i))))
                           digits))))
