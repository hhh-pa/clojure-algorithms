(ns clojure-algorithms.conversions.decimal-to-octal
  "Port of conversions/decimal_to_octal.py.
  Converts a decimal integer to a \"0o\"-prefixed octal string.")

(defn decimal-to-octal
  "Convert non-negative integer NUM to a \"0o\"-prefixed octal string."
  [num]
  (loop [n num octal 0 counter 0]
    (if (pos? n)
      (recur (quot n 8)
             (+ octal (* (rem n 8) (long (Math/pow 10 counter))))
             (inc counter))
      (str "0o" octal))))
