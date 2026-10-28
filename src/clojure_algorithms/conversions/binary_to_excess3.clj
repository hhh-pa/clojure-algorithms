(ns clojure-algorithms.conversions.binary-to-excess3
  "Port of conversions/binary_to_excess3.py.
  Converts a binary number to its Excess-3 code.")

(defn- to-bin
  "Binary digit string of non-negative integer N, left-padded to MIN-WIDTH."
  [n min-width]
  (let [digits (if (zero? n) "0"
                   (loop [x (bigint n) acc ""]
                     (if (zero? x) acc
                         (recur (quot x 2) (str (rem x 2) acc)))))]
    (if (< (count digits) min-width)
      (str (apply str (repeat (- min-width (count digits)) \0)) digits)
      digits)))

(defn binary-to-excess3
  "Convert binary string BINARY-STR to its Excess-3 code (decimal + 3)."
  [binary-str]
  (to-bin (+ (Long/parseLong binary-str 2) 3) 4))
