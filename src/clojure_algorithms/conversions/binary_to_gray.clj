(ns clojure-algorithms.conversions.binary-to-gray
  "Port of conversions/binary_to_gray.py.
  Converts a binary string to its Gray code string.")

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

(defn binary-to-gray
  "Convert binary string BINARY to its Gray code string of equal length."
  [binary]
  (let [n (Long/parseLong binary 2)
        gray (bit-xor n (bit-shift-right n 1))]
    (to-bin gray (count binary))))
