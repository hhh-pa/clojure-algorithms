(ns clojure-algorithms.bit-manipulation.binary-twos-complement
  "Two's complement representation of negative integers.
  See https://en.wikipedia.org/wiki/Two%27s_complement")

(defn twos-complement
  "Returns the \"0b\"-prefixed two's complement representation of negative
  integer `number`. Zero yields \"0b0\"; positives raise."
  [number]
  (when (pos? number)
    (throw (ex-info "input must be a negative integer" {})))
  (if (zero? number)
    "0b0"
    (let [magnitude (Math/abs (long number))
          n (count (Integer/toBinaryString magnitude))
          diff (- magnitude (bit-shift-left 1 n))
          raw (if (zero? diff) "" (Integer/toBinaryString (- diff)))]
      (str "0b1" (apply str (repeat (- n (count raw)) "0")) raw))))
