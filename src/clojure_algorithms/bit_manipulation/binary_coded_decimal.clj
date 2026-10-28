(ns clojure-algorithms.bit-manipulation.binary-coded-decimal
  "Binary coded decimal (BCD): each base-10 digit is represented by a
  4-bit binary nibble.")

(defn binary-coded-decimal
  "Returns the \"0b\"-prefixed BCD string of integer `number`.
  Negative inputs are clamped to zero."
  [number]
  (str "0b" (apply str
                   (map (fn [digit]
                          (let [s (Integer/toBinaryString (Character/digit ^char digit 10))]
                            (str (apply str (repeat (- 4 (count s)) "0")) s)))
                        (str (max 0 number))))))
