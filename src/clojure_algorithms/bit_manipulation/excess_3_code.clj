(ns clojure-algorithms.bit-manipulation.excess-3-code
  "Excess-3 code: add 3 to each decimal digit, then encode as BCD.
  See https://en.wikipedia.org/wiki/Excess-3")

(defn excess-3-code
  "Returns the \"0b\"-prefixed excess-3 encoding of integer `number`.
  Negative inputs are clamped to zero."
  [number]
  (str "0b" (apply str
                   (map (fn [digit]
                          (let [s (Integer/toBinaryString
                                   (+ 3 (Character/digit ^char digit 10)))]
                            (str (apply str (repeat (- 4 (count s)) "0")) s)))
                        (str (max 0 number))))))
