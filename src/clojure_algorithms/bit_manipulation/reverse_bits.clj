(ns clojure-algorithms.bit-manipulation.reverse-bits
  "Bit reversal for 32-bit integers.")

(defn get-reverse-bit-string
  "Returns the 32-character reversed bit string of integer `number`."
  [number]
  (when (not (integer? number))
    (throw (ex-info (str "operation can not be conducted on an object of type "
                         (cond (string? number) "str"
                               (float? number) "float"
                               :else "unknown")) {})))
  (apply str (map #(str (mod % 2))
                  (take 32 (iterate #(bit-shift-right % 1) number)))))

(defn reverse-bit
  "Reverses the bits of non-negative integer `number` as a 32-bit value
  and returns the resulting integer."
  [number]
  (when (not (integer? number))
    (throw (ex-info "Input value must be an 'int' type" {})))
  (when (neg? number)
    (throw (ex-info "The value of input must be non-negative" {})))
  (loop [n number, result 0, i 0]
    (if (= i 32)
      result
      (recur (bit-shift-right n 1)
             (bit-or (bit-shift-left result 1) (bit-and n 1))
             (inc i)))))
