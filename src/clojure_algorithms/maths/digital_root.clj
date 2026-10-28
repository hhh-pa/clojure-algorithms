(ns clojure-algorithms.maths.digital-root
  "Digital root: the single digit obtained by repeatedly summing the decimal
  digits of a number until one digit remains.
  See https://en.wikipedia.org/wiki/Digital_root")

(defn- digit-sum
  "Sums the decimal digits of the non-negative integer `n`."
  [n]
  (reduce + (map #(Character/digit ^char % 10) (str n))))

(defn digital-root
  "Returns the digital root of `number`. The input is taken by absolute
  value, so negative numbers behave like their positive counterpart."
  [number]
  (when-not (integer? number)
    (throw (ex-info "digital root only defined for integers" {:type :ValueError})))
  (loop [n (if (neg? number) (- number) number)]
    (if (< n 10)
      n
      (recur (digit-sum n)))))
