(ns clojure-algorithms.maths.special-numbers.happy-number
  "A happy number is a number which eventually reaches 1 when replaced by the
sum of the square of each digit.")

(defn- value-repr
  "Python-repr-style rendering used in error messages: strings are wrapped in
single quotes, other values use their plain string form."
  [v]
  (if (string? v) (str "'" v "'") (str v)))

(defn is-happy-number
  "Returns true if `number` is happy: it eventually reaches 1 when repeatedly
replaced by the sum of the squares of its digits. Throws an ex-info with
:type :ValueError when `number` is not a positive integer."
  [number]
  (when (or (not (integer? number)) (<= number 0))
    (throw (ex-info (str "number=" (value-repr number)
                         " must be a positive integer")
                    {:type :ValueError})))
  (letfn [(digit-square-sum [n]
            (reduce + 0 (map #(let [d (Character/digit ^char % 10)] (* d d))
                             (str n))))]
    (loop [n number, seen #{}]
      (cond
        (= n 1) true
        (contains? seen n) false
        :else (recur (digit-square-sum n) (conj seen n))))))
