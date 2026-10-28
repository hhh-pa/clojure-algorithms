(ns clojure-algorithms.dynamic-programming.narcissistic-number
  "All narcissistic (Armstrong) numbers below `limit`, with memoized
  digit powers.
  Ported from TheAlgorithms/Python dynamic_programming/narcissistic_number.py.")

(defn find-narcissistic-numbers
  "Sorted vector of narcissistic numbers below `limit`."
  [limit]
  (if (<= limit 0)
    []
    (let [cache (atom {})]
      (letfn [(digit-power [digit power]
                (if (contains? @cache [power digit])
                  (@cache [power digit])
                  (let [p (long (Math/pow digit power))]
                    (swap! cache assoc [power digit] p)
                    p)))
              (narcissistic? [number]
                (let [digits (map #(Character/digit ^char % 10)
                                  (str number))
                      power (count digits)]
                  (= number (reduce + (map #(digit-power % power) digits)))))]
        (filterv narcissistic? (range limit))))))
