(ns clojure-algorithms.maths.disarium
  "Disarium numbers.
https://rosettacode.org/wiki/Disarium_numbers")

(defn- ipow
  "Integer power: b^e computed exactly."
  [b e]
  (reduce *' 1 (repeat e b)))

(defn is-disarium
  "Check if a number is Disarium: the sum of its digits raised to their
1-based positions equals the number itself."
  [num]
  (let [s (str num)
        digits (map #(Character/digit ^char % 10) s)]
    (when (some neg? digits)
      (throw (ex-info (str "invalid literal for int() with base 10: '" s "'")
                      {:type :ValueError})))
    (= num (reduce +' 0 (map-indexed (fn [i d] (ipow d (inc i))) digits)))))
