(ns clojure-algorithms.searches.ternary-search
  "Ternary search over sorted collections.

  Port of TheAlgorithms/Python searches/ternary_search.py. Ranges
  narrower than `precision` fall back to `lin-search`. All functions
  return the item index or -1.")

(def precision
  "Range width below which the search degrades to linear scan."
  10)

(defn lin-search
  "Index of `target` in `array` within [left, right), or -1."
  [left right array target]
  (let [v (vec array)]
    (or (first (filter #(= (nth v %) target) (range left right))) -1)))

(defn ite-ternary-search
  "Iterative ternary search for `target` in `array`."
  [array target]
  (let [v (vec array)]
    (loop [left 0 right (count v)]
      (if (> left right)
        -1
        (if (< (- right left) precision)
          (lin-search left right v target)
          (let [one-third (+ left (quot (- right left) 3))
                two-third (- right (quot (- right left) 3))]
            (cond (= (nth v one-third) target) one-third
                  (= (nth v two-third) target) two-third
                  (neg? (compare target (nth v one-third)))
                  (recur left one-third)
                  (neg? (compare (nth v two-third) target))
                  (recur (inc two-third) right)
                  :else (recur (inc one-third) two-third))))))))

(defn rec-ternary-search
  "Recursive ternary search for `target` in `array` within [left, right)."
  [left right array target]
  (let [v (vec array)]
    (if (< left right)
      (if (< (- right left) precision)
        (lin-search left right v target)
        (let [one-third (+ left (quot (- right left) 3))
              two-third (- right (quot (- right left) 3))]
          (cond (= (nth v one-third) target) one-third
                (= (nth v two-third) target) two-third
                (neg? (compare target (nth v one-third)))
                (recur left one-third v target)
                (neg? (compare (nth v two-third) target))
                (recur (inc two-third) right v target)
                :else (recur (inc one-third) two-third v target))))
      -1)))
