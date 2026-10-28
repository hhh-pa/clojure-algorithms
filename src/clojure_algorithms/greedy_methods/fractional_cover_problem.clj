(ns clojure-algorithms.greedy-methods.fractional-cover-problem
  "Fractional Cover Problem (continuous knapsack): fill capacity with
  fractions of items in descending value-to-weight ratio order.
  See https://en.wikipedia.org/wiki/Set_cover_problem")

(defrecord Item [weight value])

(defn ratio
  "Returns the value-to-weight ratio of item."
  [item]
  (/ (double (:value item)) (:weight item)))

(defn fractional-cover
  "Returns the maximum value obtainable with capacity from items,
  taking fractions greedily by ratio. Throws on negative capacity."
  [items capacity]
  (when (neg? capacity)
    (throw (ex-info "Capacity cannot be negative" {:capacity capacity})))
  (loop [remaining capacity
         total 0.0
         [item & rest-items] (sort-by ratio > items)]
    (if (or (nil? item) (zero? remaining))
      total
      (let [taken (min (:weight item) remaining)]
        (recur (- remaining taken)
               (+ total (* taken (ratio item)))
               rest-items)))))
