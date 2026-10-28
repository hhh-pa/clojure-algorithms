(ns clojure-algorithms.other.greedy
  "Greedy knapsack-style selection over valued/weighted items.

  Port of TheAlgorithms/Python `other/greedy.py`.
  Items are plain maps with :name, :value, and :weight keys.")

(defn make-item
  "Create an item map with `name`, `value`, and `weight`."
  [name value weight]
  {:name name :value value :weight weight})

(defn value-weight
  "Value-per-weight ratio of `item`."
  [item]
  (/ (:value item) (:weight item)))

(defn build-menu
  "Build a vector of items from parallel `names`, `values`, `weights`."
  [names values weights]
  (mapv make-item names values weights))

(defn greedy
  "Greedily take items sorted by `key-fn` (descending) while they fit in
  `max-cost`. Returns `[taken-items total-value]`."
  [items max-cost key-fn]
  (let [ordered (sort-by key-fn #(compare %2 %1) items)]
    (loop [remaining ordered
           total-cost 0
           total-value 0.0
           result []]
      (if (empty? remaining)
        [result total-value]
        (let [item (first remaining)]
          (if (<= (+ total-cost (:weight item)) max-cost)
            (recur (rest remaining)
                   (+ total-cost (:weight item))
                   (+ total-value (:value item))
                   (conj result item))
            (recur (rest remaining) total-cost total-value result)))))))
