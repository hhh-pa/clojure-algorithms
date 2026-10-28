(ns clojure-algorithms.searches.double-linear-search
  "Linear search scanning from both ends at once.

  Port of TheAlgorithms/Python searches/double_linear_search.py.")

(defn double-linear-search
  "Index of `search-item` in `array` scanning inward from both ends,
  or -1 if absent."
  [array search-item]
  (let [v (vec array)]
    (loop [start 0 end (dec (count v))]
      (cond (> start end) -1
            (= (nth v start) search-item) start
            (= (nth v end) search-item) end
            :else (recur (inc start) (dec end))))))
