(ns clojure-algorithms.searches.linear-search
  "Linear search, iterative and recursive.

  Port of TheAlgorithms/Python searches/linear_search.py.")

(defn linear-search
  "Index of the first occurrence of `target` in `sequence`, or -1."
  [sequence target]
  (or (first (keep-indexed #(when (= %2 target) %1) sequence)) -1))

(defn rec-linear-search
  "Index of `target` in `sequence` between `low` and `high` (inclusive),
  narrowing from both ends; -1 if absent. Throws on invalid bounds."
  [sequence low high target]
  (let [v (vec sequence)]
    (when (not (and (<= 0 high (dec (count v))) (<= 0 low (dec (count v)))))
      (throw (ex-info "Invalid upper or lower bound!"
                      {:low low :high high})))
    (cond (< high low) -1
          (= (nth v low) target) low
          (= (nth v high) target) high
          :else (recur v (inc low) (dec high) target))))
