(ns clojure-algorithms.searches.sentinel-linear-search
  "Sentinel linear search.

  Port of TheAlgorithms/Python searches/sentinel_linear_search.py. The
  sentinel is appended to a copy, so the input is never mutated.")

(defn sentinel-linear-search
  "Index of `target` in `sequence`, or nil if absent."
  [sequence target]
  (let [v (vec sequence)
        guarded (conj v target)]
    (loop [i 0]
      (if (= (nth guarded i) target)
        (when (< i (count v)) i)
        (recur (inc i))))))
