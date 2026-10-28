(ns clojure-algorithms.backtracking.combination-sum
  "Combination Sum: all combinations of distinct candidates (reuse allowed)
  that add up to `target`. Average time complexity: O(n!).")

(defn- backtrack
  "Extends `path` with candidates from `previous-index` onward to reach
  `target`. Returns a vector of completed combinations."
  [candidates path target previous-index]
  (if (zero? target)
    [path]
    (vec (mapcat (fn [index]
                   (if (>= target (nth candidates index))
                     (backtrack candidates
                                (conj path (nth candidates index))
                                (- target (nth candidates index))
                                index)
                     []))
                 (range previous-index (count candidates))))))

(defn combination-sum
  "Returns all combinations of `candidates` summing to `target`.
  The list must be non-empty with non-negative elements."
  [candidates target]
  (when (empty? candidates)
    (throw (ex-info "Candidates list should not be empty" {})))
  (when (some neg? candidates)
    (throw (ex-info "All elements in candidates must be non-negative" {})))
  (backtrack (vec candidates) [] target 0))
