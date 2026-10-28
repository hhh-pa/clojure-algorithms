(ns clojure-algorithms.backtracking.all-combinations
  "All combinations of k numbers out of 1..n via backtracking.
  Time complexity: O(C(n, k)).")

(defn create-all-state
  "Backtracking helper: all ways to extend `current` with `level` more
  numbers chosen from `increment`..`total-number`. Returns a vector of combos."
  [increment total-number level current]
  (if (zero? level)
    [current]
    (vec (mapcat (fn [i]
                   (create-all-state (inc i) total-number (dec level) (conj current i)))
                 (range increment (+ (- total-number level) 2))))))

(defn combination-lists
  "Returns all k-combinations of 1..n in lexicographic order."
  [n k]
  (create-all-state 1 n k []))

(defn generate-all-combinations
  "Returns all k-combinations of 1..n using backtracking.
  `n` and `k` must be non-negative."
  [n k]
  (when (neg? k)
    (throw (ex-info "k must not be negative" {})))
  (when (neg? n)
    (throw (ex-info "n must not be negative" {})))
  (create-all-state 1 n k []))
