(ns clojure-algorithms.dynamic-programming.wildcard-matching
  "Wildcard matching with `?` (one char) and `*` (any sequence).
  Ported from TheAlgorithms/Python dynamic_programming/wildcard_matching.py.")

(defn wildcard-match?
  "True when `s` fully matches `pattern`."
  [s pattern]
  (let [m (count s)
        n (count pattern)
        dp (atom (vec (repeat (inc m) (vec (repeat (inc n) false)))))]
    (swap! dp assoc-in [0 0] true)
    (doseq [[j ch] (map-indexed vector pattern)]
      (when (= ch \*)
        (swap! dp assoc-in [0 (inc j)] (get-in @dp [0 j]))))
    (doseq [[i s-ch] (map-indexed vector s)
            [j p-ch] (map-indexed vector pattern)]
      (swap! dp assoc-in [(inc i) (inc j)]
             (cond
               (contains? #{s-ch \?} p-ch)
               (get-in @dp [i j])

               (= \* p-ch)
               (or (get-in @dp [i (inc j)]) (get-in @dp [(inc i) j]))

               :else false)))
    (get-in @dp [m n])))
