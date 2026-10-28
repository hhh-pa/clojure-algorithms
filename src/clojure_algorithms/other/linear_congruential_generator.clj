(ns clojure-algorithms.other.linear-congruential-generator
  "Linear congruential pseudorandom number generator.

  Port of TheAlgorithms/Python `other/linear_congruential_generator.py`.")

(defn make-lcg
  "Create an LCG state map. Defaults mirror the common Numerical Recipes
  parameters when called with no args."
  ([]
   (make-lcg 1664525 1013904223 (bit-shift-left 2 31)
             (System/currentTimeMillis)))
  ([multiplier increment modulo seed]
   {:multiplier multiplier :increment increment
    :modulo modulo :seed seed}))

(defn next-number
  "Advance the generator; return `[updated-state value]`."
  [{:keys [multiplier increment modulo seed] :as lcg}]
  (let [s (mod (+ (* multiplier seed) increment) modulo)]
    [(assoc lcg :seed s) s]))

(defn lcg-seq
  "Infinite lazy seq of pseudorandom values from `lcg`."
  [lcg]
  (lazy-seq
   (let [[lcg2 v] (next-number lcg)]
     (cons v (lcg-seq lcg2)))))
