(ns clojure-algorithms.other.fischer-yates-shuffle
  "Fisher-Yates (Knuth) shuffle of a collection.

  Port of TheAlgorithms/Python `other/fischer_yates_shuffle.py`.
  Returns a shuffled vector; the input is not mutated.")

(defn fisher-yates-shuffle
  "Return a shuffled vector of `coll`. With no `rand-fn`, uses `rand-int`;
  pass a `(fn [n]) -> int in [0, n)` for deterministic shuffles."
  ([coll] (fisher-yates-shuffle coll rand-int))
  ([coll rand-fn]
   (let [a (vec coll)
         n (count a)]
     (loop [v (transient a) i (dec n)]
       (if (<= i 0)
         (persistent! v)
         (let [j (rand-fn (inc i))
               vi (get v i)
               vj (get v j)]
           (assoc! v i vj)
           (assoc! v j vi)
           (recur v (dec i))))))))
