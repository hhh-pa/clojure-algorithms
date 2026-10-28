(ns clojure-algorithms.sorts.slowsort
  "Slowsort: multiply and surrender (deliberately pessimal).
  Ported from TheAlgorithms/Python.")

(defn slowsort
  "Sorts the inclusive range [start, end] of `coll` with slowsort.
  Defaults cover the whole collection. Returns a vector."
  ([coll] (slowsort coll 0 (dec (count coll))))
  ([coll start end]
   (let [v (vec coll)
         s (or start 0)
         e (if (nil? end) (dec (count v)) end)]
     (if (>= s e)
       v
       (let [mid (quot (+ s e) 2)
             a (-> v
                   (slowsort s mid)
                   (slowsort (inc mid) e))
             b (if (neg? (compare (nth a e) (nth a mid)))
                 (assoc a e (nth a mid) mid (nth a e))
                 a)]
         (slowsort b s (dec e)))))))
