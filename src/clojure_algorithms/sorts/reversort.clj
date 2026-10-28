(ns clojure-algorithms.sorts.reversort
  "Reversort (Google Code Jam 2021) plus its cost function.
  Ported from TheAlgorithms/Python.")

(defn- min-index-from
  [a i n]
  (reduce (fn [mi j]
            (if (neg? (compare (nth a j) (nth a mi))) j mi))
          i (range (inc i) n)))

(defn- reverse-range
  [v i j]
  (into (into (subvec v 0 i) (rseq (subvec v i (inc j))))
        (subvec v (inc j))))

(defn reversort
  "Sorts `coll` with the Reversort algorithm. Returns a sorted vector."
  [coll]
  (let [v (vec coll)
        n (count v)]
    (loop [a v i 0]
      (if (>= i (dec n))
        a
        (let [m (min-index-from a i n)]
          (recur (if (= m i) a (reverse-range a i m)) (inc i)))))))

(defn reversort-cost
  "Total Reversort cost: sum of the lengths of the reversed segments."
  [coll]
  (let [v (vec coll)
        n (count v)]
    (loop [a v i 0 cost 0]
      (if (>= i (dec n))
        cost
        (let [m (min-index-from a i n)]
          (recur (reverse-range a i m) (inc i) (+ cost (- m i) 1)))))))
