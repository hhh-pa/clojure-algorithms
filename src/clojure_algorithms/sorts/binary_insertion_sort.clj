(ns clojure-algorithms.sorts.binary-insertion-sort
  "Binary insertion sort: insertion sort that locates the insertion point
  with binary search. Ported from TheAlgorithms/Python.")

(defn- binary-search-pos
  "Index at which `x` should be inserted into sorted vector `v`
  considering the range [0, i)."
  [v x i]
  (loop [low 0 high (dec i)]
    (if (> low high)
      low
      (let [mid (quot (+ low high) 2)]
        (if (neg? (compare x (nth v mid)))
          (recur low (dec mid))
          (recur (inc mid) high))))))

(defn binary-insertion-sort
  "Sorts `coll` with binary insertion sort. Returns a sorted vector."
  [coll]
  (let [v (vec coll)
        n (count v)]
    (loop [a v i 1]
      (if (>= i n)
        a
        (let [x (nth a i)
              pos (binary-search-pos a x i)]
          (recur (into (conj (subvec a 0 pos) x)
                       cat
                       [(subvec a pos i) (subvec a (inc i))])
                 (inc i)))))))
