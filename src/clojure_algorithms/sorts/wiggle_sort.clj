(ns clojure-algorithms.sorts.wiggle-sort
  "Wiggle sort: reorders so v[0] <= v[1] >= v[2] <= v[3] ...
  Ported from TheAlgorithms/Python.")

(defn wiggle-sort
  "Wiggle-sorts `coll` in one pass. Returns a vector."
  [coll]
  (let [v (vec coll)]
    (loop [a v i 1]
      (if (>= i (count a))
        a
        (let [prev (nth a (dec i))
              cur (nth a i)
              c (compare prev cur)]
          (recur (if (or (and (odd? i) (pos? c))
                         (and (even? i) (neg? c)))
                   (assoc a (dec i) cur i prev)
                   a)
                 (inc i)))))))

(defn wiggle?
  "True when `v` satisfies the wiggle property."
  [v]
  (every? (fn [[i [a b]]]
            (if (odd? (inc i))
              (not (pos? (compare a b)))
              (not (neg? (compare a b)))))
          (map-indexed vector (partition 2 1 v))))
