(ns clojure-algorithms.sorts.circle-sort
  "Circle sort. Ported from TheAlgorithms/Python.")

(defn- circle-sort-util
  "One circle-sort pass over indices [low, high] of vector `v`.
  Returns [v swapped?]."
  [v low high]
  (if (= low high)
    [v false]
    (let [[a l r sw]
          (loop [arr v l low r high s false]
            (if (>= l r)
              [arr l r s]
              (if (neg? (compare (nth arr r) (nth arr l)))
                (recur (assoc arr l (nth arr r) r (nth arr l))
                       (inc l) (dec r) true)
                (recur arr (inc l) (dec r) s))))
          [a2 sw2]
          (if (and (= l r)
                   (< (inc r) (count a))
                   (neg? (compare (nth a (inc r)) (nth a l))))
            [(assoc a l (nth a (inc r)) (inc r) (nth a l)) true]
            [a sw])
          mid (+ low (quot (- high low) 2))
          [a3 s-left] (circle-sort-util a2 low mid)
          [a4 s-right] (circle-sort-util a3 (inc mid) high)]
      [a4 (or sw2 s-left s-right)])))

(defn circle-sort
  "Sorts `coll` with circle sort. Returns a sorted vector."
  [coll]
  (let [v (vec coll)]
    (if (< (count v) 2)
      v
      (loop [a v]
        (let [[a2 swapped?] (circle-sort-util a 0 (dec (count a)))]
          (if swapped? (recur a2) a2))))))
