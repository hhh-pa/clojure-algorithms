(ns clojure-algorithms.sorts.double-sort
  "Double sort: bubble sort applied left-to-right and right-to-left
  in each pass. Ported from TheAlgorithms/Python.")

(defn double-sort
  "Sorts `coll` with double sort. Returns a sorted vector."
  [coll]
  (let [v (vec coll)
        n (count v)
        passes (int (inc (/ (dec n) 2)))]
    (loop [a v p 0]
      (if (>= p passes)
        a
        (recur (loop [b a j 0]
                 (if (>= j (dec n))
                   b
                   (let [b1 (if (neg? (compare (nth b (inc j)) (nth b j)))
                              (assoc b j (nth b (inc j)) (inc j) (nth b j))
                              b)
                         r (- n 1 j)
                         l (- n 2 j)
                         b2 (if (and (>= l 0)
                                     (neg? (compare (nth b1 r) (nth b1 l))))
                              (assoc b1 r (nth b1 l) l (nth b1 r))
                              b1)]
                     (recur b2 (inc j)))))
               (inc p))))))
