(ns clojure-algorithms.sorts.shrink-shell-sort
  "Shell sort with a shrinking gap (factor 1.3).
  Ported from TheAlgorithms/Python shrink_shell_sort.py.")

(defn shell-sort
  "Sorts `coll` with shrink-gap shell sort. Returns a sorted vector."
  [coll]
  (let [v (vec coll)]
    (loop [a v gap (count v)]
      (if (<= gap 1)
        a
        (let [g (int (/ gap 1.3))
              g2 (if (zero? g) 1 g)
              a2 (loop [b a i g2]
                   (if (>= i (count b))
                     b
                     (let [t (nth b i)
                           [b2 j] (loop [c b j i]
                                    (if (and (>= j g2)
                                             (pos? (compare (nth c (- j g2))
                                                            t)))
                                      (recur (assoc c j (nth c (- j g2)))
                                             (- j g2))
                                      [c j]))]
                       (recur (assoc b2 j t) (inc i)))))]
          (recur a2 g2))))))
