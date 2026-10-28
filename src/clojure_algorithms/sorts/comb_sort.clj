(ns clojure-algorithms.sorts.comb-sort
  "Comb sort: bubble sort with a shrinking gap (shrink factor 1.3).
  Ported from TheAlgorithms/Python.")

(defn comb-sort
  "Sorts `coll` with comb sort. Returns a sorted vector."
  [coll]
  (let [n (count (vec coll))]
    (loop [a (vec coll) gap n completed (Boolean/valueOf false)]
      (if (and completed (= gap 1))
        a
        (let [gap2 (max (int (/ gap 1.3)) 1)
              [a2 done?] (loop [b a i 0 d (Boolean/valueOf (= gap2 1))]
                           (if (>= (+ i gap2) n)
                             [b d]
                             (if (neg? (compare (nth b (+ i gap2)) (nth b i)))
                               (recur (assoc b i (nth b (+ i gap2))
                                             (+ i gap2) (nth b i))
                                      (inc i) (Boolean/valueOf false))
                               (recur b (inc i) d))))]
          (recur a2 gap2 (Boolean/valueOf (boolean done?))))))))
